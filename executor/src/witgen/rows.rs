use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
};

use ast::analyzed::{Expression, PolyID, PolynomialReference};
use itertools::Itertools;
use number::{DegreeType, FieldElement};
use parser_util::lines::indent;

use crate::witgen::Constraint;

use super::{
    affine_expression::{AffineExpression, AffineResult},
    expression_evaluator::ExpressionEvaluator,
    global_constraints::RangeConstraintSet,
    range_constraints::RangeConstraint,
    symbolic_witness_evaluator::{SymoblicWitnessEvaluator, WitnessColumnEvaluator},
    EvalValue, FixedData,
};

/// A single cell, holding an optional value and range constraint.
#[derive(Clone)]
pub struct Cell<T: FieldElement> {
    /// The column name, for debugging purposes.
    pub name: &'static str,
    /// The value of the cell, if known.
    pub value: Option<T>,
    /// Range constraints of the cell, if any.
    pub range_constraint: Option<RangeConstraint<T>>,
}

/// A row of cells, indexed by polynomial ID.
#[derive(Clone)]
pub struct Row<T: FieldElement> {
    pub cells: BTreeMap<PolyID, Cell<T>>,
}

impl<T: FieldElement> Debug for Row<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.render("Row:", true))
    }
}

impl<T: FieldElement> Row<T> {
    fn get_cell_mut(&mut self, poly_id: PolyID) -> &mut Cell<T> {
        self.cells
            .get_mut(&poly_id)
            .unwrap_or_else(|| panic!("Unknown poly ID: {:?}", poly_id))
    }

    pub fn get_cell(&self, poly_id: PolyID) -> &Cell<T> {
        self.cells
            .get(&poly_id)
            .unwrap_or_else(|| panic!("Unknown poly ID: {:?}", poly_id))
    }

    fn has_column(&self, poly_id: PolyID) -> bool {
        self.cells.contains_key(&poly_id)
    }

    /// Builds a string representing the current row
    pub fn render(&self, title: &str, include_unknown: bool) -> String {
        format!("{}:\n{}", title, self.render_values(include_unknown))
    }

    /// Builds a string listing all values, one by row. Nonzero entries are
    /// first, then zero, then unknown (if `include_unknown == true`).
    pub fn render_values(&self, include_unknown: bool) -> String {
        let mut cells = self
            .cells
            .iter()
            .filter(|(_, cell)| cell.value.is_some() || include_unknown)
            .collect::<Vec<_>>();

        // Nonzero first, then zero, then unknown
        cells.sort_by_key(|(i, cell)| {
            (
                match cell.value {
                    Some(v) if v.is_zero() => 1,
                    Some(_) => 0,
                    None => 2,
                },
                *i,
            )
        });

        let render_cell = |cell: &Cell<T>| match cell.value {
            Some(v) => format!("{} = {}", cell.name, v),
            None => {
                let range_constraint_string = match &cell.range_constraint {
                    Some(range_constraint) => format!("\n  (range constraint: {range_constraint})"),
                    None => "".to_string(),
                };
                format!("{} = <unknown>{range_constraint_string}", cell.name)
            }
        };

        indent(
            &cells
                .into_iter()
                .map(|(_, cell)| render_cell(cell))
                .join("\n"),
            "    ",
        )
    }
}

impl<T: FieldElement> From<Row<T>> for BTreeMap<PolyID, T> {
    /// Builds a map from polynomial ID to value. Unknown values are set to zero.
    fn from(val: Row<T>) -> Self {
        val.cells
            .into_iter()
            .map(|(poly_id, cell)| (poly_id, cell.value.unwrap_or_default()))
            .collect()
    }
}

/// A factory for creating rows.
pub struct RowFactory<T: FieldElement> {
    /// The list of all available polynomials.
    polys: BTreeSet<PolynomialReference>,
    /// Global range constraints.
    global_range_constraints: BTreeMap<PolyID, RangeConstraint<T>>,
    poly_names: BTreeMap<PolyID, &'static str>,
}

impl<T: FieldElement> RowFactory<T> {
    pub fn new(
        polys: BTreeSet<&PolynomialReference>,
        global_range_constraints: BTreeMap<PolyID, RangeConstraint<T>>,
        poly_names: BTreeMap<PolyID, &'static str>,
    ) -> Self {
        let polys = polys.into_iter().cloned().collect();
        Self {
            polys,
            poly_names,
            global_range_constraints,
        }
    }

    /// Create a new row without values and just the global range constraints.
    pub fn fresh_row(&self) -> Row<T> {
        let cells = self
            .polys
            .iter()
            .map(|poly| {
                let range_constraint = self.global_range_constraints.get(&poly.poly_id()).cloned();
                (
                    poly.poly_id(),
                    Cell {
                        name: self.poly_names[&poly.poly_id()],
                        value: None,
                        range_constraint,
                    },
                )
            })
            .collect();
        Row { cells }
    }

    /// Create a new row from known values. Range constraints are left empty.
    pub fn make_from_known_values(&self, values: &BTreeMap<PolyID, T>) -> Row<T> {
        let cells = self
            .polys
            .iter()
            .map(|poly| {
                let value = values.get(&poly.poly_id()).cloned();
                (
                    poly.poly_id(),
                    Cell {
                        name: self.poly_names[&poly.poly_id()],
                        value,
                        range_constraint: None,
                    },
                )
            })
            .collect();
        Row { cells }
    }
}

/// A pair of mutable row references which knows how to apply updates.
pub struct RowUpdater<'row, T: FieldElement> {
    current: &'row mut Row<T>,
    next: &'row mut Row<T>,
    current_row_index: DegreeType,
}

impl<'row, T: FieldElement> RowUpdater<'row, T> {
    pub fn new(
        current: &'row mut Row<T>,
        next: &'row mut Row<T>,
        current_row_index: DegreeType,
    ) -> Self {
        Self {
            current,
            next,
            current_row_index,
        }
    }

    /// Applies the updates to the underlying rows. Returns true if any updates
    /// were applied.
    ///
    /// # Panics
    /// Panics if any updates are redundant, as this indicates a bug that would
    /// potentially cause infinite loops otherwise.
    pub fn apply_updates(
        &mut self,
        updates: &EvalValue<&PolynomialReference, T>,
        source_name: impl Fn() -> String,
    ) -> bool {
        if updates.constraints.is_empty() {
            return false;
        }

        log::trace!("    Updates from: {}", source_name());
        for (poly, c) in &updates.constraints {
            match c {
                Constraint::Assignment(value) => {
                    self.set_value(poly, *value);
                }
                Constraint::RangeConstraint(constraint) => {
                    self.update_range_constraint(poly, constraint);
                }
            }
        }
        true
    }

    fn get_cell_mut<'b>(&'b mut self, poly: &PolynomialReference) -> &'b mut Cell<T> {
        match poly.next {
            false => self.current.get_cell_mut(poly.poly_id()),
            true => self.next.get_cell_mut(poly.poly_id()),
        }
    }

    fn set_value(&mut self, poly: &PolynomialReference, value: T) {
        let row = match poly.next {
            false => self.current_row_index,
            true => self.current_row_index + 1,
        };
        log::trace!("      => {} (Row {row}) = {value}", poly.name);
        let cell = self.get_cell_mut(poly);
        // Note that this is a problem even if the value that was set is the same,
        // because we would return that progress was made when it wasn't.
        assert!(cell.value.is_none(), "Value was already set");
        cell.value = Some(value);
    }

    fn update_range_constraint(
        &mut self,
        poly: &PolynomialReference,
        constraint: &RangeConstraint<T>,
    ) {
        log::trace!("      => Adding range constraint for {poly}: {constraint}");
        let cell = self.get_cell_mut(poly);
        let old = &mut cell.range_constraint;
        let new = match old {
            Some(c) => constraint.conjunction(c),
            None => constraint.clone(),
        };
        if let Some(old) = old {
            assert!(*old != new, "Range constraint was already set");
        }
        log::trace!("         (the conjunction is {})", new);
        *old = Some(new);
    }
}

/// A pair of row references which knows which value / range constraint
/// to return for a given [PolynomialReference].
pub struct RowPair<'row, 'a, T: FieldElement> {
    pub current: &'row Row<T>,
    pub next: &'row Row<T>,
    pub current_row_index: DegreeType,
    fixed_data: &'a FixedData<'a, T>,
    evaluate_unknown_to_zero: bool,
}
impl<'row, 'a, T: FieldElement> RowPair<'row, 'a, T> {
    pub fn new(
        current: &'row Row<T>,
        next: &'row Row<T>,
        current_row_index: DegreeType,
        fixed_data: &'a FixedData<'a, T>,
        evaluate_unknown_to_zero: bool,
    ) -> Self {
        Self {
            current,
            next,
            current_row_index,
            fixed_data,
            evaluate_unknown_to_zero,
        }
    }

    pub fn has_cell(&self, poly: &PolynomialReference) -> bool {
        match poly.next {
            false => self.current.has_column(poly.poly_id()),
            true => self.next.has_column(poly.poly_id()),
        }
    }

    fn get_cell(&self, poly: &PolynomialReference) -> &Cell<T> {
        match poly.next {
            false => self.current.get_cell(poly.poly_id()),
            true => self.next.get_cell(poly.poly_id()),
        }
    }

    pub fn get_value(&self, poly: &PolynomialReference) -> Option<T> {
        self.get_cell(poly).value.or_else(|| {
            if self.evaluate_unknown_to_zero {
                Some(T::zero())
            } else {
                None
            }
        })
    }

    /// Tries to evaluate the expression to an expression affine in the witness polynomials,
    /// taking current values of polynomials into account.
    /// @returns an expression affine in the witness polynomials
    pub fn evaluate<'b>(
        &self,
        expr: &'b Expression<T>,
    ) -> AffineResult<&'b PolynomialReference, T> {
        ExpressionEvaluator::new(SymoblicWitnessEvaluator::new(
            self.fixed_data,
            self.current_row_index,
            self,
        ))
        .evaluate(expr)
    }
}

impl<T: FieldElement> WitnessColumnEvaluator<T> for RowPair<'_, '_, T> {
    fn value<'b>(&self, poly: &'b PolynomialReference) -> AffineResult<&'b PolynomialReference, T> {
        Ok(match self.get_value(poly) {
            Some(v) => v.into(),
            None => AffineExpression::from_variable_id(poly),
        })
    }
}

impl<T: FieldElement> RangeConstraintSet<&PolynomialReference, T> for RowPair<'_, '_, T> {
    fn range_constraint(&self, poly: &PolynomialReference) -> Option<RangeConstraint<T>> {
        self.get_cell(poly).range_constraint.clone()
    }
}
