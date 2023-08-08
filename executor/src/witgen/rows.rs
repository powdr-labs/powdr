use std::fmt::Debug;

use ast::analyzed::{Expression, PolynomialReference, PolynomialType};
use itertools::Itertools;
use number::{DegreeType, FieldElement};

use crate::witgen::Constraint;

use super::{
    affine_expression::{AffineExpression, AffineResult},
    column_map::ColumnMap,
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

impl<T: FieldElement> Debug for Cell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let debug_str = match self.value {
            Some(v) => format!("{} = {}", self.name, v),
            None => {
                let range_constraint_string = match &self.range_constraint {
                    Some(range_constraint) => format!("\n  (range constraint: {range_constraint})"),
                    None => "".to_string(),
                };
                format!("{} = <unknown>{range_constraint_string}", self.name)
            }
        };
        f.write_str(&debug_str)
    }
}

/// A row of cells, indexed by polynomial ID.
pub type Row<T> = ColumnMap<Cell<T>>;

impl<T: FieldElement> Debug for Row<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.render("Row:", true))
    }
}

impl<T: FieldElement> Row<T> {
    /// Builds a string representing the current row
    pub fn render(&self, title: &str, include_unknown: bool) -> String {
        format!("{}:\n{}", title, self.render_values(include_unknown))
    }

    /// Builds a string listing all values, one by row. Nonzero entries are
    /// first, then zero, then unknown (if `include_unknown == true`).
    pub fn render_values(&self, include_unknown: bool) -> String {
        let mut cells = self
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

        cells
            .into_iter()
            .map(|(_, cell)| format!("    {:?}", cell))
            .join("\n")
    }
}

/// A factory for rows, which knows the global range constraints and has pointers to column names.
pub struct RowFactory<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    global_range_constraints: ColumnMap<Option<RangeConstraint<T>>>,
}

impl<'a, T: FieldElement> RowFactory<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        global_range_constraints: ColumnMap<Option<RangeConstraint<T>>>,
    ) -> Self {
        Self {
            fixed_data,
            global_range_constraints,
        }
    }

    pub fn fresh_row(&self) -> Row<T> {
        ColumnMap::from(
            self.global_range_constraints
                .iter()
                .map(|(poly_id, range_constraint)| Cell {
                    name: self.fixed_data.witness_column_names[&poly_id],
                    value: None,
                    range_constraint: range_constraint.clone(),
                }),
            PolynomialType::Committed,
        )
    }

    pub fn row_from_known_values(&self, values: &ColumnMap<T>) -> Row<T> {
        ColumnMap::from(
            values.iter().map(|(poly_id, &v)| Cell {
                name: self.fixed_data.witness_column_names[&poly_id],
                value: Some(v),
                range_constraint: None,
            }),
            PolynomialType::Committed,
        )
    }
}

impl<T: FieldElement> From<Row<T>> for ColumnMap<T> {
    /// Builds a map from polynomial ID to value. Unknown values are set to zero.
    fn from(val: Row<T>) -> Self {
        ColumnMap::from(
            val.into_iter()
                .map(|(_, cell)| cell.value.unwrap_or_default()),
            PolynomialType::Committed,
        )
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
            false => &mut self.current[&poly.poly_id()],
            true => &mut self.next[&poly.poly_id()],
        }
    }

    fn row_number(&self, poly: &PolynomialReference) -> DegreeType {
        match poly.next {
            false => self.current_row_index,
            true => self.current_row_index + 1,
        }
    }

    fn set_value(&mut self, poly: &PolynomialReference, value: T) {
        log::trace!(
            "      => {} (Row {}) = {}",
            poly.name,
            self.row_number(poly),
            value
        );
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
        log::trace!(
            "      => Adding range constraint for {} (Row {}): {}",
            poly.name,
            self.row_number(poly),
            constraint
        );
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

    fn get_cell(&self, poly: &PolynomialReference) -> &Cell<T> {
        match poly.next {
            false => &self.current[&poly.poly_id()],
            true => &self.next[&poly.poly_id()],
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
