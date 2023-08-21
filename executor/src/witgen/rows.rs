use std::fmt::Debug;

use ast::analyzed::{Expression, PolyID, PolynomialReference, PolynomialType};
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

#[derive(Clone)]
pub enum CellValue<T: FieldElement> {
    Known(T),
    RangeConstraint(RangeConstraint<T>),
    Unknown,
}

impl<T: FieldElement> CellValue<T> {
    pub fn is_known(&self) -> bool {
        matches!(self, CellValue::Known(_))
    }

    pub fn unwrap_or_default(&self) -> T {
        match self {
            CellValue::Known(v) => *v,
            _ => Default::default(),
        }
    }
}

impl<T: FieldElement> From<&CellValue<T>> for Option<T> {
    fn from(val: &CellValue<T>) -> Self {
        match val {
            CellValue::Known(v) => Some(*v),
            _ => None,
        }
    }
}

/// A single cell, holding an optional value and range constraint.
#[derive(Clone)]
pub struct Cell<'a, T: FieldElement> {
    /// The column name, for debugging purposes.
    pub name: &'a str,
    pub value: CellValue<T>,
}

impl<T: FieldElement> Debug for Cell<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let debug_str = match &self.value {
            CellValue::Known(v) => format!("{} = {}", self.name, v),
            CellValue::RangeConstraint(rc) => {
                format!("{} = <unknown>\n  (range constraint: {})", self.name, rc)
            }
            CellValue::Unknown => format!("{} = <unknown>", self.name),
        };
        f.write_str(&debug_str)
    }
}

/// A row of cells, indexed by polynomial ID.
pub type Row<'a, T> = ColumnMap<Cell<'a, T>>;

impl<T: FieldElement> Debug for Row<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.render("Row", true))
    }
}

impl<T: FieldElement> Row<'_, T> {
    /// Builds a string representing the current row
    pub fn render(&self, title: &str, include_unknown: bool) -> String {
        format!("{}:\n{}", title, self.render_values(include_unknown))
    }

    /// Builds a string listing all values, one by row. Nonzero entries are
    /// first, then zero, then unknown (if `include_unknown == true`).
    pub fn render_values(&self, include_unknown: bool) -> String {
        let mut cells = self
            .iter()
            .filter(|(_, cell)| cell.value.is_known() || include_unknown)
            .collect::<Vec<_>>();

        // Nonzero first, then zero, then unknown
        cells.sort_by_key(|(i, cell)| {
            (
                match cell.value {
                    CellValue::Known(v) if v.is_zero() => 1,
                    CellValue::Known(_) => 0,
                    _ => 2,
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
#[derive(Clone)]
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

    pub fn fresh_row(&self) -> Row<'a, T> {
        ColumnMap::from(
            self.global_range_constraints
                .iter()
                .map(|(poly_id, range_constraint)| Cell {
                    name: self.fixed_data.column_name(&poly_id),
                    value: match range_constraint.as_ref() {
                        Some(rc) => CellValue::RangeConstraint(rc.clone()),
                        None => CellValue::Unknown,
                    },
                }),
            PolynomialType::Committed,
        )
    }

    pub fn row_from_known_values_dense(&self, values: &ColumnMap<T>) -> Row<'a, T> {
        ColumnMap::from(
            values.iter().map(|(poly_id, &v)| Cell {
                name: self.fixed_data.column_name(&poly_id),
                value: CellValue::Known(v),
            }),
            PolynomialType::Committed,
        )
    }

    pub fn row_from_known_values_sparse(
        &self,
        values: impl Iterator<Item = (PolyID, T)>,
    ) -> Row<'a, T> {
        let mut row = self.fresh_row();
        for (poly_id, v) in values {
            row[&poly_id].value = CellValue::Known(v);
        }
        row
    }
}

impl<T: FieldElement> From<Row<'_, T>> for ColumnMap<T> {
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
pub struct RowUpdater<'row, 'a, T: FieldElement> {
    current: &'row mut Row<'a, T>,
    next: &'row mut Row<'a, T>,
    current_row_index: DegreeType,
}

impl<'row, 'a, T: FieldElement> RowUpdater<'row, 'a, T> {
    pub fn new(
        current: &'row mut Row<'a, T>,
        next: &'row mut Row<'a, T>,
        current_row_index: DegreeType,
    ) -> Self {
        Self {
            current,
            next,
            current_row_index,
        }
    }

    pub fn apply_update(&mut self, poly: &PolynomialReference, c: &Constraint<T>) {
        match c {
            Constraint::Assignment(value) => {
                self.set_value(poly, *value);
            }
            Constraint::RangeConstraint(constraint) => {
                self.update_range_constraint(poly, constraint);
            }
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
            self.apply_update(poly, c)
        }
        true
    }

    fn get_cell_mut<'b>(&'b mut self, poly: &PolynomialReference) -> &'b mut Cell<'a, T> {
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
        assert!(!cell.value.is_known(), "Value was already set");
        cell.value = CellValue::Known(value);
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
        let new = match &cell.value {
            CellValue::RangeConstraint(c) => constraint.conjunction(c),
            _ => constraint.clone(),
        };
        if let CellValue::RangeConstraint(old) = &cell.value {
            assert!(*old != new, "Range constraint was already set");
        }
        assert!(
            !cell.value.is_known(),
            "Range constraint was updated but value is already known"
        );
        log::trace!("         (the conjunction is {})", new);
        cell.value = CellValue::RangeConstraint(new);
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum UnknownStrategy {
    /// If a value is unknown, evaluate it to zero.
    Zero,
    /// If a value is unknown, leave it unknown.
    Unknown,
}

/// A pair of row references which knows which value / range constraint
/// to return for a given [PolynomialReference].
pub struct RowPair<'row, 'a, T: FieldElement> {
    pub current: &'row Row<'a, T>,
    pub next: &'row Row<'a, T>,
    pub current_row_index: DegreeType,
    fixed_data: &'a FixedData<'a, T>,
    unknown_strategy: UnknownStrategy,
}
impl<'row, 'a, T: FieldElement> RowPair<'row, 'a, T> {
    pub fn new(
        current: &'row Row<'a, T>,
        next: &'row Row<'a, T>,
        current_row_index: DegreeType,
        fixed_data: &'a FixedData<'a, T>,
        unknown_strategy: UnknownStrategy,
    ) -> Self {
        Self {
            current,
            next,
            current_row_index,
            fixed_data,
            unknown_strategy,
        }
    }

    fn get_cell(&self, poly: &PolynomialReference) -> &Cell<T> {
        match poly.next {
            false => &self.current[&poly.poly_id()],
            true => &self.next[&poly.poly_id()],
        }
    }

    pub fn get_value(&self, poly: &PolynomialReference) -> Option<T> {
        match self.get_cell(poly).value {
            CellValue::Known(value) => Some(value),
            _ => match self.unknown_strategy {
                UnknownStrategy::Zero => Some(T::zero()),
                UnknownStrategy::Unknown => None,
            },
        }
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
        match self.get_cell(poly).value {
            CellValue::RangeConstraint(ref c) => Some(c.clone()),
            _ => None,
        }
    }
}
