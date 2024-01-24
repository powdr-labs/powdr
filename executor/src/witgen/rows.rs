use std::{collections::HashSet, fmt::Debug};

use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicExpression as Expression, AlgebraicReference, PolyID};
use powdr_number::{DegreeType, FieldElement};

use crate::witgen::Constraint;

use super::{
    affine_expression::{AffineExpression, AffineResult},
    data_structures::column_map::WitnessColumnMap,
    expression_evaluator::ExpressionEvaluator,
    global_constraints::{GlobalConstraints, RangeConstraintSet},
    range_constraints::RangeConstraint,
    symbolic_witness_evaluator::{SymoblicWitnessEvaluator, WitnessColumnEvaluator},
    FixedData,
};

#[derive(Clone, PartialEq, Debug)]
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

    /// Returns the new combined range constraint or new value for this cell.
    ///
    /// # Panics
    /// Panics if the update is not an improvement.
    pub fn update_with(&self, c: &Constraint<T>) -> Self {
        match (self, c) {
            (CellValue::Known(_), _) => {
                // Note that this is a problem even if the value that was set is the same,
                // because we would return that progress was made when it wasn't.
                panic!("Value was already set.");
            }
            (_, Constraint::Assignment(v)) => CellValue::Known(*v),
            (CellValue::RangeConstraint(current), Constraint::RangeConstraint(c)) => {
                let new = c.conjunction(current);
                assert!(new != *current, "Range constraint was already set");
                log::trace!("         (the conjunction is {})", new);
                CellValue::RangeConstraint(new)
            }
            (CellValue::Unknown, Constraint::RangeConstraint(c)) => {
                CellValue::RangeConstraint(c.clone())
            }
        }
    }
}

impl<T: FieldElement> From<CellValue<T>> for Option<T> {
    fn from(val: CellValue<T>) -> Self {
        match val {
            CellValue::Known(v) => Some(v),
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

impl<'a, T: FieldElement> Cell<'a, T> {
    /// Applies the new range constraint or new value to this cell.
    ///
    /// # Panics
    /// Panics if the update is not an improvement.
    pub fn apply_update(&mut self, c: &Constraint<T>) {
        self.value = self.value.update_with(c);
    }
}

impl<T: FieldElement> Debug for Cell<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let debug_str = match &self.value {
            CellValue::Known(v) => format!("{} = {}", self.name, v),
            CellValue::RangeConstraint(rc) => {
                format!("{} = ?  (range constraint: {})", self.name, rc)
            }
            CellValue::Unknown => format!("{} = ?", self.name),
        };
        f.write_str(&debug_str)
    }
}

/// A row of cells, indexed by polynomial ID.
pub type Row<'a, T> = WitnessColumnMap<Cell<'a, T>>;

impl<T: FieldElement> Debug for Row<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Row:\n{}", self.render_values(true, None))
    }
}

impl<T: FieldElement> Row<'_, T> {
    /// Builds a string representing the current row
    pub fn render(&self, title: &str, include_unknown: bool, cols: &HashSet<PolyID>) -> String {
        format!(
            "{}:\n{}\n---------------------",
            title,
            self.render_values(include_unknown, Some(cols))
        )
    }

    /// Builds a string listing all values, one by row. Nonzero entries are
    /// first, then zero, then unknown (if `include_unknown == true`).
    pub fn render_values(&self, include_unknown: bool, cols: Option<&HashSet<PolyID>>) -> String {
        let mut cells = self
            .iter()
            .filter(|(_, cell)| cell.value.is_known() || include_unknown)
            .filter(|(col, _)| cols.map(|cols| cols.contains(col)).unwrap_or(true))
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
    global_range_constraints: GlobalConstraints<T>,
}

impl<'a, T: FieldElement> RowFactory<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        global_range_constraints: GlobalConstraints<T>,
    ) -> Self {
        Self {
            fixed_data,
            global_range_constraints,
        }
    }

    pub fn fresh_row(&self, row: DegreeType) -> Row<'a, T> {
        WitnessColumnMap::from(
            self.global_range_constraints
                .witness_constraints
                .iter()
                .map(|(poly_id, range_constraint)| {
                    let name = self.fixed_data.column_name(&poly_id);
                    let value = match (
                        self.fixed_data.external_witness(row, &poly_id),
                        range_constraint.as_ref(),
                    ) {
                        (Some(external_witness), _) => CellValue::Known(external_witness),
                        (None, Some(range_constraint)) => {
                            CellValue::RangeConstraint(range_constraint.clone())
                        }
                        (None, None) => CellValue::Unknown,
                    };
                    Cell { name, value }
                }),
        )
    }
}

impl<T: FieldElement> From<Row<'_, T>> for WitnessColumnMap<T> {
    /// Builds a map from polynomial ID to value. Unknown values are set to zero.
    fn from(val: Row<T>) -> Self {
        WitnessColumnMap::from(
            val.into_iter()
                .map(|(_, cell)| cell.value.unwrap_or_default()),
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

    pub fn apply_update(&mut self, poly: &AlgebraicReference, c: &Constraint<T>) {
        match c {
            Constraint::Assignment(value) => {
                log::trace!(
                    "      => {} (Row {}) = {}",
                    poly.name,
                    self.row_number(poly),
                    value
                );
            }
            Constraint::RangeConstraint(constraint) => {
                log::trace!(
                    "      => Adding range constraint for {} (Row {}): {}",
                    poly.name,
                    self.row_number(poly),
                    constraint
                );
            }
        }
        self.get_cell_mut(poly).apply_update(c);
    }

    fn get_cell_mut<'b>(&'b mut self, poly: &AlgebraicReference) -> &'b mut Cell<'a, T> {
        match poly.next {
            false => &mut self.current[&poly.poly_id],
            true => &mut self.next[&poly.poly_id],
        }
    }

    fn row_number(&self, poly: &AlgebraicReference) -> DegreeType {
        match poly.next {
            false => self.current_row_index,
            true => self.current_row_index + 1,
        }
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
/// to return for a given [AlgebraicReference].
pub struct RowPair<'row, 'a, T: FieldElement> {
    pub current: &'row Row<'a, T>,
    pub next: Option<&'row Row<'a, T>>,
    pub current_row_index: DegreeType,
    fixed_data: &'a FixedData<'a, T>,
    unknown_strategy: UnknownStrategy,
}
impl<'row, 'a, T: FieldElement> RowPair<'row, 'a, T> {
    /// Creates a new row pair.
    pub fn new(
        current: &'row Row<'a, T>,
        next: &'row Row<'a, T>,
        current_row_index: DegreeType,
        fixed_data: &'a FixedData<'a, T>,
        unknown_strategy: UnknownStrategy,
    ) -> Self {
        Self {
            current,
            next: Some(next),
            current_row_index,
            fixed_data,
            unknown_strategy,
        }
    }

    /// Creates a new row pair from a single row, setting the next row to None.
    pub fn from_single_row(
        current: &'row Row<'a, T>,
        current_row_index: DegreeType,
        fixed_data: &'a FixedData<'a, T>,
        unknown_strategy: UnknownStrategy,
    ) -> Self {
        Self {
            current,
            next: None,
            current_row_index,
            fixed_data,
            unknown_strategy,
        }
    }

    /// Gets the cell corresponding to the given polynomial reference.
    ///
    /// # Panics
    /// Panics if the next row is accessed but the row pair has been constructed with
    /// [RowPair::from_single_row].
    fn get_cell(&self, poly: &AlgebraicReference) -> &Cell<T> {
        match (poly.next, self.next.as_ref()) {
            (false, _) => &self.current[&poly.poly_id],
            (true, Some(next)) => &next[&poly.poly_id],
            (true, None) => panic!("Tried to access next row, but it is not available."),
        }
    }

    pub fn get_value(&self, poly: &AlgebraicReference) -> Option<T> {
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
    pub fn evaluate<'b>(&self, expr: &'b Expression<T>) -> AffineResult<&'b AlgebraicReference, T> {
        ExpressionEvaluator::new(SymoblicWitnessEvaluator::new(
            self.fixed_data,
            self.current_row_index,
            self,
        ))
        .evaluate(expr)
    }

    /// Returns Ok(true) if the given row number references the "next" row,
    /// Ok(false) if it references the "current" row and Err if it is out of range.
    pub fn is_row_number_next(&self, row_number: DegreeType) -> Result<bool, ()> {
        match row_number - self.current_row_index {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(()),
        }
    }
}

impl<T: FieldElement> WitnessColumnEvaluator<T> for RowPair<'_, '_, T> {
    fn value<'b>(&self, poly: &'b AlgebraicReference) -> AffineResult<&'b AlgebraicReference, T> {
        Ok(match self.get_value(poly) {
            Some(v) => v.into(),
            None => AffineExpression::from_variable_id(poly),
        })
    }
}

impl<T: FieldElement> RangeConstraintSet<&AlgebraicReference, T> for RowPair<'_, '_, T> {
    fn range_constraint(&self, poly: &AlgebraicReference) -> Option<RangeConstraint<T>> {
        match self.get_cell(poly).value {
            CellValue::RangeConstraint(ref c) => Some(c.clone()),
            _ => None,
        }
    }
}
