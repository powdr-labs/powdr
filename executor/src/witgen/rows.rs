use std::{collections::BTreeMap, fmt::Debug};

use ast::analyzed::PolynomialReference;
use itertools::Itertools;
use number::{DegreeType, FieldElement};

use crate::witgen::Constraint;

use super::{
    affine_expression::{AffineExpression, AffineResult},
    global_constraints::RangeConstraintSet,
    range_constraints::RangeConstraint,
    symbolic_witness_evaluator::WitnessColumnEvaluator,
    EvalStatus, EvalValue, IncompleteCause,
};

/// A single cell, holding an optional value and range constraint.
#[derive(Clone)]
struct Cell<'a, T: FieldElement> {
    name: &'a str,
    value: Option<T>,
    /// Should be initialized with global range constraints
    range_constraint: Option<RangeConstraint<T>>,
}

/// A row of cells, indexed by polynomial ID.
#[derive(Clone)]
pub struct Row<'a, T: FieldElement> {
    cells: BTreeMap<u64, Cell<'a, T>>,
}

impl<T: FieldElement> Debug for Row<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.render("Row:", true))
    }
}

impl<'a, T: FieldElement> Row<'a, T> {
    fn get_cell_mut(&mut self, poly: &PolynomialReference) -> &mut Cell<'a, T> {
        self.cells.get_mut(&poly.poly_id()).unwrap()
    }

    fn get_cell(&self, poly: &PolynomialReference) -> &Cell<'a, T> {
        self.cells.get(&poly.poly_id()).unwrap()
    }

    pub fn iter_values(&self) -> impl Iterator<Item = (usize, Option<T>)> + '_ {
        self.cells
            .iter()
            .map(|(poly_id, cell)| (*poly_id as usize, cell.value))
    }

    pub fn iter_range_constraints(
        &self,
    ) -> impl Iterator<Item = (usize, Option<RangeConstraint<T>>)> + '_ {
        self.cells
            .iter()
            .map(|(poly_id, cell)| (*poly_id as usize, cell.range_constraint.clone()))
    }

    pub fn render(&self, title: &str, include_unknown: bool) -> String {
        format!("{}:\n{}", title, self.render_values(include_unknown))
    }

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

        let render_cell = |cell: &Cell<'_, T>| match cell.value {
            Some(v) => format!("{} = {}", cell.name, v),
            None => format!("{} = <unknown>", cell.name),
        };

        cells
            .into_iter()
            .map(|(_, cell)| format!("    {}", render_cell(cell)))
            .join("\n")
    }
}

impl<T: FieldElement> From<Row<'_, T>> for BTreeMap<usize, T> {
    fn from(val: Row<T>) -> Self {
        val.cells
            .into_iter()
            .map(|(poly_id, cell)| (poly_id as usize, cell.value.unwrap_or_default()))
            .collect()
    }
}

/// A factory for creating rows.
pub struct RowFactory<'a, T: FieldElement> {
    /// The list of all available polynomials.
    polys: Vec<&'a PolynomialReference>,
    /// Global range constraints.
    global_range_constraints: BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
}

impl<'a, T: FieldElement> RowFactory<'a, T> {
    pub fn new(
        polys: Vec<&'a PolynomialReference>,
        global_range_constraints: BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
    ) -> Self {
        Self {
            polys,
            global_range_constraints,
        }
    }

    /// Create a new row without values and just the global range constraints.
    pub fn fresh_row(&self) -> Row<'a, T> {
        let cells = self
            .polys
            .iter()
            .map(|poly| {
                (
                    poly.poly_id(),
                    Cell {
                        name: &poly.name,
                        value: None,
                        range_constraint: self.global_range_constraints.get(poly).cloned(),
                    },
                )
            })
            .collect();
        Row { cells }
    }

    /// Create a new row from known values. Range constraints are left empty.
    pub fn make_from_known_values(&self, values: &BTreeMap<usize, T>) -> Row<'a, T> {
        let cells = self
            .polys
            .iter()
            .map(|poly| {
                let cell = Cell {
                    name: &poly.name,
                    value: Some(*values.get(&(poly.poly_id() as usize)).unwrap()),
                    range_constraint: None,
                };
                (poly.poly_id(), cell)
            })
            .collect();
        Row { cells }
    }
}

pub struct UpdateStatus<'a> {
    pub eval_status: EvalStatus<&'a PolynomialReference>,
    pub progress: bool,
}

impl UpdateStatus<'_> {
    pub fn is_complete(&self) -> bool {
        match self.eval_status {
            EvalStatus::Complete => true,
            EvalStatus::Incomplete(_) => false,
        }
    }
}

impl<'a> From<IncompleteCause<&'a PolynomialReference>> for UpdateStatus<'a> {
    fn from(cause: IncompleteCause<&'a PolynomialReference>) -> Self {
        Self {
            progress: false,
            eval_status: cause.into(),
        }
    }
}

/// A pair of rows, needed to process a single identity.
pub struct RowPair<'a, 'b, T: FieldElement> {
    current: &'a mut Row<'b, T>,
    next: &'a mut Row<'b, T>,
    pub current_row_index: DegreeType,
    frozen: bool,
}

impl<'a, 'b, T: FieldElement> RowPair<'a, 'b, T> {
    pub fn new(
        current: &'a mut Row<'b, T>,
        next: &'a mut Row<'b, T>,
        current_row_index: DegreeType,
    ) -> Self {
        Self {
            current,
            next,
            current_row_index,
            frozen: false,
        }
    }

    /// Freeze the row pair, preventing further modifications.
    /// When reading, any unknown values will be returned as zero.
    pub fn freeze(&mut self) {
        self.frozen = true;
    }

    fn get_cell_mut<'c>(&'c mut self, poly: &PolynomialReference) -> &'c mut Cell<'b, T> {
        if self.frozen {
            panic!("Cannot modify frozen row pair");
        }

        match poly.next {
            false => self.current.get_cell_mut(poly),
            true => self.next.get_cell_mut(poly),
        }
    }

    fn get_cell(&self, poly: &PolynomialReference) -> &Cell<T> {
        match poly.next {
            false => self.current.get_cell(poly),
            true => self.next.get_cell(poly),
        }
    }

    /// Set the value of a cell given its polynomial reference.
    pub fn set_value(&mut self, poly: &PolynomialReference, value: T) {
        log::trace!("      => {poly} = {value}");
        self.get_cell_mut(poly).value = Some(value);
    }

    pub fn get_value(&self, poly: &PolynomialReference) -> Option<T> {
        self.get_cell(poly).value
    }

    /// Update the range constraint of a cell given its polynomial reference.
    fn update_range_constraint(
        &mut self,
        poly: &PolynomialReference,
        constraint: RangeConstraint<T>,
    ) {
        log::trace!("      => Adding range constraint for {poly}: {constraint}");
        let cell = self.get_cell_mut(poly);
        let old = &mut cell.range_constraint;
        let new = match old {
            Some(c) => constraint.conjunction(c),
            None => constraint,
        };
        log::trace!("         (the conjunction is {})", new);
        *old = Some(new);
    }

    pub fn process_eval_value<'c>(
        &mut self,
        evaluation_value: EvalValue<&'c PolynomialReference, T>,
        source_name: impl Fn() -> String,
    ) -> UpdateStatus<'c> {
        if evaluation_value.constraints.is_empty() {
            return UpdateStatus {
                eval_status: evaluation_value.status,
                progress: false,
            };
        }

        log::trace!("    Processing: {}", source_name());
        for (poly, c) in evaluation_value.constraints {
            match c {
                Constraint::Assignment(value) => {
                    self.set_value(poly, value);
                }
                Constraint::RangeConstraint(constraint) => {
                    self.update_range_constraint(poly, constraint);
                }
            }
        }
        UpdateStatus {
            eval_status: evaluation_value.status,
            progress: true,
        }
    }
}

impl<T: FieldElement> WitnessColumnEvaluator<T> for RowPair<'_, '_, T> {
    fn value<'b>(&self, poly: &'b PolynomialReference) -> AffineResult<&'b PolynomialReference, T> {
        let value =
            self.get_cell(poly)
                .value
                .or_else(|| if self.frozen { Some(T::zero()) } else { None });

        Ok(match value {
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
