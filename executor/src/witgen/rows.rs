use std::{collections::BTreeMap, fmt::Debug};

use ast::analyzed::PolynomialReference;
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
struct Cell<T: FieldElement> {
    value: Option<T>,
    /// Should be initialized with global range constraints
    range_constraint: Option<RangeConstraint<T>>,
}

/// A row of cells, indexed by polynomial ID.
#[derive(Clone)]
pub struct Row<T: FieldElement> {
    /// Maps from poly ID to cell.
    cells: BTreeMap<usize, Cell<T>>,
}

impl<T: FieldElement> Debug for Row<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let values_string = self
            .cells
            .iter()
            .filter_map(|(k, cell)| cell.value.map(|v| format!("{k}: {v}")))
            .collect::<Vec<_>>()
            .join(", ");
        f.write_str(&format!("({})", values_string))
    }
}

impl<T: FieldElement> Row<T> {
    fn get_cell_mut(&mut self, id: usize) -> &mut Cell<T> {
        self.cells.get_mut(&id).unwrap()
    }

    fn get_cell(&self, id: usize) -> &Cell<T> {
        self.cells.get(&id).unwrap()
    }

    pub fn iter_values(&self) -> impl Iterator<Item = (usize, Option<T>)> + '_ {
        self.cells
            .iter()
            .map(|(poly_id, cell)| (*poly_id, cell.value))
    }

    pub fn iter_range_constraints(
        &self,
    ) -> impl Iterator<Item = (usize, Option<RangeConstraint<T>>)> + '_ {
        self.cells
            .iter()
            .map(|(poly_id, cell)| (*poly_id, cell.range_constraint.clone()))
    }
}

impl<T: FieldElement> From<Row<T>> for BTreeMap<usize, T> {
    fn from(val: Row<T>) -> Self {
        val.cells
            .into_iter()
            .map(|(poly_id, cell)| (poly_id, cell.value.unwrap_or_default()))
            .collect()
    }
}

/// A factory for creating rows.
pub struct RowFactory<'a, T: FieldElement> {
    /// The list of all available polynomials.
    polys: Vec<PolynomialReference>,
    /// Global range constraints.
    global_range_constraints: BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
    id_to_name: BTreeMap<usize, String>,
}

impl<'a, T: FieldElement> RowFactory<'a, T> {
    pub fn new(
        polys: Vec<PolynomialReference>,
        global_range_constraints: BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
    ) -> Self {
        let id_to_name = polys
            .iter()
            .map(|p| (p.poly_id() as usize, p.name.clone()))
            .collect();
        Self {
            polys,
            global_range_constraints,
            id_to_name,
        }
    }

    pub fn render_row(&self, row: &Row<T>, title: &str) -> String {
        let render_cell = |cell: &Cell<T>| match cell.value {
            Some(v) => format!("{}", v),
            None => "?".to_string(),
        };
        let values_string = row
            .cells
            .iter()
            .map(|(k, cell)| format!("    {}: {}", self.id_to_name[k], render_cell(cell)))
            .collect::<Vec<_>>()
            .join("\n");
        format!("{title}:\n{}", values_string)
    }

    /// Create a new row without values and just the global range constraints.
    pub fn fresh_row(&self) -> Row<T> {
        let cells = self
            .polys
            .iter()
            .map(|poly| {
                (
                    poly.poly_id() as usize,
                    Cell {
                        value: None,
                        range_constraint: self.global_range_constraints.get(&poly).cloned(),
                    },
                )
            })
            .collect();
        Row { cells }
    }

    /// Create a new row from known values. Range constraints are left empty.
    pub fn make_from_known_values(&self, values: &BTreeMap<usize, T>) -> Row<T> {
        let cells = self
            .polys
            .iter()
            .map(|poly| {
                let poly_id = poly.poly_id() as usize;
                let cell = Cell {
                    value: Some(*values.get(&poly_id).unwrap()),
                    range_constraint: None,
                };
                (poly_id, cell)
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
pub struct RowPair<'a, T: FieldElement> {
    current: &'a mut Row<T>,
    next: &'a mut Row<T>,
    pub current_row_index: DegreeType,
    frozen: bool,
}

impl<'a, T: FieldElement> RowPair<'a, T> {
    pub fn new(
        current: &'a mut Row<T>,
        next: &'a mut Row<T>,
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

    fn get_cell_mut<'b>(&'b mut self, poly: &'b PolynomialReference) -> &'b mut Cell<T> {
        if self.frozen {
            panic!("Cannot modify frozen row pair");
        }

        let id = poly.poly_id() as usize;
        match poly.next {
            false => self.current.get_cell_mut(id),
            true => self.next.get_cell_mut(id),
        }
    }

    fn get_cell(&self, poly: &PolynomialReference) -> &Cell<T> {
        let id = poly.poly_id() as usize;
        match poly.next {
            false => self.current.get_cell(id),
            true => self.next.get_cell(id),
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

    pub fn process_eval_value<'b>(
        &mut self,
        evaluation_value: EvalValue<&'b PolynomialReference, T>,
        source_name: impl Fn() -> String,
    ) -> UpdateStatus<'b> {
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

impl<'a, T: FieldElement> WitnessColumnEvaluator<T> for RowPair<'a, T> {
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

impl<'a, T: FieldElement> RangeConstraintSet<&PolynomialReference, T> for RowPair<'a, T> {
    fn range_constraint(&self, poly: &PolynomialReference) -> Option<RangeConstraint<T>> {
        self.get_cell(poly).range_constraint.clone()
    }
}
