use p3_matrix::dense::RowMajorMatrix;
use powdr_ast::analyzed::Analyzed;

use powdr_number::Plonky3FieldElement;

use p3_air::{Air, AirBuilder, BaseAir};

#[derive(Clone)]
pub(crate) struct PowdrCircuit<'a, T> {
    /// The analyzed PIL
    _analyzed: &'a Analyzed<T>,
    /// The value of the fixed columns
    _fixed: &'a [(String, Vec<T>)],
    /// The value of the witness columns, if set
    _witness: Option<&'a [(String, Vec<T>)]>,
    /// Column name and index of the public cells
    _publics: Vec<(String, usize)>,
}

pub struct Plonky3Prover<'a, F> {
    _circuit: PowdrCircuit<'a, F>,
}

impl<'a, T: Plonky3FieldElement> BaseAir<T::Plonky3Field> for PowdrCircuit<'a, T> {
    fn width(&self) -> usize {
        todo!()
    }

    fn preprocessed_trace(&self) -> Option<RowMajorMatrix<T::Plonky3Field>> {
        None
    }
}

impl<'a, T: Plonky3FieldElement, AB: AirBuilder<F = T::Plonky3Field>> Air<AB>
    for PowdrCircuit<'a, T>
{
    fn eval(&self, _builder: &mut AB) {
        // TODO: actually encode the program
    }
}
