use p3_matrix::dense::RowMajorMatrix;
use powdr_ast::analyzed::Analyzed;

use powdr_number::Plonky3FieldElement;

use p3_air::{Air, AirBuilder, BaseAir};

struct Wrapper<T> {
    _analyzed: Analyzed<T>,
}

impl<T: Plonky3FieldElement> BaseAir<T::Plonky3Field> for Wrapper<T> {
    fn width(&self) -> usize {
        todo!()
    }

    fn preprocessed_trace(&self) -> Option<RowMajorMatrix<T::Plonky3Field>> {
        None
    }
}

impl<T: Plonky3FieldElement, AB: AirBuilder<F = T::Plonky3Field>> Air<AB> for Wrapper<T> {
    fn eval(&self, _builder: &mut AB) {
        // TODO: actually encode the program
    }
}
