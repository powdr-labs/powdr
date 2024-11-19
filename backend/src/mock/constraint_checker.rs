use powdr_number::FieldElement;

pub struct ConstraintChecker<'a, F> {
    witness: &'a [(String, Vec<F>)],
    fixed: &'a [(String, &'a [F])],
}

impl<'a, F: FieldElement> ConstraintChecker<'a, F> {
    pub fn new(witness: &'a [(String, Vec<F>)], fixed: &'a [(String, &'a [F])]) -> Self {
        Self { witness, fixed }
    }

    pub fn check(&self) {
        todo!()
    }
}
