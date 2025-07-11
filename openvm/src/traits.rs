use openvm_stark_backend::p3_field::PrimeField32;
use powdr_number::FieldElement;

pub trait IntoOpenVm: FieldElement {
    type Field: PrimeField32;

    fn into_openvm_field(self) -> Self::Field;

    fn from_openvm_field(field: Self::Field) -> Self;
}

pub type OpenVmField<P> = <P as IntoOpenVm>::Field;
