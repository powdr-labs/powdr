use stwo_prover::{constraint_framework::EvalAtRow, core::fields::m31::M31};
use powdr_number::{FieldElement, Mersenne31Field,LargeInt};



pub trait FieldElementMap<E: EvalAtRow>: FieldElement
{

    fn into_stwo_m31(self) -> M31;

    fn from_stwo_m31(stwo_m31:M31) -> Self;

    fn into_stwo_eval_field(self) -> E::F;
}

impl <E:EvalAtRow>FieldElementMap<E> for Mersenne31Field
{
    fn into_stwo_m31(self) -> M31
    {
        M31::from(self.to_integer().try_into_u32().unwrap())
    }

    fn from_stwo_m31(stwo_m31:M31) -> Self
    {
        Self::from(stwo_m31.0)
    }

    fn into_stwo_eval_field(self) -> <E as EvalAtRow>::F {
       E::F::from( self.to_integer().try_into_u32().unwrap().into())
    }
    
}