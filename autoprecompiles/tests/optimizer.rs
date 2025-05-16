use std::fmt;
use std::hash::Hash;
use std::{collections::HashSet, fmt::Display};

use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicReference, Challenge};
use powdr_autoprecompiles::SymbolicMachine;
use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::{BabyBearField, FieldElement};
use powdr_pilopt::qse_opt::algebraic_to_quadratic_symbolic_expression;

#[test]
fn load_machine_cbor() {
    let file = std::fs::File::open("tests/keccak_apc_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert_eq!(machine.constraints.len(), 6770);
    assert_eq!(machine.bus_interactions.len(), 3573);
}

#[test]
fn analyze_for_memory() {
    let file = std::fs::File::open("tests/keccak_apc_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();

    for constr in machine.constraints {
        let constr = algebraic_to_quadratic_symbolic_expression(&constr.expr);
        let constr = try_remove_is_valid(&constr).unwrap_or(&constr);
        if let Some(candidate) = QuadraticEqualityCandidate::try_from_qse(constr) {
            if let Some(limb_0) = constr
                .referenced_variables()
                .find(|v| v.to_string().contains("mem_ptr_limbs__0"))
            {
                let candidate = candidate.normalized_for_var(limb_0);
                if let Some(limb_1) = constr
                    .referenced_variables()
                    .find(|v| v.to_string().contains("mem_ptr_limbs__1"))
                {
                    // TODO here we need to solve for a QSE not for a variable
                    println!("{candidate}");
                } else {
                    // TODO solve_for is only correct if it is already normalized for that var,
                    // otherwise the offset is wrong
                    if let Some(sol) = candidate.expr().try_solve_for(limb_0) {
                        println!("{limb_0} = {sol} +? {}", candidate.offset());
                    } else {
                        println!("{}", candidate.normalized_for_var(limb_0));
                    }
                }
            } else {
                println!("{candidate}");
            }
        } else {
            println!("{constr}");
        }
    }
}

fn try_remove_is_valid<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    expr: &QuadraticSymbolicExpression<T, V>,
) -> Option<&QuadraticSymbolicExpression<T, V>> {
    // TODO this is a crude heursitic
    let (left, right) = expr.try_as_single_product()?;
    if left.is_affine()
        && left.referenced_variables().count() == 1
        && left.to_string() == "is_valid"
    {
        Some(right)
    } else if right.is_affine()
        && right.referenced_variables().count() == 1
        && right.to_string() == "is_valid"
    {
        Some(left)
    } else {
        None
    }
}

struct QuadraticEqualityCandidate<T: FieldElement, V: Ord + Clone + Hash + Eq> {
    expr: QuadraticSymbolicExpression<T, V>,
    offset: SymbolicExpression<T, V>,
    /// All unknown variables in `expr`.
    variables: HashSet<V>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display> Display
    for QuadraticEqualityCandidate<T, V>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} +? {}", self.expr, self.offset)
    }
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq> QuadraticEqualityCandidate<T, V> {
    fn try_from_qse(constr: &QuadraticSymbolicExpression<T, V>) -> Option<Self> {
        let (left, right) = constr.try_as_single_product()?;
        if !left.is_affine() || !right.is_affine() {
            return None;
        }
        // `constr = 0` is equivalent to `left * right = 0`
        let offset = (left - right).try_to_known()?.try_to_number()?;
        // `offset + right = left`
        // `constr = 0` is equivalent to `right * (right + offset) = 0`
        let variables = right
            .referenced_unknown_variables()
            .cloned()
            .collect::<HashSet<_>>();
        Some(Self {
            expr: right.clone(),
            offset: offset.into(),
            variables,
        })
    }

    /// Returns an equivalent candidate that is normalized
    /// such that `var` has a coefficient of `1`.
    fn normalized_for_var(&self, var: &V) -> Self {
        let inverse_coefficient = self
            .expr
            .coefficient_of_variable(var)
            .unwrap()
            .field_inverse();

        // self represents
        // `(coeff * var + X) * (coeff * var + X + offset) = 0`
        // Dividing by `coeff` twice results in
        // `(var + X / coeff) * (var + X / coeff + offset / coeff) = 0`
        let offset = &self.offset * &inverse_coefficient;
        let expr = self.expr.clone() * inverse_coefficient;
        Self {
            expr,
            offset,
            variables: self.variables.clone(),
        }
    }

    fn expr(&self) -> &QuadraticSymbolicExpression<T, V> {
        &self.expr
    }

    fn offset(&self) -> &SymbolicExpression<T, V> {
        &self.offset
    }
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub enum Variable {
    Reference(AlgebraicReference),
    PublicReference(String),
    Challenge(Challenge),
    /// A range check variable
    RangeCheck {
        id: usize,
    },
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Reference(r) => write!(f, "{r}"),
            Variable::PublicReference(r) => write!(f, "{r}"),
            Variable::Challenge(c) => write!(f, "{c}"),
            Variable::RangeCheck { id } => write!(f, "range_check_{id}"),
        }
    }
}

// ok, and this horrible big constraint (is_valid) * ((-943718400 * mem_ptr_limbs__0_1 + -7864320 * rs1_data__3_1 + 30720 * mem_ptr_limbs__1_1 + 943718400 * rs1_data__0_661 + -120 * rs1_data__1_661 + -30720 * rs1_data__2_661 + -503316529) * (-943718400 * mem_ptr_limbs__0_1 + -7864320 * rs1_data__3_1 + 30720 * mem_ptr_limbs__1_1 + 943718400 * rs1_data__0_661 + -120 * rs1_data__1_661 + -30720 * rs1_data__2_661 + -503316530)) essentially says mem_ptr_limbs__0_1 + mem_ptr_limbs__1_1 * 65536 = some rs1_data thing modulo 2**32
// ok, I think I'm slowly catching up again
// and what we want to know in this analysis in the end is the differences in these mem pointers
// and the thing is that the expression that appears in the memory bus send is mem_ptr_limbs__0_1 + mem_ptr_limbs__1_1 * 65536
// so what we could do is find the memory bus send, take the expression as a QSE (essentially define a temporary variable x that equals that QSE), find the other constraints where this QSE appears and "solve" them for x.
// this way, we know what gets sent to the memory bus
// and if we solve for x, we get some conditional assignment - it is conditional because of the wrapping.
// oh wait, it's not what we write, it's the address we write to
// and then as soon as we have done this analysis, the next step is to go through the program step by step, looking at all memory reads and writes. And we keep track of what we currently know about memory. If we see a load from an address and we know the value (v) at that address, we remove the load and instead add an equality constraint v = where_to_put_the_value
// If we see a store at an address a (which is a conditional assignment as above), we first need to compare with all the addresses we know about and compute the difference. If the difference is a fixed number larger than the word size, we can keep what we know. Otherwise, we need to delete the knowledge. Finally, we store the new knowledge.
// So what to figure out next: How to compute differences of these conditional assignments
// I think it should work by just computing all 4 combinations and verifying that they are not zero. We should get two different values, a "small" number and that number plus 2**32

//

// TODO problem that rs1_data__3_109 is always different.

// mem_ptr_limbs__0_109 + -16777216 * rs1_data__3_109 + 65536 * mem_ptr_limbs__1_109 + -rs1_data__0_661 + -256 * rs1_data__1_661 + -65536 * rs1_data__2_661 + 268435354 +? -268435454

// TODO introduce new variable type "range check(expr)".
