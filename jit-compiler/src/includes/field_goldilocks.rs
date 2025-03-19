#[derive(Clone, Copy, Default, PartialEq, Eq)]
#[repr(transparent)]
struct GoldilocksField(u64);

type FieldElement = GoldilocksField;
type IntType = u64;

const EPSILON: u64 = (1 << 32) - 1;

impl GoldilocksField {
    const ORDER: u64 = 0xFFFFFFFF00000001;

    /// Returns the inverse of the field element, using Fermat's little theorem.
    /// The inverse of `a` is computed as `a^(p-2)`, where `p` is the prime order of the field.
    ///
    /// Mathematically, this is equivalent to:
    ///                $a^(p-1)     = 1 (mod p)$
    ///                $a^(p-2) * a = 1 (mod p)$
    /// Therefore      $a^(p-2)     = a^-1 (mod p)$
    ///
    /// The following code has been adapted from winterfell/math/src/field/f64/mod.rs
    /// located at <https://github.com/facebook/winterfell>.
    fn try_inverse(&self) -> Option<Self> {
        if self.0 == 0 {
            return None;
        }

        // compute base^(P - 2) using 72 multiplications
        // The exponent P - 2 is represented in binary as:
        // 0b1111111111111111111111111111111011111111111111111111111111111111

        // compute base^11
        let t2 = self.square() * *self;

        // compute base^111
        let t3 = t2.square() * *self;

        // compute base^111111 (6 ones)
        // repeatedly square t3 3 times and multiply by t3
        let t6 = exp_acc::<3>(t3, t3);

        // compute base^111111111111 (12 ones)
        // repeatedly square t6 6 times and multiply by t6
        let t12 = exp_acc::<6>(t6, t6);

        // compute base^111111111111111111111111 (24 ones)
        // repeatedly square t12 12 times and multiply by t12
        let t24 = exp_acc::<12>(t12, t12);

        // compute base^1111111111111111111111111111111 (31 ones)
        // repeatedly square t24 6 times and multiply by t6 first. then square t30 and
        // multiply by base
        let t30 = exp_acc::<6>(t24, t6);
        let t31 = t30.square() * *self;

        // compute base^111111111111111111111111111111101111111111111111111111111111111
        // repeatedly square t31 32 times and multiply by t31
        let t63 = exp_acc::<32>(t31, t31);

        // compute base^1111111111111111111111111111111011111111111111111111111111111111
        Some(t63.square() * *self)
    }

    fn square(&self) -> Self {
        *self * *self
    }

    fn exp_power_of_2(&self, power_log: usize) -> Self {
        let mut res = *self;
        for _ in 0..power_log {
            res = res.square();
        }
        res
    }

    fn from_canonical_u64(n: u64) -> Self {
        debug_assert!(n < Self::ORDER);
        Self(n)
    }

    fn to_canonical_u64(self) -> u64 {
        self.0
    }
}

fn wrap(x: u64) -> u64 {
    if x >= GoldilocksField::ORDER {
        x - GoldilocksField::ORDER
    } else {
        x
    }
}

impl std::ops::Neg for GoldilocksField {
    type Output = Self;

    fn neg(self) -> Self {
        if self.0 == 0 {
            self
        } else {
            Self(Self::ORDER - self.0)
        }
    }
}

impl std::ops::Add for GoldilocksField {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn add(self, rhs: Self) -> Self {
        let (sum, over) = self.0.overflowing_add(rhs.0);
        let (sum, over) = sum.overflowing_add((over as u64) * EPSILON);
        debug_assert!(!over);
        Self(wrap(sum))
    }
}

impl std::ops::Sub for GoldilocksField {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn sub(self, rhs: Self) -> Self {
        let (diff, under) = self.0.overflowing_sub(rhs.0);
        let (diff, under) = diff.overflowing_sub((under as u64) * EPSILON);
        debug_assert!(!under);
        Self(wrap(diff))
    }
}

impl std::ops::Mul for GoldilocksField {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        reduce128((self.0 as u128) * (rhs.0 as u128))
    }
}

impl std::ops::Div for GoldilocksField {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn div(self, b: Self) -> Self::Output {
        if b.0 == 0 {
            panic!("Division by zero");
        }
        if self.0 == 0 {
            return self;
        }

        let MODULUS = Self::ORDER;

        if let Some(result) = try_integer_div_without_remainder(self.0, b.0) {
            Self(result)
        } else if let Some(result) = try_integer_div_without_remainder(self.0, MODULUS - b.0) {
            Self(MODULUS - result)
        } else if let Some(result) = try_integer_div_without_remainder(MODULUS - self.0, b.0) {
            Self(MODULUS - result)
        } else if let Some(result) =
            try_integer_div_without_remainder(MODULUS - self.0, MODULUS - b.0)
        {
            Self(result)
        } else {
            full_field_div(self, b)
        }
    }
}

fn try_integer_div_without_remainder(a: u64, b: u64) -> Option<u64> {
    (a % b == 0).then(|| a / b)
}

fn full_field_div(a: GoldilocksField, b: GoldilocksField) -> GoldilocksField {
    a * b.try_inverse().unwrap()
}

/// Fast addition modulo ORDER for x86-64.
/// This function is marked unsafe for the following reasons:
///   - It is only correct if x + y < 2**64 + ORDER = 0x1ffffffff00000001.
///   - It is only faster in some circumstances. In particular, on x86 it overwrites both inputs in
///     the registers, so its use is not recommended when either input will be used again.

#[cfg(target_arch = "x86_64")]
unsafe fn add_no_canonicalize_trashing_input(x: u64, y: u64) -> u64 {
    let res_wrapped: u64;
    let adjustment: u64;
    core::arch::asm!(
        "add {0}, {1}",
        // Trick. The carry flag is set iff the addition overflowed.
        // sbb x, y does x := x - y - CF. In our case, x and y are both {1:e}, so it simply does
        // {1:e} := 0xffffffff on overflow and {1:e} := 0 otherwise. {1:e} is the low 32 bits of
        // {1}; the high 32-bits are zeroed on write. In the end, we end up with 0xffffffff in {1}
        // on overflow; this happens be EPSILON.
        // Note that the CPU does not realize that the result of sbb x, x does not actually depend
        // on x. We must write the result to a register that we know to be ready. We have a
        // dependency on {1} anyway, so let's use it.
        "sbb {1:e}, {1:e}",
        inlateout(reg) x => res_wrapped,
        inlateout(reg) y => adjustment,
        options(pure, nomem, nostack),
    );
    assume(x != 0 || (res_wrapped == y && adjustment == 0));
    assume(y != 0 || (res_wrapped == x && adjustment == 0));
    // Add EPSILON == subtract ORDER.
    // Cannot overflow unless the assumption if x + y < 2**64 + ORDER is incorrect.
    res_wrapped + adjustment
}

#[cfg(not(target_arch = "x86_64"))]
const unsafe fn add_no_canonicalize_trashing_input(x: u64, y: u64) -> u64 {
    let (res_wrapped, carry) = x.overflowing_add(y);
    // Below cannot overflow unless the assumption if x + y < 2**64 + ORDER is incorrect.
    res_wrapped + EPSILON * (carry as u64)
}

/// Reduces to a 64-bit value. The result is in canonical form.

fn reduce128(x: u128) -> GoldilocksField {
    let (x_lo, x_hi) = split(x); // This is a no-op
    let x_hi_hi = x_hi >> 32;
    let x_hi_lo = x_hi & EPSILON;

    let (mut t0, borrow) = x_lo.overflowing_sub(x_hi_hi);
    if borrow {
        branch_hint(); // A borrow is exceedingly rare. It is faster to branch.
        t0 -= EPSILON; // Cannot underflow.
    }
    let t1 = x_hi_lo * EPSILON;
    let t2 = unsafe { add_no_canonicalize_trashing_input(t0, t1) };

    GoldilocksField(wrap(t2))
}

/// Squares the base N number of times and multiplies the result by the tail value.

fn exp_acc<const N: usize>(base: GoldilocksField, tail: GoldilocksField) -> GoldilocksField {
    base.exp_power_of_2(N) * tail
}

const fn split(x: u128) -> (u64, u64) {
    (x as u64, (x >> 64) as u64)
}

#[cfg(target_arch = "x86_64")]
fn assume(p: bool) {
    debug_assert!(p);
    if !p {
        unsafe {
            core::hint::unreachable_unchecked();
        }
    }
}

/// Try to force Rust to emit a branch. Example:
///     if x > 2 {
///         y = foo();
///         branch_hint();
///     } else {
///         y = bar();
///     }
/// This function has no semantics. It is a hint only.

fn branch_hint() {
    // NOTE: These are the currently supported assembly architectures. See the
    // [nightly reference](https://doc.rust-lang.org/nightly/reference/inline-assembly.html) for
    // the most up-to-date list.
    #[cfg(any(
        target_arch = "aarch64",
        target_arch = "arm",
        target_arch = "riscv32",
        target_arch = "riscv64",
        target_arch = "x86",
        target_arch = "x86_64",
    ))]
    unsafe {
        core::arch::asm!("", options(nomem, nostack, preserves_flags));
    }
}

impl From<u64> for GoldilocksField {
    fn from(n: u64) -> Self {
        Self(wrap(n))
    }
}

impl From<FieldElement> for IntType {
    fn from(f: FieldElement) -> Self {
        f.0
    }
}

impl std::fmt::Display for GoldilocksField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn integer_div(a: GoldilocksField, b: GoldilocksField) -> GoldilocksField {
    GoldilocksField(a.0 / b.0)
}

impl std::ops::BitAnd<u64> for GoldilocksField {
    type Output = Self;

    fn bitand(self, b: u64) -> GoldilocksField {
        Self(self.0 & b)
    }
}
impl std::ops::BitOr<GoldilocksField> for GoldilocksField {
    type Output = Self;

    fn bitor(self, b: GoldilocksField) -> GoldilocksField {
        Self(self.0 | b.0)
    }
}

impl From<ibig::IBig> for FieldElement {
    fn from(x: ibig::IBig) -> Self {
        FieldElement::from(u64::try_from(x).unwrap())
    }
}
impl From<FieldElement> for ibig::IBig {
    fn from(x: FieldElement) -> Self {
        ibig::IBig::from(IntType::from(x))
    }
}
