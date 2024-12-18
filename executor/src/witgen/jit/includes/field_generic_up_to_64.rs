// The following types are defined in the including code:
// #[derive(Clone, Copy, Default)]
// #[repr(transparent)]
// struct FieldElement({int_type});

// type IntType = {int_type};
// type DoubleIntType = {double_int_type};
// const MODULUS: IntType = {modulus}_{int_type};

impl std::fmt::Display for FieldElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<IntType> for FieldElement {
    #[inline]
    fn from(i: IntType) -> Self {
        Self(i)
    }
}
impl std::ops::Add for FieldElement {
    type Output = Self;
    #[inline]
    fn add(self, b: Self) -> Self {
        // TODO this is inefficient.
        Self(
            IntType::try_from(
                ((self.0 as DoubleIntType) + (b.0 as DoubleIntType)) % (MODULUS as DoubleIntType),
            )
            .unwrap(),
        )
    }
}
impl std::ops::Sub for FieldElement {
    type Output = Self;
    #[inline]
    fn sub(self, b: Self) -> Self {
        // TODO this is inefficient.
        Self(
            IntType::try_from(
                ((self.0 as DoubleIntType) + (MODULUS as DoubleIntType) - (b.0 as DoubleIntType))
                    % (MODULUS as DoubleIntType),
            )
            .unwrap(),
        )
    }
}
impl std::ops::Neg for FieldElement {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        if self.0 == 0 {
            self
        } else {
            Self(MODULUS - self.0)
        }
    }
}
impl std::ops::Mul<FieldElement> for FieldElement {
    type Output = Self;
    #[inline]
    fn mul(self, b: FieldElement) -> FieldElement {
        // TODO this is inefficient.
        Self(
            IntType::try_from(
                ((self.0 as DoubleIntType) * (b.0 as DoubleIntType)) % (MODULUS as DoubleIntType),
            )
            .unwrap(),
        )
    }
}
impl std::ops::Div<FieldElement> for FieldElement {
    type Output = Self;
    #[inline]
    fn div(self, b: FieldElement) -> FieldElement {
        if b.0 == 0 {
            panic!("Division by zero");
        }

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
#[inline]
fn try_integer_div_without_remainder(a: IntType, b: IntType) -> Option<IntType> {
    (a % b == 0).then(|| a / b)
}
fn full_field_div(_: FieldElement, _: FieldElement) -> FieldElement {
    todo!()
    // TODO generate the algorithm we use for goldilocks
    // for a generic prime field.
}
#[inline]
fn integer_div(a: FieldElement, b: FieldElement) -> FieldElement {
    FieldElement(a.0 / b.0)
}
impl std::ops::BitAnd<FieldElement> for FieldElement {
    type Output = Self;
    #[inline]
    fn bitand(self, b: FieldElement) -> FieldElement {
        Self(self.0 & b.0)
    }
}
impl std::ops::BitOr<FieldElement> for FieldElement {
    type Output = Self;
    #[inline]
    fn bitor(self, b: FieldElement) -> FieldElement {
        Self(self.0 | b.0)
    }
}
