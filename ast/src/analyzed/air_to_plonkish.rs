use std::default;

use crate::analyzed::AlgebraicBinaryOperation;
use crate::analyzed::AlgebraicBinaryOperator;
use crate::analyzed::AlgebraicExpression;
use crate::analyzed::AlgebraicReference;
use crate::analyzed::PolyID;
use crate::analyzed::PolynomialType;
use powdr_number::FieldElement;

#[derive(Clone, Debug)]
pub struct PlonkishExpression<T>
where
    T: FieldElement,
{
    pub row: u64,
    pub ql: AlgebraicExpression<T>,
    pub qr: AlgebraicExpression<T>,
    pub qo: AlgebraicExpression<T>,
    pub qmul: AlgebraicExpression<T>,
    pub qconst: AlgebraicExpression<T>,
    pub a: AlgebraicExpression<T>,
    pub b: AlgebraicExpression<T>,
    pub c: AlgebraicExpression<T>,
}

impl<T> Default for PlonkishExpression<T>
where
    T: FieldElement,
{
    fn default() -> Self {
        PlonkishExpression {
            row: 0,
            ql: AlgebraicExpression::Number(T::zero()),
            qr: AlgebraicExpression::Number(T::zero()),
            qo: AlgebraicExpression::Number(T::zero()),
            qmul: AlgebraicExpression::Number(T::zero()),
            qconst: AlgebraicExpression::Number(T::zero()),
            a: AlgebraicExpression::Number(T::zero()),
            b: AlgebraicExpression::Number(T::zero()),
            c: AlgebraicExpression::Number(T::zero()),
        }
    }
}

impl<T> AlgebraicExpression<T>
where
    T: FieldElement,
{
    pub fn build_plonkish(&self, row_offset: u64) -> Vec<PlonkishExpression<T>> {
        let mut row = row_offset;
        let mut plonkish_expr = Vec::<PlonkishExpression<T>>::new();
        let mut poly_id_offset = 1000;
        self.to_plonkish(&mut row, &mut plonkish_expr, &mut poly_id_offset);
        plonkish_expr
    }
    pub fn to_plonkish(
        &self,
        row_offset: &mut u64,
        plonkish_expr: &mut Vec<PlonkishExpression<T>>,
        poly_id_offset: &mut u64,
    ) -> AlgebraicExpression<T> {
        match self {
            AlgebraicExpression::Reference(ref r) => {
                AlgebraicExpression::Reference(AlgebraicReference {
                    name: r.name.clone(),
                    poly_id: r.poly_id,
                    next: r.next,
                })
            }
            AlgebraicExpression::PublicReference(ref s) => {
                AlgebraicExpression::PublicReference(s.clone())
            }
            AlgebraicExpression::Challenge(ref c) => AlgebraicExpression::Challenge(c.clone()),
            AlgebraicExpression::Number(ref n) => AlgebraicExpression::Number(*n),
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                match op {
                    AlgebraicBinaryOperator::Add => match (&**left, &**right) {
                        (
                            AlgebraicExpression::Reference(ref left),
                            AlgebraicExpression::Reference(ref right),
                        ) => Self::op_left_poly_right_poly(
                            op,
                            left,
                            right,
                            row_offset,
                            plonkish_expr,
                            poly_id_offset,
                        ),
                        (AlgebraicExpression::Reference(ref left), _) => {
                            Self::op_left_poly_right_recursive(
                                op,
                                left,
                                right,
                                row_offset,
                                plonkish_expr,
                                poly_id_offset,
                                false,
                            )
                        }
                        (_, AlgebraicExpression::Reference(ref right)) => {
                            Self::op_left_poly_right_recursive(
                                op,
                                right,
                                left,
                                row_offset,
                                plonkish_expr,
                                poly_id_offset,
                                false,
                            )
                        }
                        (_, _) => Self::op_left_recursive_right_recursive(
                            op,
                            left,
                            right,
                            row_offset,
                            plonkish_expr,
                            poly_id_offset,
                        ),
                    },
                    AlgebraicBinaryOperator::Sub => match (&**left, &**right) {
                        (
                            AlgebraicExpression::Reference(ref left),
                            AlgebraicExpression::Reference(ref right),
                        ) => Self::op_left_poly_right_poly(
                            op,
                            left,
                            right,
                            row_offset,
                            plonkish_expr,
                            poly_id_offset,
                        ),
                        (AlgebraicExpression::Reference(ref left), _) => {
                            Self::op_left_poly_right_recursive(
                                op,
                                left,
                                right,
                                row_offset,
                                plonkish_expr,
                                poly_id_offset,
                                false,
                            )
                        }
                        (_, AlgebraicExpression::Reference(ref right)) => {
                            Self::op_left_poly_right_recursive(
                                op,
                                right,
                                left,
                                row_offset,
                                plonkish_expr,
                                poly_id_offset,
                                true,
                            )
                        }
                        (_, _) => Self::op_left_recursive_right_recursive(
                            op,
                            left,
                            right,
                            row_offset,
                            plonkish_expr,
                            poly_id_offset,
                        ),
                    },

                    // first step, put every multiplication gate into a plonkish expression
                    // TODO: this should be into const*a + const*b + const*ab + const*c
                    // when left op right is (poly * poly), a new constraint is built
                    AlgebraicBinaryOperator::Mul => match (&**left, &**right) {
                        (
                            AlgebraicExpression::Reference(ref left),
                            AlgebraicExpression::Reference(ref right),
                        ) => Self::op_left_poly_right_poly(
                            op,
                            left,
                            right,
                            row_offset,
                            plonkish_expr,
                            poly_id_offset,
                        ),
                        (_, AlgebraicExpression::Reference(right)) => {
                            Self::op_left_poly_right_recursive(
                                op,
                                right,
                                left,
                                row_offset,
                                plonkish_expr,
                                poly_id_offset,
                                false,
                            )
                        }
                        (AlgebraicExpression::Reference(left), _) => {
                            Self::op_left_poly_right_recursive(
                                op,
                                left,
                                right,
                                row_offset,
                                plonkish_expr,
                                poly_id_offset,
                                false,
                            )
                        }
                        (_, _) => {
                            Self::op_left_recursive_right_recursive(
                                op,
                                left,
                                right,
                                row_offset,
                                plonkish_expr,
                                poly_id_offset,
                            )
                        }
                    },
                    AlgebraicBinaryOperator::Pow => {
                        unimplemented!("Power operation is not supported in plonkish yet.");
                    }
                }
            }
            _ => {
                unimplemented!("unknown operation")
            } 
        }
    }

    // for each op, there will be following cases:
    // mul:
    //    poly * poly,
    //    poly * recursive,   this include poly * number
    //    recurseive * poly,
    //    recursive * recursive,

    // sub:
    //    poly - poly,
    //    poly - recursive,   this include poly - number
    //    recursive - poly,
    //    recursive - recursive
    //    (optimize: poly - number or number-poly: this case just edit the last row_expression's qconst)

    pub fn op_left_poly_right_poly(
        op: &AlgebraicBinaryOperator,
        left: &AlgebraicReference,
        right: &AlgebraicReference,
        row_offset: &mut u64,
        plonkish_expr: &mut Vec<PlonkishExpression<T>>,
        poly_id_offset: &mut u64,
    ) -> AlgebraicExpression<T> {
        let mut row_expression = PlonkishExpression::default();
        row_expression.row = *row_offset;

        match *op {
            AlgebraicBinaryOperator::Add => {
                row_expression.ql = AlgebraicExpression::Number(T::one());
                row_expression.qr = AlgebraicExpression::Number(T::one());
                row_expression.qo = AlgebraicExpression::Number(T::one());
                row_expression.a = AlgebraicExpression::Reference(AlgebraicReference {
                    name: left.name.clone(),
                    poly_id: left.poly_id,
                    next: left.next,
                });
                row_expression.b = AlgebraicExpression::Reference(AlgebraicReference {
                    name: right.name.clone(),
                    poly_id: right.poly_id,
                    next: right.next,
                });
                row_expression.c = AlgebraicExpression::Reference(AlgebraicReference {
                    name: format!("temp_{}", *row_offset),
                    poly_id: PolyID {
                        id: *poly_id_offset,
                        ptype: PolynomialType::Committed,
                    },
                    next: false,
                });
            }
            AlgebraicBinaryOperator::Sub => {
                row_expression.ql = AlgebraicExpression::Number(T::one());
                row_expression.qr = AlgebraicExpression::Number(-T::one());
                row_expression.qo = AlgebraicExpression::Number(T::one());
                row_expression.a = AlgebraicExpression::Reference(AlgebraicReference {
                    name: left.name.clone(),
                    poly_id: left.poly_id,
                    next: left.next,
                });
                row_expression.b = AlgebraicExpression::Reference(AlgebraicReference {
                    name: right.name.clone(),
                    poly_id: right.poly_id,
                    next: right.next,
                });
                row_expression.c = AlgebraicExpression::Reference(AlgebraicReference {
                    name: format!("temp_{}", *row_offset),
                    poly_id: PolyID {
                        id: *poly_id_offset,
                        ptype: PolynomialType::Committed,
                    },
                    next: false,
                });
            }
            AlgebraicBinaryOperator::Mul => {
                row_expression.qmul = AlgebraicExpression::Number(T::one());
                row_expression.qo = AlgebraicExpression::Number(T::one());
                row_expression.a = AlgebraicExpression::Reference(AlgebraicReference {
                    name: left.name.clone(),
                    poly_id: left.poly_id,
                    next: left.next,
                });
                row_expression.b = AlgebraicExpression::Reference(AlgebraicReference {
                    name: right.name.clone(),
                    poly_id: right.poly_id,
                    next: right.next,
                });
                row_expression.c = AlgebraicExpression::Reference(AlgebraicReference {
                    name: format!("temp_{}", *row_offset),
                    poly_id: PolyID {
                        id: *poly_id_offset,
                        ptype: PolynomialType::Committed,
                    },
                    next: false,
                });
            }
            _ => {
                panic!("left poly right poly not implemented for op {:?}", op)
            }
        }

        plonkish_expr.push(row_expression.clone());
        *row_offset += 1;
        row_expression.c.clone()
    }

    pub fn op_left_poly_right_recursive(
        op: &AlgebraicBinaryOperator,
        left: &AlgebraicReference,
        right: &AlgebraicExpression<T>,
        row_offset: &mut u64,
        plonkish_expr: &mut Vec<PlonkishExpression<T>>,
        poly_id_offset: &mut u64,
        left_right_flip: bool,
    ) -> AlgebraicExpression<T> {
        let mut row_expression = PlonkishExpression::default();
        row_expression.row = *row_offset;

        match *op {
            AlgebraicBinaryOperator::Add => {
                row_expression.ql = AlgebraicExpression::Number(T::one());
                row_expression.qr = AlgebraicExpression::Number(T::one());
                row_expression.qo = AlgebraicExpression::Number(T::one());
                row_expression.a = AlgebraicExpression::Reference(AlgebraicReference {
                    name: left.name.clone(),
                    poly_id: left.poly_id,
                    next: left.next,
                });
                row_expression.b = right.to_plonkish(row_offset, plonkish_expr, poly_id_offset);

                row_expression.c = AlgebraicExpression::Reference(AlgebraicReference {
                    name: format!("temp_{}", *row_offset),
                    poly_id: PolyID {
                        id: *poly_id_offset,
                        ptype: PolynomialType::Committed,
                    },
                    next: false,
                });
            }
            AlgebraicBinaryOperator::Sub => {
                if left_right_flip {
                    row_expression.ql = AlgebraicExpression::Number(-T::one());
                    row_expression.qr = AlgebraicExpression::Number(T::one());
                } else {
                    row_expression.ql = AlgebraicExpression::Number(T::one());
                    row_expression.qr = AlgebraicExpression::Number(-T::one());
                }

                row_expression.qo = AlgebraicExpression::Number(T::one());
                row_expression.a = AlgebraicExpression::Reference(AlgebraicReference {
                    name: left.name.clone(),
                    poly_id: left.poly_id,
                    next: left.next,
                });
                row_expression.b = right.to_plonkish(row_offset, plonkish_expr, poly_id_offset);
                row_expression.c = AlgebraicExpression::Reference(AlgebraicReference {
                    name: format!("temp_{}", *row_offset),
                    poly_id: PolyID {
                        id: *poly_id_offset,
                        ptype: PolynomialType::Committed,
                    },
                    next: false,
                });
            }
            AlgebraicBinaryOperator::Mul => {
                row_expression.qmul = AlgebraicExpression::Number(T::one());
                row_expression.qo = AlgebraicExpression::Number(T::one());
                row_expression.a = AlgebraicExpression::Reference(AlgebraicReference {
                    name: left.name.clone(),
                    poly_id: left.poly_id,
                    next: left.next,
                });
                row_expression.b = right.to_plonkish(row_offset, plonkish_expr, poly_id_offset);

                row_expression.c = AlgebraicExpression::Reference(AlgebraicReference {
                    name: format!("temp_{}", *row_offset),
                    poly_id: PolyID {
                        id: *poly_id_offset,
                        ptype: PolynomialType::Committed,
                    },
                    next: false,
                });
            }
            _ => {
                panic!("left poly right poly not implemented for op {:?}", op)
            }
        }

        plonkish_expr.push(row_expression.clone());
        *row_offset += 1;
        row_expression.c.clone()
    }

    pub fn op_left_recursive_right_recursive(
        op: &AlgebraicBinaryOperator,
        left: &AlgebraicExpression<T>,
        right: &AlgebraicExpression<T>,
        row_offset: &mut u64,
        plonkish_expr: &mut Vec<PlonkishExpression<T>>,
        poly_id_offset: &mut u64,
    ) -> AlgebraicExpression<T> {
        let mut row_expression = PlonkishExpression::default();
        row_expression.row = *row_offset;

        match *op {
            AlgebraicBinaryOperator::Add => {
                row_expression.ql = AlgebraicExpression::Number(T::one());
                row_expression.qr = AlgebraicExpression::Number(T::one());
                row_expression.qo = AlgebraicExpression::Number(T::one());
                row_expression.a = left.to_plonkish(row_offset, plonkish_expr, poly_id_offset);
                row_expression.b = right.to_plonkish(row_offset, plonkish_expr, poly_id_offset);

                row_expression.c = AlgebraicExpression::Reference(AlgebraicReference {
                    name: format!("temp_{}", *row_offset),
                    poly_id: PolyID {
                        id: *poly_id_offset,
                        ptype: PolynomialType::Committed,
                    },
                    next: false,
                });
            }
            AlgebraicBinaryOperator::Sub => {
                row_expression.ql = AlgebraicExpression::Number(-T::one());
                row_expression.qr = AlgebraicExpression::Number(T::one());

                row_expression.qo = AlgebraicExpression::Number(T::one());
                row_expression.a = left.to_plonkish(row_offset, plonkish_expr, poly_id_offset);
                row_expression.b = right.to_plonkish(row_offset, plonkish_expr, poly_id_offset);
                row_expression.c = AlgebraicExpression::Reference(AlgebraicReference {
                    name: format!("temp_{}", *row_offset),
                    poly_id: PolyID {
                        id: *poly_id_offset,
                        ptype: PolynomialType::Committed,
                    },
                    next: false,
                });
            }
            AlgebraicBinaryOperator::Mul => {
                row_expression.qmul = AlgebraicExpression::Number(T::one());
                row_expression.qo = AlgebraicExpression::Number(-T::one());
                row_expression.a = left.to_plonkish(row_offset, plonkish_expr, poly_id_offset);
                row_expression.b = right.to_plonkish(row_offset, plonkish_expr, poly_id_offset);

                row_expression.c = AlgebraicExpression::Reference(AlgebraicReference {
                    name: format!("temp_{}", *row_offset),
                    poly_id: PolyID {
                        id: *poly_id_offset,
                        ptype: PolynomialType::Committed,
                    },
                    next: false,
                });
            }
            _ => {
                panic!(
                    "left recursive right recursive not implemented for op {:?}",
                    op
                )
            }
        }

        plonkish_expr.push(row_expression.clone());
        *row_offset += 1;
        row_expression.c.clone()
    }
}
