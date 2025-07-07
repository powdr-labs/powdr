#![allow(non_snake_case)]
use crate::pairing_check_with_hint::{
    BN254_PSEUDO_BINARY_ENCODING, PairingCheckError, frobenius_coeff_fq6_c1, xi_to_q_minus_1_over_2,
};
use ark_bn254::{Fq, Fq2, Fq6, Fq6Config, Fq12, G1Affine, G2Affine};
use ark_ff::{AdditiveGroup, Field};
use ark_ff::Fp6Config;
use core::iter::zip;
use core::ops::Neg;

/// The expected output of this function when running the Miller loop with embedded exponent is
/// c^2 * l_{2Q}
pub fn pre_loop(
    Q_acc: Vec<G2Affine>,
    _Q: &[G2Affine],
    c: Option<Fq12>,
    xy_fracs: &[(Fq, Fq)],
) -> (Fq12, Vec<G2Affine>) {
    let mut f = if let Some(mut c) = c {
        // for the miller loop with embedded exponent, f will be set to c at the beginning of
        // the function, and we will square c due to the last two values of the
        // pseudo-binary encoding (BN254_PSEUDO_BINARY_ENCODING) being 0 and 1.
        // Therefore, the final value of f at the end of this block is c^2.
        c.square_in_place();
        c
    } else {
        Fq12::ONE
    };

    let mut Q_acc = Q_acc;
    let mut initial_lines = Vec::<EvaluatedLine<Fq2>>::new();

    // We don't need to special case the first iteration for Bn254, but since we are using the
    // same Miller loop implementation for both Bn254 and Bls12_381, we need to do the
    // first iteration separately here.
    let (Q_out_double, lines_2S) = Q_acc
        .into_iter()
        .map(|Q| miller_double_step(&Q))
        .unzip::<_, _, Vec<_>, Vec<_>>();
    Q_acc = Q_out_double;

    let lines_iter = lines_2S.iter().zip(xy_fracs.iter());
    for (line_2S, xy_frac) in lines_iter {
        let line = line_2S.evaluate(xy_frac);
        initial_lines.push(line);
    }

    f = evaluate_lines_vec(f, initial_lines);

    (f, Q_acc)
}

/// The output of a line function on Fp12 x Fp12 (see `UnevaluatedLine`).
/// Represents 1 + b w' + c w'^3 where w' = w for D-type and w' = w^-1 for M-type twists.
#[derive(Clone, Copy, Debug)]
pub struct EvaluatedLine<Fq2> {
    pub b: Fq2,
    pub c: Fq2,
}

impl UnevaluatedLine<Fq2> {
    pub fn evaluate(&self, xy_frac: &(Fq, Fq)) -> EvaluatedLine<Fq2> {
        let (x_over_y, y_inv) = xy_frac;
        // Represents the line L(x,y) = 1 + b (x/y) w^1 + c (1/y) w^3
        EvaluatedLine {
            b: self.b.mul_by_base_prime_field(x_over_y),
            c: self.c.mul_by_base_prime_field(y_inv),
        }
    }
}

pub struct UnevaluatedLine<Fq2> {
    pub b: Fq2,
    pub c: Fq2,
}

/// Miller double step.
/// Returns 2S and a line in Fp12 tangent to \Psi(S).
/// Assumptions:
///     - s is not point at infinity.
///     - a in the curve equation is 0.
/// The case y = 0 does not happen as long as the curve satisfies that 0 = X^3 + b has no
/// solutions in Fp2. The curve G1Affine and twist G2Affine are both chosen for bn254,
/// bls12_381 so that this never happens.
pub fn miller_double_step(s: &G2Affine) -> (G2Affine, UnevaluatedLine<Fq2>) {
    let two: Fq2 = Fq2::from(2u64);
    let three: Fq2 = Fq2::from(3u64);

    let x = &s.x;
    let y = &s.y;
    // λ = (3x^2) / (2y)
    let lambda = &((three * x.square()) * (&(two * y).inverse().unwrap()));
    // x_2s = λ^2 - 2x
    let x_2s = lambda * lambda - two * x;
    // y_2s = λ(x - x_2s) - y
    let y_2s = lambda * &(x - &x_2s) - y;
    let two_s = G2Affine::new(x_2s, y_2s);

    // l_{\Psi(S),\Psi(S)}(P)
    let b = Fq2::ZERO - lambda;
    let c = lambda * x - y;

    (two_s, UnevaluatedLine { b, c })
}
pub fn evaluate_lines_vec(f: Fq12, lines: Vec<EvaluatedLine<Fq2>>) -> Fq12 {
    let mut f = f;
    let mut lines = lines;
    if lines.len() % 2 == 1 {
        f = mul_by_013(&f, &lines.pop().unwrap());
    }
    for chunk in lines.chunks(2) {
        if let [line0, line1] = chunk {
            let prod = mul_013_by_013(line0, line1);
            f = mul_by_01234(&f, &prod);
        } else {
            panic!("lines.len() % 2 should be 0 at this point");
        }
    }
    f
}

/// Multiplies a line in 013-form with a Fp12 element to get an Fp12 element
fn mul_by_013(f: &Fq12, l: &EvaluatedLine<Fq2>) -> Fq12 {
    from_evaluated_line_d_type(l.clone()) * f
}
fn mul_013_by_013(l0: &EvaluatedLine<Fq2>, l1: &EvaluatedLine<Fq2>) -> [Fq2; 5] {
    let b0 = &l0.b;
    let c0 = &l0.c;
    let b1 = &l1.b;
    let c1 = &l1.c;

    // where w⁶ = xi
    // l0 * l1 = 1 + (b0 + b1)w + (b0b1)w² + (c0 + c1)w³ + (b0c1 + b1c0)w⁴ + (c0c1)w⁶
    //         = (1 + c0c1 * xi) + (b0 + b1)w + (b0b1)w² + (c0 + c1)w³ + (b0c1 + b1c0)w⁴
    let x0 = Fq2::ONE + c0 * c1 * &Fq6Config::NONRESIDUE;
    let x1 = b0 + b1;
    let x2 = b0 * b1;
    let x3 = c0 + c1;
    let x4 = b0 * c1 + b1 * c0;

    [x0, x1, x2, x3, x4]
}
fn from_evaluated_line_d_type(line: EvaluatedLine<Fq2>) -> Fq12 {
    Fq12::new(
        Fq6::new(Fq2::ONE, line.b, Fq2::ZERO),
        Fq6::new(line.c, Fq2::ZERO, Fq2::ZERO),
    )
}

/// Multiplies a line in 01234-form with a Fp12 element to get an Fp12 element
fn mul_by_01234(f: &Fq12, x: &[Fq2; 5]) -> Fq12 {
    let fx = Fq12::new(Fq6::new(x[0], x[2], x[4]), Fq6::new(x[1], x[3], Fq2::ZERO));
    f * fx
}

/// Miller double and add step (2S + Q implemented as S + Q + S for efficiency).
/// Returns 2S+Q, a line in Fp12 passing through S and Q, and a line in Fp12 passing through S+Q
/// and S Assumption: Q != +- S && (S+Q) != +-S, so that there is no division by zero.
/// The way this is used in miller loop, this is always satisfied.
pub fn miller_double_and_add_step(
    s: &G2Affine,
    q: &G2Affine,
) -> (G2Affine, UnevaluatedLine<Fq2>, UnevaluatedLine<Fq2>) {
    let two: Fq2 = Fq2::from(2u64);

    let x_s = &s.x;
    let y_s = &s.y;
    let x_q = &q.x;
    let y_q = &q.y;

    // λ1 = (y_s - y_q) / (x_s - x_q)
    let lambda1 = &((y_s - y_q) * (&(x_s - x_q).inverse().unwrap()));
    let x_s_plus_q = lambda1 * lambda1 - x_s - x_q;

    // λ2 = -λ1 - 2y_s / (x_{s+q} - x_s)
    let lambda2 =
        &(Fq2::ZERO - lambda1.clone() - (two * y_s) * (&(&x_s_plus_q - x_s).inverse().unwrap()));
    let x_s_plus_q_plus_s = lambda2 * lambda2 - x_s - &x_s_plus_q;
    let y_s_plus_q_plus_s = lambda2 * &(x_s - &x_s_plus_q_plus_s) - y_s;

    let s_plus_q_plus_s = G2Affine::new(x_s_plus_q_plus_s, y_s_plus_q_plus_s);

    // l_{\Psi(S),\Psi(Q)}(P)
    let b0 = Fq2::ZERO - lambda1;
    let c0 = lambda1 * x_s - y_s;

    // l_{\Psi(S+Q),\Psi(S)}(P)
    let b1 = Fq2::ZERO - lambda2;
    let c1 = lambda2 * x_s - y_s;

    (
        s_plus_q_plus_s,
        UnevaluatedLine { b: b0, c: c0 },
        UnevaluatedLine { b: b1, c: c1 },
    )
}

/// Runs the multi-Miller loop with an embedded exponent, removing the need to calculate the
/// residue witness in the final exponentiation step
///
/// `c` is assumed nonzero.
pub fn multi_miller_loop_embedded_exp(P: &[G1Affine], Q: &[G2Affine], c: Option<Fq12>) -> Fq12 {
    assert!(!P.is_empty());
    assert_eq!(P.len(), Q.len());

    // Filter out the pair with infinity points
    let (P, Q): (Vec<_>, Vec<_>) = zip(P, Q)
        .filter(|(p, q)| !p.infinity && !q.infinity)
        .map(|(p, q)| (p.clone(), q.clone()))
        .unzip();

    let xy_fracs = P
        .iter()
        .map(|P| ((&P.x) * (&P.y.inverse().unwrap()), P.y.inverse().unwrap()))
        .collect::<Vec<(Fq, Fq)>>();
    let c_inv = if let Some(c) = c.as_ref() {
        c.inverse().unwrap()
    } else {
        Fq12::ONE
    };

    let mut Q_acc = Q.to_vec();

    let (f_out, Q_acc_out) = pre_loop(Q_acc, &Q, c.clone(), &xy_fracs);

    let mut f = f_out;
    Q_acc = Q_acc_out;

    for i in (0..BN254_PSEUDO_BINARY_ENCODING.len() - 2).rev() {
        f.square_in_place();

        let mut lines = Vec::with_capacity(xy_fracs.len());

        if BN254_PSEUDO_BINARY_ENCODING[i] == 0 {
            // Run miller double step if \sigma_i == 0
            // OPT[jpw]: Q_acc could be mutated in-place for better memory allocation
            let (Q_out, lines_2S) = Q_acc
                .iter()
                .map(miller_double_step)
                .unzip::<_, _, Vec<_>, Vec<_>>();
            Q_acc = Q_out;

            let lines_iter = lines_2S.iter().zip(xy_fracs.iter());
            for (line_2S, xy_frac) in lines_iter {
                let line = line_2S.evaluate(xy_frac);
                lines.push(line);
            }
        } else {
            // use embedded exponent technique if c is provided
            f = if let Some(c) = c.as_ref() {
                match BN254_PSEUDO_BINARY_ENCODING[i] {
                    1 => &f * c,
                    -1 => &f * &c_inv,
                    _ => panic!("Invalid sigma_i"),
                }
            } else {
                f
            };

            // Run miller double and add if \sigma_i != 0
            // OPT[jpw]: Q_acc could be mutated in-place for better memory allocation
            let mut Q_out = Vec::new();
            let mut lines_S_plus_Q = Vec::new();
            let mut lines_S_plus_Q_plus_S = Vec::new();

            for (Q_acc, q) in Q_acc.iter().zip(&Q) {
                let q_signed = match BN254_PSEUDO_BINARY_ENCODING[i] {
                    1 => q,
                    -1 => &q.neg(),
                    _ => panic!("Invalid sigma_i"),
                };

                let (q_res, line1, line2) = miller_double_and_add_step(Q_acc, q_signed);
                Q_out.push(q_res);
                lines_S_plus_Q.push(line1);
                lines_S_plus_Q_plus_S.push(line2);
            }
            Q_acc = Q_out;

            let lines_iter = lines_S_plus_Q
                .iter()
                .zip(lines_S_plus_Q_plus_S.iter())
                .zip(xy_fracs.iter())
                .map(|((a, b), c)| (a, b, c));
            for (line_S_plus_Q, line_S_plus_Q_plus_S, xy_frac) in lines_iter {
                let line0 = line_S_plus_Q.evaluate(xy_frac);
                let line1 = line_S_plus_Q_plus_S.evaluate(xy_frac);
                lines.push(line0);
                lines.push(line1);
            }
        };

        f = evaluate_lines_vec(f, lines);
    }

    let (f_out, _) = post_loop(&f, Q_acc.clone(), &Q, c, &xy_fracs);
    f = f_out;

    f
}

// Square and multiply implementation of final exponentiation. Used if the hint fails to prove
// the pairing check.
// `exp` should be big-endian.
pub fn exp_check_fallback<F: Field>(f: &F, exp: &[u8]) -> Result<(), PairingCheckError>
where
    for<'a> &'a F: core::ops::Mul<&'a F, Output = F>,
{
    if exp_bytes(f, true, exp) == F::ONE {
        Ok(())
    } else {
        Err(PairingCheckError)
    }
}

fn exp_bytes<F: Field>(f: &F, is_positive: bool, bytes_be: &[u8]) -> F
where
    for<'a> &'a F: core::ops::Mul<&'a F, Output = F>,
{
    let mut x = f.clone();

    if !is_positive {
        x = x.inverse().unwrap();
    }

    let mut res = F::ONE;

    let x_sq = &x * &x;
    let ops = [x.clone(), x_sq.clone(), &x_sq * &x];

    for &b in bytes_be.iter() {
        let mut mask = 0xc0;
        for j in 0..4 {
            res = &res * &res * &res * &res;
            let c = (b & mask) >> (6 - 2 * j);
            if c != 0 {
                res *= &ops[(c - 1) as usize];
            }
            mask >>= 2;
        }
    }
    res
}

/// Compute f_{Miller,Q}(P) from f_{6x+2,Q}(P)
fn post_loop(
    f: &Fq12,
    Q_acc: Vec<G2Affine>, // at this point, Q_acc = (6x+2)Q
    Q: &[G2Affine],
    _c: Option<Fq12>,
    xy_fracs: &[(Fq, Fq)],
) -> (Fq12, Vec<G2Affine>) {
    let mut Q_acc = Q_acc;
    let mut lines = Vec::<EvaluatedLine<Fq2>>::new();

    //Todo: needs to correct these numbers
    let x_to_q_minus_1_over_3 = frobenius_coeff_fq6_c1()[1];
    let x_to_q_sq_minus_1_over_3 = frobenius_coeff_fq6_c1()[2];

    // For each q, compute q1 such that `frob_p(twist(q)) = twist(q1)`
    let q1_vec = Q
        .iter()
        .map(|Q| {
            let x = Q.x.frobenius_map(1);
            let x = x * x_to_q_minus_1_over_3;
            let y = Q.y.frobenius_map(1);
            let y = y * &xi_to_q_minus_1_over_2();
            G2Affine::new(x, y)
        })
        .collect::<Vec<_>>();
    // compute l_{(6x+2)\Psi(Q), \phi_p(\Psi(Q))} where \phi_p is the Frobenius map
    let (Q_out_add, lines_S_plus_Q) = Q_acc
        .iter()
        .zip(q1_vec.iter())
        .map(|(Q_acc, q1)| miller_add_step(Q_acc, q1))
        .unzip::<_, _, Vec<_>, Vec<_>>();
    Q_acc = Q_out_add;

    let lines_iter = lines_S_plus_Q.iter().zip(xy_fracs.iter());
    for (lines_S_plus_Q, xy_frac) in lines_iter {
        let line = lines_S_plus_Q.evaluate(xy_frac);
        lines.push(line);
    }

    // For each q, compute q2 such that `-frob_p^2(twist(q)) = twist(q2)`
    let q2_vec = Q
        .iter()
        .map(|Q| {
            // There is a frobenius mapping π²(Q) that we skip here since it is equivalent to
            // the identity mapping
            let x = &Q.x * x_to_q_sq_minus_1_over_3;
            G2Affine::new(x, Q.y.clone())
        })
        .collect::<Vec<_>>();

    // compute l_{(6x+2)\Psi(Q) + \phi_p(\Psi(Q)), -(\phi_p)^2(\Psi(Q))} where \phi_p is the
    // Frobenius map
    let (Q_out_add, lines_S_plus_Q) = Q_acc
        .iter()
        .zip(q2_vec.iter())
        .map(|(Q_acc, q2)| miller_add_step(Q_acc, q2))
        .unzip::<_, _, Vec<_>, Vec<_>>();
    Q_acc = Q_out_add;

    let lines_iter = lines_S_plus_Q.iter().zip(xy_fracs.iter());
    for (lines_S_plus_Q, xy_frac) in lines_iter {
        let line = lines_S_plus_Q.evaluate(xy_frac);
        lines.push(line);
    }

    let mut f = f.clone();
    f = evaluate_lines_vec(f, lines);

    (f, Q_acc)
}

/// Miller add step.
/// Returns S+Q and a line in Fp12 passing through \Psi(S) and \Psi(Q).
fn miller_add_step(s: &G2Affine, q: &G2Affine) -> (G2Affine, UnevaluatedLine<Fq2>) {
    let x_s = &s.x;
    let y_s = &s.y;
    let x_q = &q.x;
    let y_q = &q.y;

    // λ1 = (y_s - y_q) / (x_s - x_q)
    let x_delta = x_s - x_q;
    let lambda = &((y_s - y_q) * (&x_delta.inverse().unwrap()));
    let x_s_plus_q = lambda * lambda - x_s - x_q;
    let y_s_plus_q = lambda * &(x_q - &x_s_plus_q) - y_q;

    let s_plus_q = G2Affine::new(x_s_plus_q, y_s_plus_q);

    // l_{\Psi(S),\Psi(Q)}(P)
    let b = Fq2::ZERO - lambda;
    let c = lambda * x_s - y_s;

    (s_plus_q, UnevaluatedLine { b, c })
}
