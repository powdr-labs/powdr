#![allow(clippy::needless_range_loop)]
use core::array;

use openvm_ruint::aliases::U256;

openvm::entry!(main);

const N: usize = 32;
type Matrix = [[U256; N]; N];

pub fn get_matrix(val: u32) -> Matrix {
    array::from_fn(|_| array::from_fn(|_| U256::from(val)))
}

pub fn mult(a: &Matrix, b: &Matrix) -> Matrix {
    let mut c = get_matrix(0);
    for i in 0..N {
        for j in 0..N {
            for k in 0..N {
                c[i][j] += a[i][k] * b[k][j];
            }
        }
    }
    c
}

pub fn get_identity_matrix() -> Matrix {
    let mut res = get_matrix(0);
    for i in 0..N {
        res[i][i] = U256::from(1u32);
    }
    res
}

pub fn main() {
    let a: Matrix = get_identity_matrix();
    let b: Matrix = get_matrix(28);
    let c: Matrix = mult(&a, &b);
    if c != b {
        panic!("Matrix multiplication failed");
    }
}
