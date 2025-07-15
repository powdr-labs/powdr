#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]

use hex_literal::hex;
use openvm_algebra_guest::IntMod;
use openvm_ecc_guest::{msm, weierstrass::WeierstrassPoint, Group};
use openvm_k256::{Secp256k1Coord, Secp256k1Point, Secp256k1Scalar};

openvm::init!();

openvm::entry!(main);

pub fn main() {
    // Sample points got from https://asecuritysite.com/ecc/ecc_points2 and
    // https://learnmeabitcoin.com/technical/cryptography/elliptic-curve/#add
    let x1 = Secp256k1Coord::from_u32(1);
    let y1 = Secp256k1Coord::from_le_bytes(&hex!(
        "EEA7767E580D75BC6FDD7F58D2A84C2614FB22586068DB63B346C6E60AF21842"
    ));
    let x2 = Secp256k1Coord::from_u32(2);
    let y2 = Secp256k1Coord::from_le_bytes(&hex!(
        "D1A847A8F879E0AEE32544DA5BA0B3BD1703A1F52867A5601FF6454DD8180499"
    ));
    // This is the sum of (x1, y1) and (x2, y2).
    let x3 = Secp256k1Coord::from_le_bytes(&hex!(
        "BE675E31F8AC8200CBCC6B10CECCD6EB93FB07D99BB9E7C99CC9245C862D3AF2"
    ));
    let y3 = Secp256k1Coord::from_le_bytes(&hex!(
        "B44573B48FD3416DD256A8C0E1BAD03E88A78BF176778682589B9CB478FC1D79"
    ));
    // This is the double of (x2, y2).
    let x4 = Secp256k1Coord::from_le_bytes(&hex!(
        "3BFFFFFF32333333333333333333333333333333333333333333333333333333"
    ));
    let y4 = Secp256k1Coord::from_le_bytes(&hex!(
        "AC54ECC4254A4EDCAB10CC557A9811ED1EF7CB8AFDC64820C6803D2C5F481639"
    ));

    let mut p1 = Secp256k1Point::from_xy(x1.clone(), y1.clone()).unwrap();
    let mut p2 = Secp256k1Point::from_xy(x2, y2).unwrap();

    // Generic add can handle equal or unequal points.
    let p3 = &p1 + &p2;
    assert_eq!(p3.x(), &x3);
    assert_eq!(p3.y(), &y3);
    
    let p4 = &p2 + &p2;
    assert_eq!(p4.x(), &x4);
    assert_eq!(p4.y(), &y4);

    // Add assign and double assign
    p1 += &p2;
    assert_eq!(p1.x(), &x3);
    assert_eq!(p1.y(), &y3);
    
    p2.double_assign();
    assert_eq!(p2.x(), &x4);
    assert_eq!(p2.y(), &y4);


    // Ec Mul
    let p1 = Secp256k1Point::from_xy(x1, y1).unwrap();
    let scalar = Secp256k1Scalar::from_u32(12345678);
    // Calculated with https://learnmeabitcoin.com/technical/cryptography/elliptic-curve/#ec-multiply-tool
    let x5 = Secp256k1Coord::from_le_bytes(&hex!(
        "194A93387F790803D972AF9C4A40CB89D106A36F58EE2F31DC48A41768216D6D"
    ));
    let y5 = Secp256k1Coord::from_le_bytes(&hex!(
        "9E272F746DA7BED171E522610212B6AEEAAFDB2AD9F4B530B8E1B27293B19B2C"
    ));
    let result = msm(&[scalar], &[p1]);

    assert_eq!(result.x(), &x5);
    assert_eq!(result.y(), &y5);
}