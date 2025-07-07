use crate::pairing_utils::{exp_check_fallback, multi_miller_loop_embedded_exp};
use ark_bn254::{Bn254, Fq, Fq2, Fq6, Fq12, G1Affine, G2Affine};
use ark_ec::pairing::Pairing;
use ark_ff::{AdditiveGroup, Field, PrimeField};
use hex_literal::hex;
use num_bigint::BigUint;
use num_traits::Num;

pub fn pairing_check(P: &[G1Affine], Q: &[G2Affine]) -> Result<(), PairingCheckError> {
    try_honest_pairing_check(P, Q).unwrap_or_else(|| {
        let f = Bn254::multi_miller_loop(P, Q);
        exp_check_fallback(&f.0, &FINAL_EXPONENT)
    })
}

fn try_honest_pairing_check(
    P: &[G1Affine],
    Q: &[G2Affine],
) -> Option<Result<(), PairingCheckError>> {
    let fq12 = multi_miller_loop_embedded_exp(P, Q, None);
    let (c, u) = final_exp_hint(&fq12);

    if c == Fq12::ZERO {
        return None;
    }
    let c_inv = c.inverse().unwrap();

    // We follow Theorem 3 of https://eprint.iacr.org/2024/640.pdf to check that the pairing equals 1
    // By the theorem, it suffices to provide c and u such that f * u == c^λ.
    // Since λ = 6x + 2 + q^3 - q^2 + q, we will check the equivalent condition:
    // f * c^-{6x + 2} * u * c^-{q^3 - q^2 + q} == 1
    // This is because we can compute f * c^-{6x+2} by embedding the c^-{6x+2} computation in
    // the miller loop.

    // c_mul = c^-{q^3 - q^2 + q}
    let c_q3_inv = c_inv.clone().frobenius_map(3);
    let c_q2 = c.clone().frobenius_map(2);
    let c_q_inv = c_inv.clone().frobenius_map(1);
    let c_mul = c_q3_inv * c_q2 * c_q_inv;

    // Pass c inverse into the miller loop so that we compute fc == f * c^-{6x + 2}
    let fc = multi_miller_loop_embedded_exp(P, Q, Some(c_inv));

    if fc * c_mul * u == Fq12::ONE {
        print!("Pairing check passed with hint");
        Some(Ok(()))
    } else {
        None
    }
}

fn final_exp_hint(f: &Fq12) -> (Fq12, Fq12) {
    // Residue witness
    let mut c;
    // Cubic nonresidue power
    let u;

    let u_coeffs = Fq2::from_base_prime_field_elems([u27_coeff_0(), u27_coeff_1()]);

    let fq2_zero = Fq2::ZERO;
    let fq6 = Fq6::new(fq2_zero, u_coeffs.unwrap(), fq2_zero);
    let unity_root_27 = Fq12::new(fq6, Fq6::ZERO);
    debug_assert_eq!(unity_root_27.pow([27]), Fq12::ONE);

    if f.pow(exp1().to_u64_digits()) == Fq12::ONE {
        c = *f;
        u = Fq12::ONE;
    } else {
        let f_mul_unity_root_27 = f * unity_root_27;
        if f_mul_unity_root_27.pow(exp1().to_u64_digits()) == Fq12::ONE {
            c = f_mul_unity_root_27;
            u = unity_root_27;
        } else {
            c = f_mul_unity_root_27 * unity_root_27;
            u = unity_root_27.square();
        }
    }

    // 1. Compute r-th root and exponentiate to rInv where
    //   rInv = 1/r mod (p^12-1)/r
    c = c.pow(r_inv().to_u64_digits());

    // 2. Compute m-th root where
    //   m = (6x + 2 + q^3 - q^2 +q)/3r
    // Exponentiate to mInv where
    //   mInv = 1/m mod p^12-1
    c = c.pow(m_inv().to_u64_digits());

    // 3. Compute cube root
    // since gcd(3, (p^12-1)/r) != 1, we use a modified Tonelli-Shanks algorithm
    // see Alg.4 of https://eprint.iacr.org/2024/640.pdf
    // Typo in the paper: p^k-1 = 3^n * s instead of p-1 = 3^r * s
    // where k=12 and n=3 here and exp2 = (s+1)/3
    let mut x = c.pow(exp2().to_u64_digits());

    // 3^t is ord(x^3 / residueWitness)
    let c_inv = c.inverse().unwrap();
    let mut x3 = x.square() * x * c_inv;
    let mut t = 0;
    let mut tmp = x3.square();

    // Modified Tonelli-Shanks algorithm for computing the cube root
    fn tonelli_shanks_loop(x3: &mut Fq12, tmp: &mut Fq12, t: &mut i32) {
        while *x3 != Fq12::ONE {
            *tmp = (*x3).square();
            *x3 *= *tmp;
            *t += 1;
        }
    }

    tonelli_shanks_loop(&mut x3, &mut tmp, &mut t);

    while t != 0 {
        tmp = unity_root_27.pow(exp2().to_u64_digits());
        x *= tmp;

        x3 = x.square() * x * c_inv;
        t = 0;
        tonelli_shanks_loop(&mut x3, &mut tmp, &mut t);
    }

    debug_assert_eq!(c, x * x * x);
    // x is the cube root of the residue witness c
    c = x;

    (c, u)
}

pub fn u27_coeff_0() -> Fq {
    Fq::from_be_bytes_mod_order(&hex!(
        "14f790bbd583653f862867ef12a24eb7992478d2aeef5cdebf9094793132b563"
    ))
}

pub fn u27_coeff_1() -> Fq {
    Fq::from_be_bytes_mod_order(&hex!(
        "0a063e5502b196f64f84e31d49aece1f44c8873b8927d16d4c0093fe44aacc65"
    ))
}

pub fn exp1() -> BigUint {
    BigUint::from_str_radix(
        "4030969696062745741797811005853058291874379204406359442560681893891674450106959530046539719647151210908190211459382793062006703141168852426020468083171325367934590379984666859998399967609544754664110191464072930598755441160008826659219834762354786403012110463250131961575955268597858015384895449311534622125256548620283853223733396368939858981844663598065852816056384933498610930035891058807598891752166582271931875150099691598048016175399382213304673796601585080509443902692818733420199004555566113537482054218823936116647313678747500267068559627206777530424029211671772692598157901876223857571299238046741502089890557442500582300718504160740314926185458079985126192563953772118929726791041828902047546977272656240744693339962973939047279285351052107950250121751682659529260304162131862468322644288196213423232132152125277136333208005221619443705106431645884840489295409272576227859206166894626854018093044908314720",
        10
    ).unwrap()
}

pub fn exp2() -> BigUint {
    BigUint::from_str_radix(
        "149295173928249842288807815031594751550902933496531831205951181255247201855813315927649619246190785589192230054051214557852100116339587126889646966043382421034614458517950624444385183985538694617189266350521219651805757080000326913304438324531658755667115202342597480058368713651772519088329461085612393412046538837788290860138273939590365147475728281409846400594680923462911515927255224400281440435265428973034513894448136725853630228718495637529802733207466114092942366766400693830377740909465411612499335341437923559875826432546203713595131838044695464089778859691547136762894737106526809539677749557286722299625576201574095640767352005953344997266128077036486155280146436004404804695964512181557316554713802082990544197776406442186936269827816744738898152657469728130713344598597476387715653492155415311971560450078713968012341037230430349766855793764662401499603533676762082513303932107208402000670112774382027",
        10
    ).unwrap()
}

pub fn r_inv() -> BigUint {
    BigUint::from_str_radix(
        "495819184011867778744231927046742333492451180917315223017345540833046880485481720031136878341141903241966521818658471092566752321606779256340158678675679238405722886654128392203338228575623261160538734808887996935946888297414610216445334190959815200956855428635568184508263913274453942864817234480763055154719338281461936129150171789463489422401982681230261920147923652438266934726901346095892093443898852488218812468761027620988447655860644584419583586883569984588067403598284748297179498734419889699245081714359110559679136004228878808158639412436468707589339209058958785568729925402190575720856279605832146553573981587948304340677613460685405477047119496887534881410757668344088436651291444274840864486870663164657544390995506448087189408281061890434467956047582679858345583941396130713046072603335601764495918026585155498301896749919393",
        10
    ).unwrap()
}

pub fn m_inv() -> BigUint {
    BigUint::from_str_radix(
        "17840267520054779749190587238017784600702972825655245554504342129614427201836516118803396948809179149954197175783449826546445899524065131269177708416982407215963288737761615699967145070776364294542559324079147363363059480104341231360692143673915822421222230661528586799190306058519400019024762424366780736540525310403098758015600523609594113357130678138304964034267260758692953579514899054295817541844330584721967571697039986079722203518034173581264955381924826388858518077894154909963532054519350571947910625755075099598588672669612434444513251495355121627496067454526862754597351094345783576387352673894873931328099247263766690688395096280633426669535619271711975898132416216382905928886703963310231865346128293216316379527200971959980873989485521004596686352787540034457467115536116148612884807380187255514888720048664139404687086409399",
        10
    ).unwrap()
}

// Encodes 6x+2 where x is the BN254 seed.
// 6*x+2 = sum_i BN254_PSEUDO_BINARY_ENCODING[i] * 2^i
// where BN254_PSEUDO_BINARY_ENCODING[i] is in {-1, 0, 1}
// Validated against BN254_SEED_ABS by a test in tests.rs
pub const BN254_PSEUDO_BINARY_ENCODING: [i8; 66] = [
    0, 0, 0, 1, 0, 1, 0, -1, 0, 0, -1, 0, 0, 0, 1, 0, 0, -1, 0, -1, 0, 0, 0, 1, 0, -1, 0, 0, 0, 0,
    -1, 0, 0, 1, 0, -1, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0, -1, 0, 1, 0, -1, 0, 0, 0, -1, 0, -1, 0,
    0, 0, 1, 0, -1, 0, 1,
];

pub const FINAL_EXPONENT: [u8; 540] = hex!(
    "02ee1db5dcc825b7e1bda9c0496a1c0a89ee0193d4977b3f7d4507d07363baa13f8d14a917848517badc3a43d1073776ab353f2c30698e8cc7deada9c0aadff5e9cfee9a074e43b9a660835cc872ee83ff3a0f0f1c0ad0d6106feaf4e347aa68ad49466fa927e7bb9375331807a0dce2630d9aa4b113f414386b0e8819328148978e2b0dd39099b86e1ab656d2670d93e4d7acdd350da5359bc73ab61a0c5bf24c374693c49f570bcd2b01f3077ffb10bf24dde41064837f27611212596bc293c8d4c01f25118790f4684d0b9c40a68eb74bb22a40ee7169cdc1041296532fef459f12438dfc8e2886ef965e61a474c5c85b0129127a1b5ad0463434724538411d1676a53b5a62eb34c05739334f46c02c3f0bd0c55d3109cd15948d0a1fad20044ce6ad4c6bec3ec03ef19592004cedd556952c6d8823b19dadd7c2498345c6e5308f1c511291097db60b1749bf9b71a9f9e0100418a3ef0bc627751bbd81367066bca6a4c1b6dcfc5cceb73fc56947a403577dfa9e13c24ea820b09c1d9f7c31759c3635de3f7a3639991708e88adce88177456c49637fd7961be1a4c7e79fb02faa732e2f3ec2bea83d196283313492caa9d4aff1c910e9622d2a73f62537f2701aaef6539314043f7bbce5b78c7869aeb2181a67e49eeed2161daf3f881bd88592d767f67c4717489119226c2f011d4cab803e9d71650a6f80698e2f8491d12191a04406fbc8fbd5f48925f98630e68bfb24c0bcb9b55df57510"
);

pub fn frobenius_coeff_fq6_c1() -> [Fq2; 3] {
    [
        Fq2 {
            c0: Fq::from_le_bytes_mod_order(&hex!(
                "0100000000000000000000000000000000000000000000000000000000000000"
            )),
            c1: Fq::from_le_bytes_mod_order(&hex!(
                "0000000000000000000000000000000000000000000000000000000000000000"
            )),
        },
        Fq2 {
            c0: Fq::from_le_bytes_mod_order(&hex!(
                "3d556f175795e3990c33c3c210c38cb743b159f53cec0b4cf711794f9847b32f"
            )),
            c1: Fq::from_le_bytes_mod_order(&hex!(
                "a2cb0f641cd56516ce9d7c0b1d2aae3294075ad78bcca44b20aeeb6150e5c916"
            )),
        },
        Fq2 {
            c0: Fq::from_le_bytes_mod_order(&hex!(
                "48fd7c60e544bde43d6e96bb9f068fc2b0ccace0e7d96d5e29a031e1724e6430"
            )),
            c1: Fq::from_le_bytes_mod_order(&hex!(
                "0000000000000000000000000000000000000000000000000000000000000000"
            )),
        },
    ]
}
pub fn xi_to_q_minus_1_over_2() -> Fq2 {
    Fq2 {
        c0: Fq::from_le_bytes_mod_order(&hex!(
            "5a13a071460154dc9859c9a9ede0aadbb9f9e2b698c65edcdcf59a4805f33c06"
        )),
        c1: Fq::from_le_bytes_mod_order(&hex!(
            "e3b02326637fd382d25ba28fc97d80212b6f79eca7b504079a0441acbc3cc007"
        )),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PairingCheckError;

impl core::error::Error for PairingCheckError {}
impl core::fmt::Display for PairingCheckError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Pairing check failed")
    }
}
