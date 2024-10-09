use std::machines::range::Byte2;

// Implements 32-bit addition and subtraction using 16-bit limbs.
// Requires the field to contain at least 17 bits.
machine AddSub(byte2: Byte2) with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    operation add<0> A_h, A_l, B_h, B_l -> C_h, C_l;
    operation sub<1> C_h, C_l, B_h, B_l -> A_h, A_l;
    operation gt<2> C_h, C_l, B_h, B_l -> carry32;

    std::check::assert(std::field::modulus() >= 2**18, || "Field too small.");

    col witness operation_id;

    col fixed latch(i) { 1 };

    let carry16;
    std::utils::force_bool(carry16);

    let carry32;
    std::utils::force_bool(carry32);

    link => byte2.check(A_l);
    link => byte2.check(A_h);
    link => byte2.check(B_l);
    link => byte2.check(B_h);
    link => byte2.check(C_l);
    link => byte2.check(C_h);

    col witness A_h, A_l;
    col witness B_h, B_l;
    col witness C_h, C_l;

    A_l + B_l = C_l + carry16 * 2**16;
    A_h + B_h + carry16 = C_h + carry32 * 2**16;
}
