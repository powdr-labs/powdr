use std::machines::range::Byte2;
use std::machines::range::Bit2;
use std::machines::range::Range9;
use std::field::modulus;
use std::check::require_field_bits;

// Implements 32-bit addition using 16-bit limbs.
// Requires the field to contain at least 17 bits.
machine Add4(byte2: Byte2, bit2: Bit2, range9: Range9) with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    require_field_bits(17, || "Add4 requires a field that fits any 17-Bit value.");

    operation add<0> A_h, A_l, B_h, B_l, C_h, C_l, D_h, D_l -> E_h, E_l;

    col witness operation_id;

    col fixed latch(i) { 1 };

    let carry16;
    link => bit2.check(carry16);

    let carry32;
    link => bit2.check(carry32);

    //let carry;
    //link => range9.check(carry);
//
    //carry = 2 * carry32 + carry16;

    link => byte2.check(A_l);
    link => byte2.check(A_h);
    link => byte2.check(B_l);
    link => byte2.check(B_h);
    link => byte2.check(C_l);
    link => byte2.check(C_h);
    link => byte2.check(D_l);
    link => byte2.check(D_h);
    link => byte2.check(E_l);
    link => byte2.check(E_h);

    col witness A_h, A_l;
    col witness B_h, B_l;
    col witness C_h, C_l;
    col witness D_h, D_l;
    col witness E_h, E_l;

    A_l + B_l + C_l + D_l = E_l + carry16 * 2**16;
    A_h + B_h + C_h + D_h + carry16 = E_h + carry32 * 2**16;
}
