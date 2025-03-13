use std::prelude::Query;

machine Sqrt with
    latch: latch,
    operation_id: operation_id,
{

    operation sqrt<0> x -> y;

    col fixed operation_id = [0]*;
    col fixed latch = [1]*;
    // Only works for small results, to keep the degree of this example small.
    col fixed range(i) { i % 8 };
    col witness x;

    // Witness generation is not smart enough to figure out that
    // there is a unique witness, so we provide it as a hint.
    let sqrt_hint: fe -> fe = |x| std::convert::fe(sqrt_rec(std::convert::int(x), std::convert::int(x)));

    // This function computes the square root of an integer or at least
    // the largest integer smaller than the square root if the input
    // is not a square.
    // The parameter `y` is a guess, which can be equal to `x`.
    let sqrt_rec: int, int -> int = |y, x|
        if y * y <= x && (y + 1) * (y + 1) > x {
            y
        } else {
            sqrt_rec((y + x / y) / 2, x)
        };

    col witness y;
    query |i| std::prover::compute_from(y, i, [x], |inputs| sqrt_hint(inputs[0]));

    y * y = x;
    
    // Note that this is required to make the witness unique
    // (y := -y would also satisfy y * y = x, but we want the positive solution).
    [ y ] in [ range ];
}


machine Main with degree: 8 {
    Sqrt sqrt(8, 8);

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    col witness XInv;
    col witness XIsZero;
    XIsZero  = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;

    instr assert_zero X { XIsZero = 1 }

    instr sqrt X -> Y link => Y = sqrt.sqrt(X);


    function main {

        A <== sqrt(4);
        assert_zero A - 2;

        A <== sqrt(1);
        assert_zero A - 1;

        return;
    }
}
