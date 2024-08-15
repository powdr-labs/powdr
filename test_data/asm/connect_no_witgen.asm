// Should be a 2**32th root of unity in the goldilocks field.
let root_of_unity: fe = 7277203076849721926;

/// Returns a 2**n'th root of unity on input n.
let root_of_unity_for_log_degree: int -> fe = |n| root_of_unity ** (2**(32 - n));

let omega = root_of_unity_for_log_degree(4);
let power_of_omega: int -> fe = |k| omega ** k;

machine Empty with degree: 16 {
    // The permutation (0 1) (2 3) (4 5) ...
    let r: col = |i| match i % 2 {
        0 => power_of_omega(i + 1),
        1 => power_of_omega(i - 1),
    };
    let w: col;
    let f: col = |i| i / 2;
    w = f;

    std::check::assert(std::prover::min_degree() == 2**4, || "Degree is not 2**4");
    std::check::assert(omega ** (2**3) != 1, || "");
    std::check::assert(omega ** (2**4) == 1, || "");

    [ w ] connect [ r ];
}