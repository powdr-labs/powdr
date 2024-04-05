// Should be a 1024th root of unity.
let root_of_unity: fe = 11353340290879379826;
let power_of_omega: int -> fe = |k| root_of_unity ** k;

machine Empty {
    // The permutation (0 1) (2 3) (4 5) ...
    let r: col = |i| match i % 2 {
        0 => power_of_omega(i + 1),
        1 => power_of_omega(i - 1),
    };
    let w: col;
    let f: col = |i| i / 2;
    w = f;

    // TODO we should also check that the degree is equal to 1024
    std::check::assert(root_of_unity ** 1024 == 1, || "");

    { w } connect { r };
}