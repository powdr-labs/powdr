let x = 12;

mod utils {
    // Returns folder(...folder(folder(0, f(0)), f(1)) ..., f(length - 1))
    let fold = |length, f, initial, folder| match length {
        0 => initial,
        _ => folder(fold(length - 1, f, initial, folder), f(length - 1))
    };

    /// creates the array [f(0), f(1), ..., f(length - 1)]
    let make_array = |length, f| fold(length, f, [], |acc, e| acc + [e]);

    /// returns f(0) + f(1) + ... + f(length - 1)
    let sum = |length, f| fold(length, f, 0, |acc, e| acc + e);

    use super::x as r;
    let y = r;
}

mod R {
    use super::x;
    use super::utils::y;
    use super::utils::sum;
    use super::utils::make_array;

    machine FullConstant {
        degree 2;

        let C = |i| match i % 2 {
            0 => x,
            1 => y,
        };
        col commit w[2];

        sum(2, |i| w[i]) == 8;
        make_array(2, |i| w[i] == 4);
    }
}
