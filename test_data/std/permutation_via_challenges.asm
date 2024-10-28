use std::protocols::permutation::permutation;

machine Main with degree: 8 {

    col fixed x = [1, 5, 2, 6, 4, 2, 6, 3];
    col fixed SEL = [1, 0, 1, 0, 1, 0, 1, 0];
    col witness y;

    // Pre-compute f(x) = x + 1
    col witness sub_sel, sub_x, sub_y;
    std::utils::force_bool(sub_sel);
    sub_y = sub_x + 1;

    // Connect machines
    permutation(SEL $ [x, y] is sub_sel $ [sub_x, sub_y]);
}
