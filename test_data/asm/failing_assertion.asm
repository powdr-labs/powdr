use std::check;

machine Empty {
    let line = |i| i - 7;
    let line_col: col = line;
    col witness w;
    w = line_col;


    check::assert(line(7) == 0, || "This should succeed.");
    check::assert(line(7) != 0, || "This should fail.");
}