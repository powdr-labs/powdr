use std::check;

machine Empty {
    let line = |i| i - 7;
    col witness w;
    w = line;


    check::assert(line(7) == 0, || "This should succeed.");
    check::assert(line(7) != 0, || "This should fail.");
}