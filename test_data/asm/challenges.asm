use std::prover::challenge;

machine Main with degree: 4 {

    col fixed foo = [1, 2, 3, 4]*;

    // Stage-0 witness column, does not depend on any challenges:
    let bar;
    bar = foo + 3;

    // Stage-1 witness column, depends on after-stage-0 challenge #1:
    let stage(1) bar2;
    let alpha: expr = challenge(0, 1);
    bar2 = bar + alpha;
}