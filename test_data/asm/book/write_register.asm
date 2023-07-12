machine Machine {

    degree 8;

    reg pc[@pc];
    reg X[<=];
// ANCHOR: declaration
reg A;
// ANCHOR_END: declaration
    reg B;

    function main {
// ANCHOR: component
// write to A
A <=X= 1;
// A is 1

// read from A
B <=X= A;
// A is still 1
// ANCHOR_END: component
        return;
    }
}