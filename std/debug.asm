/// This is a built-in function taking a string argument and printing it on stdout
/// when evaluated.
/// It returns an empty array so that it can be used at constraint level.
/// This symbol is not an empty array, the actual semantics are overridden.
let print: string -> constr[] = [];

let println: string -> constr[] = |msg| print(msg + "\n");