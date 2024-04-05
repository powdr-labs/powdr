/// This is a built-in function taking a string argument and printing it on stdout
/// when evaluated.
/// It returns an empty array so that it can be used at constraint level.
/// This symbol is not an empty array, the actual semantics are overridden.
let<T: ToString> print: T -> constr[] = [];

let<T: ToString> println: T -> constr[] = |msg| { let _ = print(msg); print("\n") };