/// A variable in a PlonK gate.
enum Variable<V> {
    /// A variable from the input constraint system.
    /// At run-time, we can get the concrete values from the APC witness generation.
    Witness(V),
    /// A temporary variable (represented by an ID). Assuming there is at most one temporary variable in a gate,
    /// we can solve for its value at run-time.
    Tmp(usize),
}

/// A PlonK gate. For each gate, the following equation must hold:
/// q_l * a + q_r * b + q_o * c + q_mul * a * b + q_const = 0
/// where q_l, q_r, q_o, q_mul, and q_const are fixed coefficients
/// and a, b, c are variables.
/// If the same variable appears in multiple gates, a copy constraint
/// must be enforced.
struct Gate<T, V> {
    q_l: T,
    q_r: T,
    q_o: T,
    q_mul: T,
    q_const: T,
    a: Variable<V>,
    b: Variable<V>,
    c: Variable<V>,
}

/// The PlonK circuit, which is just a collection of gates.
struct PlonkCircuit<T, V> {
    gates: Vec<Gate<T, V>>,
}
