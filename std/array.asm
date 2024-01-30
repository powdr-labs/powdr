/// This is a built-in function taking an array argument and returning
/// the length of the array.
/// This symbol is not an empty array, the actual semantics are overridden.
let len = [];

/// Evaluates to the array [f(0), f(1), ..., f(length - 1)].
let new = |length, f| std::utils::fold(length, f, [], |acc, e| (acc + [e]));

/// Evaluates to the array [f(arr[0]), f(arr[1]), ..., f(arr[len(arr) - 1])].
let map = |arr, f| new(len(arr), |i| f(arr[i]));

/// Computes folder(...folder(folder(initial, arr[0]), arr[1]) ..., arr[len(arr) - 1])
let fold = |arr, initial, folder| std::utils::fold(len(arr), |i| arr[i], initial, folder);

/// Returns the sum of the array elements.
/// TODO the type is currently wrong (it should be generic and take any array whose
/// elements implement addition, not just integers), but we will probably only start
/// checking that type once we also have generic types.
let sum: int[] -> int = [|arr| fold(arr, 0, |a, b| a + b)][0];