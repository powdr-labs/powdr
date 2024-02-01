/// This is a built-in function taking an array argument and returning
/// the length of the array.
/// This symbol is not an empty array, the actual semantics are overridden.
let<T> len: T[] -> int = [];

/// Evaluates to the array [f(0), f(1), ..., f(length - 1)].
let<T> new: int, (int -> T) -> T[] = |length, f| std::utils::fold(length, f, [], |acc, e| (acc + [e]));

/// Evaluates to the array [f(arr[0]), f(arr[1]), ..., f(arr[len(arr) - 1])].
let<T1, T2> map: T1[], (T1 -> T2) -> T2[] = |arr, f| new(len(arr), |i| f(arr[i]));

/// Computes folder(...folder(folder(initial, arr[0]), arr[1]) ..., arr[len(arr) - 1])
let<T1, T2> fold: T1[], T2, (T2, T1 -> T2) -> T2 = |arr, initial, folder| std::utils::fold(len(arr), |i| arr[i], initial, folder);

/// Returns the sum of the array elements.
// TODO: Should make use of the Default or Zero trait instead of FromLiteral (then we can also
// use this function to flatten an array of arrays.
let<T: Add + FromLiteral> sum: T[] -> T = |arr| fold(arr, 0, |a, b| a + b);

/// Zips two arrays
/// TODO: Assert that lengths are equal when expressions are supported.
let<T1, T2, T3> zip: T1[], T2[], (T1, T2 -> T3) -> T3[] = |array1, array2, fn| new(len(array1), |i| fn(array1[i], array2[i]));
