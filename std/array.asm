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
/// This actually also works on field elements, so the type is currently too restrictive.
let sum: int[] -> int = |arr| fold(arr, 0, |a, b| a + b);

/// Zips two arrays
let zip2 = |array1, array2, fn| new(len(array1), |i| fn(array1[i], array2[i]));

/// Zip three arrays
let zip3 = |array1, array2, array3, fn| new(len(array1), |i| fn(array1[i], array2[i], array3[i]));