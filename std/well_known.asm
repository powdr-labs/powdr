/// Creates a new witness column which is 1 on the first row 0 on the other rows
let is_first: int -> int = |i| if i == 0 { 1 } else { 0 };