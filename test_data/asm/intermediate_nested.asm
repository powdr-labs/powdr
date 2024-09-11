machine Intermediate with
    latch: latch,
    operation_id: operation_id,
    degree: 8
{
	let latch: col = |i| 1;
    let operation_id: col = |i| 0;
	let x;
	col intermediate = x;
	col int2 = intermediate * x;
	col int3 = int2 + intermediate;
	int3 = (3 * x) + x;
}
