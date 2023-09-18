machine Intermediate(latch, operation_id) {
	constraints {
		col fixed latch = [1]*;
		col fixed operation_id = [0]*;
		col witness x;
		col intermediate = x;
		col int2 = intermediate * x;
		col int3 = int2 + intermediate;
		int3 = (3 * x) + x;
	}
}