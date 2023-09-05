machine FullConstant(latch, operation_id) {
	degree 2;

	constraints {
		pol constant operation_id = [0]*;
		pol constant latch = [1]*;
		pol constant C = [0, 1]*;
		col commit w;
		w = C;
	}
}