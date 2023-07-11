machine FullConstant(latch, function_id) {
	degree 2;

	constraints {
		pol constant latch = [1]*;
		pol constant C = [0, 1]*;
		col commit w;
		w = C;
	}
}