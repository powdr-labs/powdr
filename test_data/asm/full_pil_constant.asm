machine FullConstant(latch, function_id) {
	degree 2;

	constraints {
		pol constant function_id = [0]*;
		pol constant latch = [1]*;
		pol constant C = [0, 1]*;
		col commit w;
		w = C;
	}
}