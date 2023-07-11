machine FullConstant {
	degree 2;

	constraints {
		pol constant C = [0, 1]*;
		col commit w;
		w = C;
	}
}