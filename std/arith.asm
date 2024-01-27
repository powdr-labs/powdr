/*
    EQ0: A(x1) * B(y1) + C(x2) = D (y2) * 2 ** 16 + op (y3)
*/
machine Arith(latch,operation_id){
    operation eq0<0> x1[15],x1[14],x1[13],x1[12],x1[11],x1[10],x1[9],x1[8],x1[7],x1[6],x1[5],x1[4],x1[3],x1[2],x1[1],x1[0], y1[15],y1[14],y1[13],y1[12],y1[11],y1[10],y1[9],y1[8],y1[7],y1[6],y1[5],y1[4],y1[3],y1[2],y1[1],y1[0], x2[15],x2[14],x2[13],x2[12],x2[11],x2[10],x2[9],x2[8],x2[7],x2[6],x2[5],x2[4],x2[3],x2[2],x2[1],x2[0] -> y3[15],y3[14],y3[13],y3[12],y3[11],y3[10],y3[9],y3[8],y3[7],y3[6],y3[5],y3[4],y3[3],y3[2],y3[1],y3[0];
    pol constant latch = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]*;
    col witness operation_id;

    constant %BASE = 2**4;
    let EL_RANGE = |i| i % %BASE ; 
    let CR_RANGE = |i| i % 2 ** 8;

    pol commit x1[16],y1[16],x2[16]; pol witness y2[16],y3[16];

    let clock = |j, row| row % 32 == j;
    // Arrays of fixed columns are not supported yet.
    // These need an explicit lambda, otherwise they are not recognized as fixed columns.
    // The type system will handle that in the future.
    let CLK32_0 = |row| clock(0, row);
    let CLK32_1 = |row| clock(1, row);
    let CLK32_2 = |row| clock(2, row);
    let CLK32_3 = |row| clock(3, row);
    let CLK32_4 = |row| clock(4, row);
    let CLK32_5 = |row| clock(5, row);
    let CLK32_6 = |row| clock(6, row);
    let CLK32_7 = |row| clock(7, row);
    let CLK32_8 = |row| clock(8, row);
    let CLK32_9 = |row| clock(9, row);
    let CLK32_10 = |row| clock(10, row);
    let CLK32_11 = |row| clock(11, row);
    let CLK32_12 = |row| clock(12, row);
    let CLK32_13 = |row| clock(13, row);
    let CLK32_14 = |row| clock(14, row);
    let CLK32_15 = |row| clock(15, row);
    let CLK32_16 = |row| clock(16, row);
    let CLK32_17 = |row| clock(17, row);
    let CLK32_18 = |row| clock(18, row);
    let CLK32_19 = |row| clock(19, row);
    let CLK32_20 = |row| clock(20, row);
    let CLK32_21 = |row| clock(21, row);
    let CLK32_22 = |row| clock(22, row);
    let CLK32_23 = |row| clock(23, row);
    let CLK32_24 = |row| clock(24, row);
    let CLK32_25 = |row| clock(25, row);
    let CLK32_26 = |row| clock(26, row);
    let CLK32_27 = |row| clock(27, row);
    let CLK32_28 = |row| clock(28, row);
    let CLK32_29 = |row| clock(29, row);
    let CLK32_30 = |row| clock(30, row);
    let CLK32_31 = |row| clock(31, row);
    let CLK = [CLK32_0,CLK32_1,CLK32_2,CLK32_3,CLK32_4,CLK32_5,CLK32_6,CLK32_7,CLK32_8,CLK32_9,CLK32_10,CLK32_11,CLK32_12,CLK32_13,CLK32_14,CLK32_15,CLK32_16,CLK32_17,CLK32_18,CLK32_19,CLK32_20,CLK32_21,CLK32_22,CLK32_23,CLK32_24,CLK32_25,CLK32_26,CLK32_27,CLK32_28,CLK32_29,CLK32_30,CLK32_31];

    let fold = |length, f, initial, folder| match length {
        0 => initial,
        _ => folder(fold(length - 1, f, initial, folder), f(length - 1))
    };

    let make_array = |length, f| fold(length, f, [], |acc, e| acc + [e]);
    let sum = |length, f| fold(length, f, 0, |acc, e| acc + e);

    let fixed_inside_block = [|x,clk| (x - x') * (1 - clk) == 0][0];

    let array_as_fun = |arr, len| |i| match i < len {
		1 => arr[i],
		_ => 0,
	};
    let prepend_zeros = |arr, amount| |i| match i < amount { 1 => 0, _ => arr(i - amount) };
    let dot_prod = |n, a, b| sum(n, |i| a(i) * b(i));
	let product = |a, b| |n| dot_prod(n + 1, a, |i| b(n - i));

    make_array(16, |i| fixed_inside_block(x1[i],CLK[31]));
    make_array(16, |i| fixed_inside_block(y1[i],CLK[31]));
    make_array(16, |i| fixed_inside_block(x2[i],CLK[31]));
    make_array(16, |i| fixed_inside_block(y2[i],CLK[31]));
    make_array(16, |i| fixed_inside_block(y3[i],CLK[31]));

    sum(16, |i| x1[i] * CLK[i]) + sum(16, |i| y1[i] * CLK[16 + i]) in EL_RANGE;
    sum(16, |i| x2[i] * CLK[i]) in EL_RANGE;

    y3[0] in EL_RANGE; y3[1] in EL_RANGE; y3[2] in EL_RANGE; y3[3] in EL_RANGE;y3[4] in EL_RANGE; y3[5] in EL_RANGE; y3[6] in EL_RANGE; y3[7] in EL_RANGE;y3[8] in EL_RANGE; y3[9] in EL_RANGE; y3[10] in EL_RANGE; y3[11] in EL_RANGE;y3[12] in EL_RANGE; y3[13] in EL_RANGE; y3[14] in EL_RANGE; y3[15] in EL_RANGE;
    y2[0] in EL_RANGE; y2[1] in EL_RANGE; y2[2] in EL_RANGE; y2[3] in EL_RANGE;y2[4] in EL_RANGE; y2[5] in EL_RANGE; y2[6] in EL_RANGE; y2[7] in EL_RANGE;y2[8] in EL_RANGE; y2[9] in EL_RANGE; y2[10] in EL_RANGE; y2[11] in EL_RANGE;y2[12] in EL_RANGE; y2[13] in EL_RANGE; y2[14] in EL_RANGE; y2[15] in EL_RANGE;

    let x1f = array_as_fun(x1, 16);
    let y1f = array_as_fun(y1, 16);
	let x2f = array_as_fun(x2, 16);
	let y2f = array_as_fun(y2, 16);
	let y3f = array_as_fun(y3, 16);
    //let sf = array_as_fun(s, 16);
	//let q0f = array_as_fun(q0, 16);

    let eq0 = [|nr| product(x1f, y1f)(nr)+ x2f(nr)- prepend_zeros(y2f, 16)(nr) - y3f(nr)][0];

    let carry;carry in CR_RANGE;

    carry * CLK[0] = 0;

    sum(32, |i| eq0(i) * CLK[i]) + carry = carry' * %BASE;

}
