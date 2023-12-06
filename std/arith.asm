machine Arith(latch,operation_id){
    operation eq0<0> x1_1,x1_0,y1_1,y1_0,x2_1,x2_0 -> y3_1,y3_0;
    pol constant latch = [0,0,0,1]*;
    col witness operation_id;
    /*
        EQ0: A(x1) * B(y1) + C(x2) = D (y2) * 2 ** 8 + op (y3)
        // 8-bit additon/multiplication using 4-bit register // Base - 16
                90 * 255 + 0 = 89 * 256 + 166 (22950)
                                    5   10          // x1                       tempsum carry   sum   result(y2/y3)  eq0    
                                    15  15          // y1                          150    0   150(9,6)      6        144 
                            ---------------                                        225    9   234(14,10)    10       215
                                    75  150                                         75   14    89(5,9)      9         66
                                75  150                                                   5     5           5         -5
                            ...................
                                75  225 150         // tempsum
                            5   14   9    0         // carry
                            ....................
                            5   89  234 150         // sum
                            ------------------            
                            5   9   10    6         // result
    */

    let BIT4 = |i| i % 2 ** 4;  // (0,15)
    let BIT5 = |i| i % 2 ** 5;  // (0,31)

    pol commit x1_1,x1_0,y1_1,y1_0,x2_1,x2_0;
    x1_0 in BIT4;x1_1 in BIT4;y1_0 in BIT4;y1_1 in BIT4;x2_0 in BIT4;x2_1 in BIT4;
    pol constant CLK4_0 = [1,0,0,0]*;
    pol constant CLK4_1 = [0,1,0,0]*;
    pol constant CLK4_2 = [0,0,1,0]*;
    pol constant CLK4_3 = [0,0,0,1]*;

    pol witness y2_1,y2_0,y3_1,y3_0;
    y3_0 in BIT4; y3_1 in BIT4;y2_0 in BIT4;y2_1 in BIT4;
    x1_0' * (1-CLK4_3) = x1_0 * (1-CLK4_3);
    x1_1' * (1-CLK4_3) = x1_1 * (1-CLK4_3);
    y1_0' * (1-CLK4_3) = y1_0 * (1-CLK4_3);
	y1_1' * (1-CLK4_3) = y1_1 * (1-CLK4_3);
    x2_0' * (1-CLK4_3) = x2_0 * (1-CLK4_3);
	x2_1' * (1-CLK4_3) = x2_1 * (1-CLK4_3);

    y3_0' * (1-CLK4_3) = y3_0 * (1-CLK4_3);
    y3_1' * (1-CLK4_3) = y3_1 * (1-CLK4_3);
    y2_0' * (1-CLK4_3) = y2_0 * (1-CLK4_3);
	y2_1' * (1-CLK4_3) = y2_1 * (1-CLK4_3);
    
    let carry;carry in BIT5;
    
    carry * CLK4_0 = 0;

    pol eq0_0 = (x1_0 * y1_0) + x2_0 - y3_0;
    pol eq0_1 = (x1_0 * y1_1) + (x1_1 * y1_0) + x2_1 - y3_1; 
    pol eq0_2 = (x1_1 * y1_1) - y2_0;
    pol eq0_3 = -y2_1;
    
    pol eq0 = eq0_0 * CLK4_0 + eq0_1 * CLK4_1 + eq0_2 * CLK4_2 + eq0_3 * CLK4_3;
    carry' * 16 = carry + eq0;
}
