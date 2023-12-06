use std::arith::Arith;

machine Main{
    degree 50;

    reg pc[@pc];
    reg A_0[<=];
    reg A_1[<=];
    reg B_0[<=];
    reg B_1[<=];
    reg C_0[<=];
    reg C_1[<=];
    reg D_0[<=];
    reg D_1[<=];
    
    reg X0[<=];
    reg X1[<=];
    reg t_0;
    reg t_1;

    Arith arith;

    instr eq0 A_1,A_0,B_1,B_0,C_1,C_0 -> D_1,D_0 = arith.eq0

    instr assert_eq X1, X0 {
        X0 = X1
    }


    function main {
        // ( 90 * 255 + 0 ) % 256 == 166
        t_1,t_0 <== eq0(5,10,15,15,0,0);
        assert_eq t_0,6;
        assert_eq t_1,10;

        // ( 255 * 255 + 0 ) % 256 == 1
        t_1,t_0 <== eq0(15,15,15,15,0,0);
        assert_eq t_0,1;
        assert_eq t_1,0;

        // ( 17 * 1 + 17 ) % 256 == 34
        t_1,t_0 <== eq0(1,1,0,1,1,1);
        assert_eq t_0,2;
        assert_eq t_1,2;

        // ( 255 * 1 + 255 ) % 256 == 254
        t_1,t_0 <== eq0(15,15,0,1,15,15);
        assert_eq t_0,14;
        assert_eq t_1,15;
    }
}