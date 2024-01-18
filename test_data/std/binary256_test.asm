use std::binary256::Binary;

machine Main{
    reg pc[@pc];
    reg X0_7[<=];reg X0_6[<=];reg X0_5[<=];reg X0_4[<=];reg X0_3[<=];reg X0_2[<=];reg X0_1[<=];reg X0_0[<=];
    reg X1_7[<=];reg X1_6[<=];reg X1_5[<=];reg X1_4[<=];reg X1_3[<=];reg X1_2[<=];reg X1_1[<=];reg X1_0[<=];
    reg X2_7[<=];reg X2_6[<=];reg X2_5[<=];reg X2_4[<=];reg X2_3[<=];reg X2_2[<=];reg X2_1[<=];reg X2_0[<=];
    
    reg out_7;reg out_6;reg out_5;reg out_4;reg out_3;reg out_2;reg out_1;reg out_0;

    degree 196608;
    let input;

    Binary bin;

    instr and X0_7,X0_6,X0_5,X0_4,X0_3,X0_2,X0_1,X0_0, X1_7,X1_6,X1_5,X1_4,X1_3,X1_2,X1_1,X1_0 -> X2_7,X2_6,X2_5,X2_4,X2_3,X2_2,X2_1,X2_0 = bin.and
    instr or X0_7,X0_6,X0_5,X0_4,X0_3,X0_2,X0_1,X0_0, X1_7,X1_6,X1_5,X1_4,X1_3,X1_2,X1_1,X1_0 -> X2_7,X2_6,X2_5,X2_4,X2_3,X2_2,X2_1,X2_0 = bin.or
    instr xor X0_7,X0_6,X0_5,X0_4,X0_3,X0_2,X0_1,X0_0, X1_7,X1_6,X1_5,X1_4,X1_3,X1_2,X1_1,X1_0 -> X2_7,X2_6,X2_5,X2_4,X2_3,X2_2,X2_1,X2_0 = bin.xor

    instr loop { pc' = pc }

    instr assert_eq8 X0_7,X0_6,X0_5,X0_4,X0_3,X0_2,X0_1,X0_0, X1_7,X1_6,X1_5,X1_4,X1_3,X1_2,X1_1,X1_0 {
        X0_7 = X1_7, X0_6 = X1_6, X0_5 = X1_5, X0_4 = X1_4, X0_3 = X1_3, X0_2 = X1_2, X0_1 = X1_1, X0_0 = X1_0
    }

    function main {
        /////////
        // AND
        /////////
        //  a: 0x0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F,
        //  b: 0x0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F,
        //  c: 0x0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F,
        out_7,out_6,out_5,out_4,out_3,out_2,out_1,out_0 <== and(0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F, 0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F);
        assert_eq8 out_7,out_6,out_5,out_4,out_3,out_2,out_1,out_0, 0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F,0x0F0F0F0F;

        /////////
        // OR
        /////////
        //  a: 0xb01465104267f84effb2ed7b9c1d7ec65f4652652b2367e75549a06e692cb53f,
        //  b: 0xb486e735789b55a76376c3478ae4bc588d0740184aa0873dd0386392daed8db5,
        //  c: 0xb496e7357afffdeffff6ef7f9efdfededf47527d6ba3e7ffd579e3fefbedbdbf,
        out_7,out_6,out_5,out_4,out_3,out_2,out_1,out_0 <== or(0xb0146510, 0x4267f84e, 0xffb2ed7b, 0x9c1d7ec6, 0x5f465265, 0x2b2367e7, 0x5549a06e, 0x692cb53f,   0xb486e735, 0x789b55a7, 0x6376c347, 0x8ae4bc58, 0x8d074018, 0x4aa0873d, 0xd0386392, 0xdaed8db5);
        assert_eq8 out_7,out_6,out_5,out_4,out_3,out_2,out_1,out_0, 0xb496e735, 0x7afffdef, 0xfff6ef7f, 0x9efdfede, 0xdf47527d, 0x6ba3e7ff, 0xd579e3fe, 0xfbedbdbf;

        /////////
        // XOR
        /////////
        //  a: 0x0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F,
        //  b: 0xF0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0,
        //  c: 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,
        out_7,out_6,out_5,out_4,out_3,out_2,out_1,out_0 <== xor(0x0F0F0F0F, 0x0F0F0F0F, 0x0F0F0F0F, 0x0F0F0F0F, 0x0F0F0F0F, 0x0F0F0F0F, 0x0F0F0F0F, 0x0F0F0F0F,     0xF0F0F0F0, 0xF0F0F0F0, 0xF0F0F0F0, 0xF0F0F0F0, 0xF0F0F0F0, 0xF0F0F0F0, 0xF0F0F0F0, 0xF0F0F0F0);
        assert_eq8 out_7,out_6,out_5,out_4,out_3,out_2,out_1,out_0, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF;

        //  a: 0xb01465104267f84effb2ed7b9c1d7ec65f4652652b2367e75549a06e692cb53f,
        //  b: 0xb486e735789b55a76376c3478ae4bc588d0740184aa0873dd0386392daed8db5,
        //  c: 0x49282253afcade99cc42e3c16f9c29ed241127d6183e0da8571c3fcb3c1388a,
        out_7,out_6,out_5,out_4,out_3,out_2,out_1,out_0 <== xor(0xb0146510, 0x4267f84e, 0xffb2ed7b, 0x9c1d7ec6, 0x5f465265, 0x2b2367e7, 0x5549a06e, 0x692cb53f,     0xb486e735, 0x789b55a7, 0x6376c347, 0x8ae4bc58, 0x8d074018, 0x4aa0873d, 0xd0386392, 0xdaed8db5);
        assert_eq8 out_7,out_6,out_5,out_4,out_3,out_2,out_1,out_0, 0x4928225, 0x3afcade9, 0x9cc42e3c, 0x16f9c29e, 0xd241127d, 0x6183e0da, 0x8571c3fc, 0xb3c1388a;

    }


}