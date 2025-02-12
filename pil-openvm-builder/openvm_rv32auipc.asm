
machine Rv32Auipc with
	latch: latch,
    call_selectors: sel, {

    col fixed is_first_row = [1] + [0]*;
    col fixed is_last_row = [0] + [1]*;
    col fixed is_transition = [0] + [1]* + [0];

    col fixed latch = [1]*;


    // Witness columns
    col witness w0;
    col witness w1;
    col witness w2;
    col witness w3;
    col witness w4;
    col witness w5;
    col witness w6;
    col witness w7;
    col witness w8;
    col witness w9;
    col witness w10;
    col witness w11;
    col witness w12;

    // Constraints
    (w0*(w0-1)) = 0;
    (w0*(w7-(w0-(((0+(w4*256))+(w5*65536))+(w6*16777216))))) = 0;
    (w0*((2005401601*(((w4+w1)-w8)+0))*((2005401601*(((w4+w1)-w8)+0))-1))) = 0;
    (w0*((2005401601*(((w5+w2)-w9)+(2005401601*(((w4+w1)-w8)+0))))*((2005401601*(((w5+w2)-w9)+(2005401601*(((w4+w1)-w8)+0))))-1))) = 0;
    (w0*((2005401601*(((w6+w3)-w10)+(2005401601*(((w5+w2)-w9)+(2005401601*(((w4+w1)-w8)+0))))))*((2005401601*(((w6+w3)-w10)+(2005401601*(((w5+w2)-w9)+(2005401601*(((w4+w1)-w8)+0))))))-1))) = 0;
}