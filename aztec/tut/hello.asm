machine HelloWorld {

    degree 8;

    reg pc[@pc];
    reg X[<=]; // assignment registers
    reg Y[<=];
    reg A;     // floating registers 

    instr incr X -> Y {
        Y = X + 1
    }   

    instr decr X -> Y {
        Y = X -1
    }

    // How do we stop this from being an assignment 
    // rather than a comparison, as the instruction looks the same
    instr assert_zero X {
        X = 0
    }

    // ============== memory instructions ==============
    instr mstore X { { addr, STEP, X } is m_is_write { m_addr, m_step, m_value } }
    instr mload -> X { { addr, STEP, X } is m_is_read { m_addr, m_step, m_value } }

    constraints {

    }

    function main {
        A <=X= ${ ("input", 0)}; // This is a built in function to take args from the command line
        A <== incr(A);
        A <== decr(A);
        assert_zero A;
        return;
    }
}