use std::machines::write_once_memory16::WriteOnceMemory;

machine Main with degree: 256 {
    WriteOnceMemory memory;

    reg pc[@pc];
    reg X[<=];
    reg YH[<=];
    reg YL[<=];
    reg AH;
    reg AL;

    instr mload X -> YH, YL link => memory.access(X, YH, YL);
    instr mstore X, YH, YL -> link => memory.access(X, YH, YL);

    instr assert_eq X, YL { X = YL }

    function main {
        // Minimal address: 0
        mstore 0, 0x12, 0x34;
        AH, AL <== mload(0);
        assert_eq AH, 0x12;
        assert_eq AL, 0x34;

        // Maximal address: N - 1
        mstore 255, 0x12, 0x34;
        AH, AL <== mload(255);
        assert_eq AH, 0x12;
        assert_eq AL, 0x34;

        // Maximal value: Any field element
        mstore 17, 0xffff, 0xffff;
        AH, AL <== mload(17);
        assert_eq AH, 0xffff;
        assert_eq AL, 0xffff;

        // Read same value again
        AH, AL <== mload(17);
        assert_eq AH, 0xffff;
        assert_eq AL, 0xffff;

        return;
    }
}
