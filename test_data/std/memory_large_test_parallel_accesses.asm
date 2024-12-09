use std::machines::range::Byte2;
use std::machines::large_field::memory::Memory;

let main_degree: int = 2**8;
let memory_degree: int = 2**8;

machine Main with
    degree: main_degree,
    operation_id: operation_id
{
    Byte2 byte2;
    Memory memory(byte2, memory_degree, memory_degree);

    // Read values from ADDR1 & ADDR2, write value3 to ADDR3
    link if ACTIVE ~> value1 = memory.mload(ADDR1, STEP);
    link if ACTIVE ~> value2 = memory.mload(ADDR2, STEP + 1);
    link if ACTIVE ~> memory.mstore(ADDR3, STEP + 2, value3);

    // Because we're doing 3 memory operations in each time step, we'll
    // increment the time step by 3
    col fixed STEP(i) {i * 3};
    col fixed ACTIVE = [1, 1, 1, 1] + [0]*;
    col witness value1, value2, value3;

    // In each row, we set value3 = value1 + value2 + 1
    ACTIVE * (value1 + value2 + 1 - value3) = 0;

    // Program (with expected memory after each step)
    // - ADDR1 = ADDR2 = ADDR3 = 0        => [1, 0, 0, 0]
    // - ADDR1 = 0, ADDR2 = 1, ADDR3 = 2  => [1, 0, 2, 0]
    // - ADDR1 = 0, ADDR2 = 2, ADDR3 = 0  => [4, 0, 2, 0]
    // - ADDR1 = 0, ADDR2 = 0, ADDR3 = 4  => [4, 0, 2, 9]
    col fixed ADDR1 = [0, 0, 0, 0] + [0]*;
    col fixed ADDR2 = [0, 1, 2, 0] + [0]*;
    col fixed ADDR3 = [0, 2, 0, 4] + [0]*;

    // So, the last written value should be 9:
    col fixed LAST = [0, 0, 0, 1] + [0]*;
    LAST * (value3 - 9) = 0;
}
