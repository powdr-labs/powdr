# Dispatch table test
#
# Text addresses are stored in the data section and called through function pointers.

    .section .data
    .align 4
    .global data_section

data_section:
    .word text_label1   # Pointer to text_label1
    .word text_label2   # Pointer to text_label2

    .section .text
    .align 4
    .global _start

_start:
    # Save the return pointer
    mv a3, ra

    # Load the address of data_section
    la a0, data_section
    
    # Load the first pointer from data_section
    lw a1, 0(a0)
    # Call the function pointer
    jalr a1

    # Bail if the result is not 1
    li a2, 1
    beq a1, a2, continue
    unimp

continue:
    # Load the second pointer from data_section
    lw a1, 4(a0)
    # Call the function pointer
    jalr a1

    # Bail if the result is not 2
    li a2, 2
    beq a1, a2, finish
    unimp

finish:
    # Finish the test (returning from _start)
    jr a3

text_label1:
    # Function at text_label1
    li a1, 1
    ret

text_label2:
    # Function at text_label2
    li a1, 2
    ret
