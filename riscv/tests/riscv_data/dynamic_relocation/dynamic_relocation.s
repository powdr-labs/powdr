# TODO: turn this into an actual test, with fail conditions, and put on the testsuite

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
    # Load the address of data_section
    la a0, data_section
    
    # Load the first pointer from data_section
    lw a1, 0(a0)
    # Call the function pointer
    jalr ra, a1

    # Bail if the result is not 1
    li a2, 1
    beq a0, a2, continue
    unimp

continue:
    # Load the second pointer from data_section
    lw a1, 4(a0)
    # Call the function pointer
    jalr ra, a1

    # Bail if the result is not 2
    li a2, 2
    beq a0, a2, final_loop
    unimp

    # Finish in an infinite loop
final_loop:
    j final_loop

text_label1:
    # Function at text_label1
    li a0, 1          # For example, setting a0 to 1
    ret

text_label2:
    # Function at text_label2
    li a0, 2          # For example, setting a0 to 2
    ret
