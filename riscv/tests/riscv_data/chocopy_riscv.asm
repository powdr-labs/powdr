# Compiled ChocoPy Program to RISC-V assembly
# Execute (run or step-through) using the 'Simulator' tab above 
# Output will appear on the bottom-left of the simulator
  .equiv @sbrk, 9
  .equiv @print_string, 4
  .equiv @print_char, 11
  .equiv @print_int, 1
  .equiv @exit2, 17
  .equiv @read_string, 8
  .equiv @fill_line_buffer, 18
  .equiv @.__obj_size__, 4
  .equiv @.__len__, 12
  .equiv @.__int__, 12
  .equiv @.__bool__, 12
  .equiv @.__str__, 16
  .equiv @.__elts__, 16
  .equiv @error_div_zero, 2
  .equiv @error_arg, 1
  .equiv @error_oob, 3
  .equiv @error_none, 4
  .equiv @error_oom, 5
  .equiv @error_nyi, 6
  .equiv @listHeaderWords, 4
  .equiv @strHeaderWords, 4
  .equiv @bool.True, const_1
  .equiv @bool.False, const_0

.data

.globl _object_prototype
_object_prototype:
  .word 0                                  # Type tag for class: object
  .word 3                                  # Object size
  .word _object_dispatchTable              # Pointer to dispatch table
  .align 2

.globl _int_prototype
_int_prototype:
  .word 1                                  # Type tag for class: int
  .word 4                                  # Object size
  .word _int_dispatchTable                 # Pointer to dispatch table
  .word 0                                  # Initial value of attribute: __int__
  .align 2

.globl _bool_prototype
_bool_prototype:
  .word 2                                  # Type tag for class: bool
  .word 4                                  # Object size
  .word _bool_dispatchTable                # Pointer to dispatch table
  .word 0                                  # Initial value of attribute: __bool__
  .align 2

.globl _str_prototype
_str_prototype:
  .word 3                                  # Type tag for class: str
  .word 5                                  # Object size
  .word _str_dispatchTable                 # Pointer to dispatch table
  .word 0                                  # Initial value of attribute: __len__
  .word 0                                  # Initial value of attribute: __str__
  .align 2

.globl _.list_prototype
_.list_prototype:
  .word -1                                 # Type tag for class: .list
  .word 4                                  # Object size
  .word 0                                  # Pointer to dispatch table
  .word 0                                  # Initial value of attribute: __len__
  .align 2

.globl _object_dispatchTable
_object_dispatchTable:
  .word _object.__init__                   # Implementation for method: object.__init__

.globl _int_dispatchTable
_int_dispatchTable:
  .word _object.__init__                   # Implementation for method: int.__init__

.globl _bool_dispatchTable
_bool_dispatchTable:
  .word _object.__init__                   # Implementation for method: bool.__init__

.globl _str_dispatchTable
_str_dispatchTable:
  .word _object.__init__                   # Implementation for method: str.__init__

.text

.globl main
main:
  lui a0, 8192                             # Initialize heap size (in multiples of 4KB)
  add s11, s11, a0                         # Save heap size
  jal heap.init                            # Call heap.init routine
  mv gp, a0                                # Initialize heap pointer
  mv s10, gp                               # Set beginning of heap
  add s11, s10, s11                        # Set end of heap (= start of heap + heap size)
  mv ra, zero                              # No normal return from main program.
  mv fp, zero                              # No preceding frame.
  mv fp, zero                              # Top saved FP is 0.
  mv ra, zero                              # No function return from top level.
  addi sp, sp, -@..main.size               # Reserve space for stack frame.
  sw ra, @..main.size-4(sp)                # return address
  sw fp, @..main.size-8(sp)                # control link
  addi fp, sp, @..main.size                # New fp is at old SP.
  jal initchars                            # Initialize one-character strings.
  li a0, 4                                 # Load integer literal 4
  sw a0, -28(fp)                           # Push argument 5 from last.
  li a0, 8                                 # Load integer literal 8
  sw a0, -32(fp)                           # Push argument 4 from last.
  li a0, 15                                # Load integer literal 15
  sw a0, -36(fp)                           # Push argument 3 from last.
  li a0, 16                                # Load integer literal 16
  sw a0, -40(fp)                           # Push argument 2 from last.
  li a0, 23                                # Load integer literal 23
  sw a0, -44(fp)                           # Push argument 1 from last.
  li a0, 5                                 # Pass list length
  sw a0, -48(fp)                           # Push argument 0 from last.
  addi sp, fp, -48                         # Set SP to last argument.
  jal conslist                             # Move values to new list object
  addi sp, fp, -@..main.size               # Set SP to stack frame top.
  sw a0, -12(fp)                           # Push argument 1 from last.
  li a0, 15                                # Load integer literal 15
  sw a0, -16(fp)                           # Push argument 0 from last.
  addi sp, fp, -16                         # Set SP to last argument.
  jal _contains                            # Invoke function: contains
  addi sp, fp, -@..main.size               # Set SP to stack frame top.
  beqz a0, label_2                         # Branch on false.
  la a0, const_2                           # Load string literal
  sw a0, -16(fp)                           # Push argument 0 from last.
  addi sp, fp, -16                         # Set SP to last argument.
  jal _print                               # Invoke function: print
  addi sp, fp, -@..main.size               # Set SP to stack frame top.
  j label_1                                # Then body complete; jump to end-if
label_2:                                   # Else body
  la a0, const_3                           # Load string literal
  sw a0, -16(fp)                           # Push argument 0 from last.
  addi sp, fp, -16                         # Set SP to last argument.
  jal _print                               # Invoke function: print
  addi sp, fp, -@..main.size               # Set SP to stack frame top.
label_1:                                   # End of if-else statement
  .equiv @..main.size, 48
label_0:                                   # End of program
  li a0, 10                                # Code for ecall: exit
  ecall

.globl _object.__init__
_object.__init__:
# Init method for type object.	
  mv a0, zero                              # `None` constant
  jr ra                                    # Return

.globl _print
_print:
# Function print
  lw a0, 0(sp)                             # Load arg
  beq a0, zero, print_6                    # None is an illegal argument
  lw t0, 0(a0)                             # Get type tag of arg
  li t1, 1                                 # Load type tag of `int`
  beq t0, t1, print_7                      # Go to print(int)
  li t1, 3                                 # Load type tag of `str`
  beq t0, t1, print_8                      # Go to print(str)
  li t1, 2                                 # Load type tag of `bool`
  beq t0, t1, print_9                      # Go to print(bool)
print_6:                                   # Invalid argument
  li a0, 1                                 # Exit code for: Invalid argument
  la a1, const_4                           # Load error message as str
  addi a1, a1, @.__str__                   # Load address of attribute __str__
  j abort                                  # Abort

# Printing bools
print_9:                                   # Print bool object in A0
  lw a0, @.__bool__(a0)                    # Load attribute __bool__
  beq a0, zero, print_10                   # Go to: print(False)
  la a0, const_5                           # String representation: True
  j print_8                                # Go to: print(str)
print_10:                                  # Print False object in A0
  la a0, const_6                           # String representation: False
  j print_8                                # Go to: print(str)

# Printing strs.
print_8:                                   # Print str object in A0
  addi a1, a0, @.__str__                   # Load address of attribute __str__
  j print_11                               # Print the null-terminated string is now in A1
  mv a0, zero                              # Load None
  j print_5                                # Go to return
print_11:                                  # Print null-terminated string in A1
  li a0, @print_string                     # Code for ecall: print_string
  ecall                                    # Print string
  li a1, 10                                # Load newline character
  li a0, @print_char                       # Code for ecall: print_char
  ecall                                    # Print character
  j print_5                                # Go to return

# Printing ints.
print_7:                                   # Print int object in A0
  lw a1, @.__int__(a0)                     # Load attribute __int__
  li a0, @print_int                        # Code for ecall: print_int
  ecall                                    # Print integer
  li a1, 10                                # Load newline character
  li a0, 11                                # Code for ecall: print_char
  ecall                                    # Print character

print_5:                                   # End of function
  mv a0, zero                              # Load None
  jr ra                                    # Return to caller

.globl _len
_len:
# Function len
      # We do not save/restore fp/ra for this function
      # because we know that it does not use the stack or does not
      # call other functions.

  lw a0, 0(sp)                             # Load arg
  beq a0, zero, len_12                     # None is an illegal argument
  lw t0, 0(a0)                             # Get type tag of arg
  li t1, 3                                 # Load type tag of `str`
  beq t0, t1, len_13                       # Go to len(str)
  li t1, -1                                # Load type tag for list objects
  beq t0, t1, len_13                       # Go to len(list)
len_12:                                    # Invalid argument
  li a0, @error_arg                        # Exit code for: Invalid argument
  la a1, const_4                           # Load error message as str
  addi a1, a1, @.__str__                   # Load address of attribute __str__
  j abort                                  # Abort
len_13:                                    # Get length of string
  lw a0, @.__len__(a0)                     # Load attribute: __len__
  jr ra                                    # Return to caller

.globl _input
_input:

  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  sw s1, 4(sp)
  addi fp, sp, 16
  li a0, @fill_line_buffer
  ecall
  bgez a0, input_nonempty
  la a0, _str_prototype
  j input_done
input_nonempty:
  mv s1, a0
  addi t0, s1, 5
  addi t0, t0, @.__str__
  srli a1, t0, 2
  la a0, _str_prototype
  jal ra, alloc2
  sw s1, @.__len__(a0)
  mv a2, s1
  mv s1, a0
  addi a1, a0, @.__str__
  li a0, @read_string
  ecall
  addi a0, a0, 1
  sw a0, @.__len__(s1)
  add t0, a0, s1
  li t1, 10
  sb t1, @.__str__-1(t0)
  sb zero, @.__str__(t0)
  mv a0, s1
input_done:
  lw s1, -12(fp)
  lw ra, -4(fp)
  lw fp, -8(fp)
  addi sp, sp, 16
  jr ra

.globl _contains
_contains:
  addi sp, sp, -@contains.size             # Reserve space for stack frame.
  sw ra, @contains.size-4(sp)              # return address
  sw fp, @contains.size-8(sp)              # control link
  addi fp, sp, @contains.size              # New fp is at old SP.
  li a0, 0                                 # Load integer literal 0
  sw a0, -12(fp)                           # local variable i
  j label_6                                # Jump to loop test
label_5:                                   # Top of while loop
  lw a0, 4(fp)                             # Load var: contains.items
  sw a0, -16(fp)                           # Push on stack slot 4
  lw a0, -12(fp)                           # Load var: contains.i
  lw a1, -16(fp)                           # Pop stack slot 4
  bnez a1, label_8                         # Ensure not None
  j error.None                             # Go to error handler
label_8:                                   # Not None
  lw t0, 12(a1)                            # Load attribute: __len__
  bltu a0, t0, label_9                     # Ensure 0 <= index < len
  j error.OOB                              # Go to error handler
label_9:                                   # Index within bounds
  addi a0, a0, 4                           # Compute list element offset in words
  li t0, 4                                 # Word size in bytes
  mul a0, a0, t0                           # Compute list element offset in bytes
  add a0, a1, a0                           # Pointer to list element
  lw a0, 0(a0)                             # Get list element
  sw a0, -16(fp)                           # Push on stack slot 4
  lw a0, 0(fp)                             # Load var: contains.x
  lw t0, -16(fp)                           # Pop stack slot 4
  bne t0, a0, label_7                      # Branch on not ==
  li a0, 1                                 # Load boolean literal: true
  j label_4                                # Go to return
label_7:                                   # End of if-else statement
  lw a0, -12(fp)                           # Load var: contains.i
  sw a0, -16(fp)                           # Push on stack slot 4
  li a0, 1                                 # Load integer literal 1
  lw t0, -16(fp)                           # Pop stack slot 4
  add a0, t0, a0                           # Operator +
  sw a0, -12(fp)                           # Assign var: contains.i
label_6:                                   # Test loop condition
  lw a0, -12(fp)                           # Load var: contains.i
  sw a0, -16(fp)                           # Push on stack slot 4
  lw a0, 4(fp)                             # Load var: contains.items
  sw a0, -32(fp)                           # Push argument 0 from last.
  addi sp, fp, -32                         # Set SP to last argument.
  jal _len                                 # Invoke function: len
  addi sp, fp, -@contains.size             # Set SP to stack frame top.
  lw t0, -16(fp)                           # Pop stack slot 4
  blt t0, a0, label_5                      # Branch on <
  li a0, 0                                 # Load boolean literal: false
  j label_4                                # Go to return
  mv a0, zero                              # Load None
  j label_4                                # Jump to function epilogue
label_4:                                   # Epilogue
  .equiv @contains.size, 32
  lw ra, -4(fp)                            # Get return address
  lw fp, -8(fp)                            # Use control link to restore caller's fp
  addi sp, sp, @contains.size              # Restore stack pointer
  jr ra                                    # Return to caller

.globl alloc
alloc:
# Runtime support function alloc.
        # Prototype address is in a0.
  lw a1, 4(a0)                             # Get size of object in words
  j alloc2                                 # Allocate object with exact size

.globl alloc2
alloc2:
# Runtime support function alloc2 (realloc).
        # Prototype address is in a0.
        # Number of words to allocate is in a1.
  li a2, 4                                 # Word size in bytes
  mul a2, a1, a2                           # Calculate number of bytes to allocate
  add a2, gp, a2                           # Estimate where GP will move
  bgeu a2, s11, alloc2_15                  # Go to OOM handler if too large
  lw t0, @.__obj_size__(a0)                # Get size of object in words
  mv t2, a0                                # Initialize src ptr
  mv t3, gp                                # Initialize dest ptr
alloc2_16:                                 # Copy-loop header
  lw t1, 0(t2)                             # Load next word from src
  sw t1, 0(t3)                             # Store next word to dest
  addi t2, t2, 4                           # Increment src
  addi t3, t3, 4                           # Increment dest
  addi t0, t0, -1                          # Decrement counter
  bne t0, zero, alloc2_16                  # Loop if more words left to copy
  mv a0, gp                                # Save new object's address to return
  sw a1, @.__obj_size__(a0)                # Set size of new object in words
                                           # (same as requested size)
  mv gp, a2                                # Set next free slot in the heap
  jr ra                                    # Return to caller
alloc2_15:                                 # OOM handler
  li a0, @error_oom                        # Exit code for: Out of memory
  la a1, const_7                           # Load error message as str
  addi a1, a1, @.__str__                   # Load address of attribute __str__
  j abort                                  # Abort

.globl abort
abort:
# Runtime support function abort (does not return).
  mv t0, a0                                # Save exit code in temp
  li a0, @print_string                     # Code for print_string ecall
  ecall                                    # Print error message in a1
  li a1, 10                                # Load newline character
  li a0, @print_char                       # Code for print_char ecall
  ecall                                    # Print newline
  mv a1, t0                                # Move exit code to a1
  li a0, @exit2                            # Code for exit2 ecall
  ecall                                    # Exit with code
abort_17:                                  # Infinite loop
  j abort_17                               # Prevent fallthrough

.globl heap.init
heap.init:
# Runtime support function heap.init.
  mv a1, a0                                # Move requested size to A1
  li a0, @sbrk                             # Code for ecall: sbrk
  ecall                                    # Request A1 bytes
  jr ra                                    # Return to caller

.globl concat
concat:

        addi sp, sp, -32
        sw ra, 28(sp)
        sw fp, 24(sp)
        addi fp, sp, 32
	sw s1, -12(fp)
        sw s2, -16(fp)
        sw s3, -20(fp)
	sw s4, -24(fp)
        sw s5, -28(fp)
        lw t0, 4(fp)
        lw t1, 0(fp)
        beqz t0, concat_none
        beqz t1, concat_none
        lw t0, @.__len__(t0)
        lw t1, @.__len__(t1)
        add s5, t0, t1
        addi a1, s5, @listHeaderWords
        la a0, _.list_prototype
        jal alloc2
        sw s5, @.__len__(a0)
	mv s5, a0
        addi s3, s5, @.__elts__
        lw s1, 4(fp)
	lw s2, @.__len__(s1)
        addi s1, s1, @.__elts__
	lw s4, 12(fp)
concat_1:
        beqz s2, concat_2
        lw a0, 0(s1)
	jalr ra, s4, 0
        sw a0, 0(s3)
        addi s2, s2, -1
        addi s1, s1, 4
        addi s3, s3, 4
        j concat_1
concat_2:
        lw s1, 0(fp)
        lw s2, @.__len__(s1)
        addi s1, s1, @.__elts__
	lw s4, 8(fp)
concat_3:
        beqz s2, concat_4
        lw a0, 0(s1)
	jalr ra, s4, 0
        sw a0, 0(s3)
        addi s2, s2, -1
        addi s1, s1, 4
        addi s3, s3, 4
        j concat_3
concat_4:
	mv a0, s5
        lw s1, -12(fp)
        lw s2, -16(fp)
        lw s3, -20(fp)
	lw s4, -24(fp)
        lw s5, -28(fp)
        lw ra, -4(fp)
        lw fp, -8(fp)
        addi sp, sp, 32
        jr ra
concat_none:
        j error.None


.globl conslist
conslist:

        addi sp, sp, -8
        sw ra, 4(sp)
        sw fp, 0(sp)
        addi fp, sp, 8
        lw a1, 0(fp)
        la a0, _.list_prototype
        beqz a1, conslist_done
        addi a1, a1, @listHeaderWords
        jal alloc2
        lw t0, 0(fp)
        sw t0, @.__len__(a0)
        slli t1, t0, 2
        add t1, t1, fp
        addi t2, a0, @.__elts__
conslist_1:
        lw t3, 0(t1)
        sw t3, 0(t2)
        addi t1, t1, -4
        addi t2, t2, 4
        addi t0, t0, -1
        bnez t0, conslist_1
conslist_done:
        lw ra, -4(fp)
        lw fp, -8(fp)
        addi sp, sp, 8
        jr ra


.globl strcat
strcat:

        addi sp, sp, -12
        sw ra, 8(sp)
        sw fp, 4(sp)
        addi fp, sp, 12
        lw t0, 4(fp)
        lw t1, 0(fp)
        lw t0, @.__len__(t0)
        beqz t0, strcat_4
        lw t1, @.__len__(t1)
        beqz t1, strcat_5
        add t1, t0, t1
        sw t1, -12(fp)
        addi t1, t1, 4
        srli t1, t1, 2
        addi a1, t1, @listHeaderWords
        la a0, _str_prototype
        jal alloc2
        lw t0, -12(fp)
        sw t0, @.__len__(a0)
        addi t2, a0, 16
        lw t0, 4(fp)
        lw t1, @.__len__(t0)
        addi t0, t0, @.__str__
strcat_1:
        beqz t1, strcat_2
        lbu t3, 0(t0)
        sb t3, 0(t2)
        addi t1, t1, -1
        addi t0, t0, 1
        addi t2, t2, 1
        j strcat_1
strcat_2:
        lw t0, 0(fp)
        lw t1, 12(t0)
        addi t0, t0, 16
strcat_3:
        beqz t1, strcat_6
        lbu t3, 0(t0)
        sb t3, 0(t2)
        addi t1, t1, -1
        addi t0, t0, 1
        addi t2, t2, 1
        j strcat_3
strcat_4:
        lw a0, 0(fp)
        j strcat_7
strcat_5:
        lw a0, 4(fp)
        j strcat_7
strcat_6:
        sb zero, 0(t2)
strcat_7:
        lw ra, -4(fp)
        lw fp, -8(fp)
        addi sp, sp, 12
        jr ra


.globl streql
streql:

        addi sp, sp, -8
        sw ra, 4(sp)
        sw fp, 0(sp)
        addi fp, sp, 8
        lw a1, 4(fp)
        lw a2, 0(fp)
        lw t0, @.__len__(a1)
        lw t1, @.__len__(a2)
        bne t0, t1, streql_no
streql_1:
        lbu t2, @.__str__(a1)
        lbu t3, @.__str__(a2)
        bne t2, t3, streql_no
        addi a1, a1, 1
        addi a2, a2, 1
        addi t0, t0, -1
        bgtz t0, streql_1
        li a0, 1
        j streql_end
streql_no:
        xor a0, a0, a0
streql_end:
        lw ra, -4(fp)
        lw fp, -8(fp)
        addi sp, sp, 8
        jr ra


.globl strneql
strneql:

        addi sp, sp, -8
        sw ra, 4(sp)
        sw fp, 0(sp)
        addi fp, sp, 8
        lw a1, 4(fp)
        lw a2, 0(fp)
        lw t0, @.__len__(a1)
        lw t1, @.__len__(a2)
        bne t0, t1, strneql_yes
strneql_1:
        lbu t2, @.__str__(a1)
        lbu t3, @.__str__(a2)
        bne t2, t3, strneql_yes
        addi a1, a1, 1
        addi a2, a2, 1
        addi t0, t0, -1
        bgtz t0, strneql_1
        xor a0, a0, a0
        j strneql_end
strneql_yes:
        li a0, 1
strneql_end:
        lw ra, -4(fp)
        lw fp, -8(fp)
        addi sp, sp, 8
        jr ra


.globl makeint
makeint:

        addi sp, sp, -8
        sw ra, 4(sp)
        sw a0, 0(sp)
        la a0, _int_prototype
        jal ra, alloc
        lw t0, 0(sp)
        sw t0, @.__int__(a0)
        lw ra, 4(sp)
        addi sp, sp, 8
        jr ra


.globl makebool
makebool:

	slli a0, a0, 4
        la t0, @bool.False
        add a0, a0, t0
	jr ra


.globl noconv
noconv:

        jr ra


.globl initchars
initchars:

        jr ra


.globl error.None
error.None:
  li a0, 4                                 # Exit code for: Operation on None
  la a1, const_8                           # Load error message as str
  addi a1, a1, 16                          # Load address of attribute __str__
  j abort                                  # Abort

.globl error.Div
error.Div:
  li a0, 2                                 # Exit code for: Division by zero
  la a1, const_9                           # Load error message as str
  addi a1, a1, 16                          # Load address of attribute __str__
  j abort                                  # Abort

.globl error.OOB
error.OOB:
  li a0, 3                                 # Exit code for: Index out of bounds
  la a1, const_10                          # Load error message as str
  addi a1, a1, 16                          # Load address of attribute __str__
  j abort                                  # Abort

.data

.globl const_0
const_0:
  .word 2                                  # Type tag for class: bool
  .word 4                                  # Object size
  .word _bool_dispatchTable                # Pointer to dispatch table
  .word 0                                  # Constant value of attribute: __bool__
  .align 2

.globl const_1
const_1:
  .word 2                                  # Type tag for class: bool
  .word 4                                  # Object size
  .word _bool_dispatchTable                # Pointer to dispatch table
  .word 1                                  # Constant value of attribute: __bool__
  .align 2

.globl const_3
const_3:
  .word 3                                  # Type tag for class: str
  .word 8                                  # Object size
  .word _str_dispatchTable                 # Pointer to dispatch table
  .word 15                                 # Constant value of attribute: __len__
  .string "Item not found."                # Constant value of attribute: __str__
  .align 2

.globl const_9
const_9:
  .word 3                                  # Type tag for class: str
  .word 9                                  # Object size
  .word _str_dispatchTable                 # Pointer to dispatch table
  .word 16                                 # Constant value of attribute: __len__
  .string "Division by zero"               # Constant value of attribute: __str__
  .align 2

.globl const_7
const_7:
  .word 3                                  # Type tag for class: str
  .word 8                                  # Object size
  .word _str_dispatchTable                 # Pointer to dispatch table
  .word 13                                 # Constant value of attribute: __len__
  .string "Out of memory"                  # Constant value of attribute: __str__
  .align 2

.globl const_10
const_10:
  .word 3                                  # Type tag for class: str
  .word 9                                  # Object size
  .word _str_dispatchTable                 # Pointer to dispatch table
  .word 19                                 # Constant value of attribute: __len__
  .string "Index out of bounds"            # Constant value of attribute: __str__
  .align 2

.globl const_5
const_5:
  .word 3                                  # Type tag for class: str
  .word 6                                  # Object size
  .word _str_dispatchTable                 # Pointer to dispatch table
  .word 4                                  # Constant value of attribute: __len__
  .string "True"                           # Constant value of attribute: __str__
  .align 2

.globl const_8
const_8:
  .word 3                                  # Type tag for class: str
  .word 9                                  # Object size
  .word _str_dispatchTable                 # Pointer to dispatch table
  .word 17                                 # Constant value of attribute: __len__
  .string "Operation on None"              # Constant value of attribute: __str__
  .align 2

.globl const_2
const_2:
  .word 3                                  # Type tag for class: str
  .word 7                                  # Object size
  .word _str_dispatchTable                 # Pointer to dispatch table
  .word 11                                 # Constant value of attribute: __len__
  .string "Item found!"                    # Constant value of attribute: __str__
  .align 2

.globl const_4
const_4:
  .word 3                                  # Type tag for class: str
  .word 9                                  # Object size
  .word _str_dispatchTable                 # Pointer to dispatch table
  .word 16                                 # Constant value of attribute: __len__
  .string "Invalid argument"               # Constant value of attribute: __str__
  .align 2

.globl const_6
const_6:
  .word 3                                  # Type tag for class: str
  .word 6                                  # Object size
  .word _str_dispatchTable                 # Pointer to dispatch table
  .word 5                                  # Constant value of attribute: __len__
  .string "False"                          # Constant value of attribute: __str__
  .align 2
