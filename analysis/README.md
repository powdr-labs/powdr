# Analysis

This crate is where most of the compilation pipeline lives. It takes a parse tree and returns a tree of constrained machines.

## Definitions

We define two types of machines: virtual machines and constrained machines. Constrained machines are the lower level kind of machine. They have a notion of blocks through a latch and a function_id. Virtual machines are a higher level type of machines. For each machine type, we provide the elements which only appear exclusively in that type.

|                         | Virtual | Constrained |
|-------------------------|---------|-------------|
| pc                      | yes     | no          |
| latch                   | no      | yes         |
| function_id             | no      | yes         |
| functions bodies        | yes     | no          |
| internal instructions   | yes     | no          |

The pipeline accepts both kinds of machines, and they are represented by the same type `Machine`. Some steps are specific to virtual machines. They can still be applied to constrained machines and must have no effect. In the process, virtual machines get reduced to constrained machines by encoding their high-level elements into constrained machines elements.

## Pipeline

We go through the pipeline using the following program as an example:

```
machine Main {

    degree 16;

    DifferentSignatures sub;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];

    instr identity X -> Y = sub.identity
    instr one -> Y = sub.one
    instr nothing = sub.nothing

    function main {
        nothing;
        return;
    }
}

// A machine exposing functions of different signatures
machine DifferentSignatures {

    reg pc[@pc];

    function identity x: field -> field {
        return x;
    }

    function one -> field {
        return 1;
    }

    function nothing {
        return;
    }
}
```

### Type checking

Type checking takes a parse tree and returns a tree of machines. The output type aims at being as strict as possible.

### Virtual machine reduction

Virtual machine reduction turns virtual machines into constrained machines. It has no effect on contrained machines.

#### Inference

Inference infers the value of assignment registers.

```diff
-               A <== one();
+               A <=Y= one();
```

#### Batcher

The batcher groups statements within each function into batches which can be executed in the same execution step. In our example, we gain 33% in the main machine by batching the first label with the following statement.

```diff
+               // END BATCH
+               // END BATCH
+               // END BATCH
+               // END BATCH Unimplemented
+               // END BATCH
```

#### ASM to PIL

ASM to PIL has two steps: ROM generation and reduction to constrained.

##### ROM generation

Rom generation generates a single ROM for each virtual machine using the following process:
- Find the maximum number of inputs among all functions. Introduce as many input registers. Do the same for outputs, introducing output registers
- Replace references to the function arguments by references to these input registers.
- Pad all return statements with zeroes up to the number of output registers.
- Inline all functions, adding a label before each one.
- Add an infinite loop also behind a label
- Enable non-deterministically jumping to one of these labels by setting `function_id` to the index of the label

This process introduces instructions and registers, and returns the modified machine along with the ROM.

##### VM to constrained

Once we have the ROM for a machine, we reduce it to constraints. Some parts of this process are specific to our dispatcher implementation:
- All input registers are unconstrained when `_reset` is on, allowing to pass inputs.
- `return` is treated as an instruction even though it is not declared as such

As a result, we obtain a constrained machine for each original virtual machine. The `function_id` and `latch` are set using respectively the location of the machine functions inside the ROM and the instruction flag of `return`.

The diff for our example program is as follows:
```diff
-machine DifferentSignatures {
// registers are removed and encoded as constraints
-       reg pc[@pc];
// functions bodies are removed and the function id is set, see down below
-       function identity x: field -> field {
-               return x;
-               // END BATCH
// the latch and function id are set for virtual machines
+machine DifferentSignatures(instr_return, _function_id) {
+       constraints {
+               pol commit _function_id;
// we introduce some constraints to help witness generation: they enforce that the first block is an execution of any function, and the second is the sink (infinite loop) on line `5` in the ROM. These will be removed in the near future.
+               pol commit _sigma;
+               pol constant _romgen_first_step = [1] + [0]*;
+               _sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
+               (_sigma * (_function_id - 5)) = 0;
-       function one  -> field {
-               return 1;
-               // END BATCH
+
// we encode the virtual machine in constraints
+       constraints {
+               pol commit pc;
+               pol commit _input_0;
+               pol commit _output_0;
+               pol commit instr__jump_to_operation;
+               pol commit instr__reset;
+               pol commit instr__loop;
+               pol commit instr_return;
+               pol commit _output_0_const;
+               pol commit _output_0_read_free;
+               pol commit read__output_0_pc;
+               pol commit read__output_0__input_0;
+               _output_0 = ((((read__output_0_pc * pc) + (read__output_0__input_0 * _input_0)) + _output_0_const) + (_output_0_read_free * _output_0_free_value));
+               pol constant first_step = [1] + [0]*;
+               ((1 - instr__reset) * _input_0') = ((1 - instr__reset) * _input_0);
+               pc' = ((1 - first_step') * ((((instr__jump_to_operation * _function_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
+               pol constant p_line = [0, 1, 2, 3, 4, 5] + [5]*;
+               pol commit _output_0_free_value(i) query match pc {  };
+               pol constant p__output_0_const = [0, 0, 0, 1, 0, 0] + [0]*;
+               pol constant p__output_0_read_free = [0, 0, 0, 0, 0, 0] + [0]*;
+               pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0, 0] + [0]*;
+               pol constant p_instr__loop = [0, 0, 0, 0, 0, 1] + [1]*;
+               pol constant p_instr__reset = [1, 0, 0, 0, 0, 0] + [0]*;
+               pol constant p_instr_return = [0, 0, 1, 1, 1, 0] + [0]*;
+               pol constant p_read__output_0__input_0 = [0, 0, 1, 0, 0, 0] + [0]*;
+               pol constant p_read__output_0_pc = [0, 0, 0, 0, 0, 0] + [0]*;
+               { pc, instr__jump_to_operation, instr__reset, instr__loop, instr_return, _output_0_const, _output_0_read_free, read__output_0_pc, read__output_0__input_0 } in { p_line, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p__output_0_const, p__output_0_read_free, p_read__output_0_pc, p_read__output_0__input_0 };
-       function nothing {
-               return;
-               // END BATCH
+
// the body of functions is removed and they are assigned their function id. Their inputs and outputs are mapped to the relevant registers
+       function identity<2> _input_0 -> _output_0 {
+
+       function one<3>  -> _output_0 {
+
+       }
+       function nothing<4> {
+
+       }
// we do the same for the main machine
-machine Main {
+machine Main(instr_return, _function_id) {
-       reg pc[@pc];
-       reg X[<=];
-       reg Y[<=];
-       reg A;
+       constraints {
+               pol commit _function_id;
+               pol commit _sigma;
+               pol constant _romgen_first_step = [1] + [0]*;
+               _sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
+               (_sigma * (_function_id - 4)) = 0;
+       }
+
+       constraints {
+               pol commit pc;
+               pol commit X;
+               pol commit Y;
+               pol commit reg_write_X_A;
+               pol commit reg_write_Y_A;
+               pol commit A;
+               pol commit instr_identity;
+               pol commit instr_one;
+               pol commit instr_nothing;
+               pol commit instr__jump_to_operation;
+               pol commit instr__reset;
+               pol commit instr__loop;
+               pol commit instr_return;
+               pol commit X_const;
+               pol commit X_read_free;
+               pol commit read_X_A;
+               pol commit read_X_pc;
+               X = ((((read_X_A * A) + (read_X_pc * pc)) + X_const) + (X_read_free * X_free_value));
+               pol commit Y_const;
+               pol commit Y_read_free;
+               pol commit read_Y_A;
+               pol commit read_Y_pc;
+               Y = ((((read_Y_A * A) + (read_Y_pc * pc)) + Y_const) + (Y_read_free * Y_free_value));
+               pol constant first_step = [1] + [0]*;
+               A' = ((((reg_write_X_A * X) + (reg_write_Y_A * Y)) + (instr__reset * 0)) + ((1 - ((reg_write_X_A + reg_write_Y_A) + instr__reset)) * A));
+               pc' = ((1 - first_step') * ((((instr__jump_to_operation * _function_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
+               pol constant p_line = [0, 1, 2, 3, 4] + [4]*;
+               pol commit X_free_value(i) query match pc {  };
+               pol commit Y_free_value(i) query match pc {  };
+               pol constant p_X_const = [0, 0, 0, 0, 0] + [0]*;
+               pol constant p_X_read_free = [0, 0, 0, 0, 0] + [0]*;
+               pol constant p_Y_const = [0, 0, 0, 0, 0] + [0]*;
+               pol constant p_Y_read_free = [0, 0, 1, 0, 0] + [0]*;
+               pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0] + [0]*;
+               pol constant p_instr__loop = [0, 0, 0, 0, 1] + [1]*;
+               pol constant p_instr__reset = [1, 0, 0, 0, 0] + [0]*;
+               pol constant p_instr_identity = [0, 0, 0, 0, 0] + [0]*;
+               pol constant p_instr_nothing = [0, 0, 0, 0, 0] + [0]*;
+               pol constant p_instr_one = [0, 0, 1, 0, 0] + [0]*;
+               pol constant p_instr_return = [0, 0, 0, 1, 0] + [0]*;
+               pol constant p_read_X_A = [0, 0, 0, 0, 0] + [0]*;
+               pol constant p_read_X_pc = [0, 0, 0, 0, 0] + [0]*;
+               pol constant p_read_Y_A = [0, 0, 0, 0, 0] + [0]*;
+               pol constant p_read_Y_pc = [0, 0, 0, 0, 0] + [0]*;
+               pol constant p_reg_write_X_A = [0, 0, 0, 0, 0] + [0]*;
+               pol constant p_reg_write_Y_A = [0, 0, 1, 0, 0] + [0]*;
+               { pc, reg_write_X_A, reg_write_Y_A, instr_identity, instr_one, instr_nothing, instr__jump_to_operation, instr__reset, instr__loop, instr_return, X_const, X_read_free, read_X_A, read_X_pc, Y_const, Y_read_free, read_Y_A, read_Y_pc } in { p_line, p_reg_write_X_A, p_reg_write_Y_A, p_instr_identity, p_instr_one, p_instr_nothing, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p_X_const, p_X_read_free, p_read_X_A, p_read_X_pc, p_Y_const, p_Y_read_free, p_read_Y_A, p_read_Y_pc };
+       }
+
-       function main {
-               start::
-               A <=Y= one();
-               // END BATCH Unimplemented
-               return;
-               // END BATCH
+       function main<2> {
+
```

### Block enforcer

This step takes constrained machines and enforces that the `function_id` can only change if the `latch` is on. This defines blocks of computation which can be created based on the functions exposed by each machine.

We add an identical block of constraints for each machine type:
```diff
+       constraints {
+               pol constant _block_enforcer_last_step = [0]* + [1];
+               pol commit _function_id_no_change;
+               _function_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
+               (_function_id_no_change * (_function_id' - _function_id)) = 0;
+       }
+
+       }
+
+       constraints {
+               pol constant _block_enforcer_last_step = [0]* + [1];
+               pol commit _function_id_no_change;
+               _function_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
+               (_function_id_no_change * (_function_id' - _function_id)) = 0;
```

### Airgen

Airgen takes machines which are only left with constraints and external instructions and instanciates them as a tree of AIR objects. Objects can point to each other using links, which encode the interaction between different machines.

The final program after analysis is the following:
```
machine DifferentSignatures(instr_return, _function_id) {
        constraints {
                pol commit _function_id;
                pol commit _sigma;
                pol constant _romgen_first_step = [1] + [0]*;
                _sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
                (_sigma * (_function_id - 5)) = 0;
        }

        constraints {
                pol commit pc;
                pol commit _input_0;
                pol commit _output_0;
                pol commit instr__jump_to_operation;
                pol commit instr__reset;
                pol commit instr__loop;
                pol commit instr_return;
                pol commit _output_0_const;
                pol commit _output_0_read_free;
                pol commit read__output_0_pc;
                pol commit read__output_0__input_0;
                _output_0 = ((((read__output_0_pc * pc) + (read__output_0__input_0 * _input_0)) + _output_0_const) + (_output_0_read_free * _output_0_free_value));
                pol constant first_step = [1] + [0]*;
                ((1 - instr__reset) * _input_0') = ((1 - instr__reset) * _input_0);
                pc' = ((1 - first_step') * ((((instr__jump_to_operation * _function_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
                pol constant p_line = [0, 1, 2, 3, 4, 5] + [5]*;
                pol commit _output_0_free_value(i) query match pc {  };
                pol constant p__output_0_const = [0, 0, 0, 1, 0, 0] + [0]*;
                pol constant p__output_0_read_free = [0, 0, 0, 0, 0, 0] + [0]*;
                pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0, 0] + [0]*;
                pol constant p_instr__loop = [0, 0, 0, 0, 0, 1] + [1]*;
                pol constant p_instr__reset = [1, 0, 0, 0, 0, 0] + [0]*;
                pol constant p_instr_return = [0, 0, 1, 1, 1, 0] + [0]*;
                pol constant p_read__output_0__input_0 = [0, 0, 1, 0, 0, 0] + [0]*;
                pol constant p_read__output_0_pc = [0, 0, 0, 0, 0, 0] + [0]*;
                { pc, instr__jump_to_operation, instr__reset, instr__loop, instr_return, _output_0_const, _output_0_read_free, read__output_0_pc, read__output_0__input_0 } in { p_line, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p__output_0_const, p__output_0_read_free, p_read__output_0_pc, p_read__output_0__input_0 };
        }

        constraints {
                pol constant _block_enforcer_last_step = [0]* + [1];
                pol commit _function_id_no_change;
                _function_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
                (_function_id_no_change * (_function_id' - _function_id)) = 0;
        }

        function identity<2> _input_0 -> _output_0 {

        }
        function one<3>  -> _output_0 {

        }
        function nothing<4> {

        }
}
machine Main(instr_return, _function_id) {
        degree 16;
        constraints {
                pol commit _function_id;
                pol commit _sigma;
                pol constant _romgen_first_step = [1] + [0]*;
                _sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
                (_sigma * (_function_id - 4)) = 0;
        }

        constraints {
                pol commit pc;
                pol commit X;
                pol commit Y;
                pol commit reg_write_X_A;
                pol commit reg_write_Y_A;
                pol commit A;
                pol commit instr_identity;
                pol commit instr_one;
                pol commit instr_nothing;
                pol commit instr__jump_to_operation;
                pol commit instr__reset;
                pol commit instr__loop;
                pol commit instr_return;
                pol commit X_const;
                pol commit X_read_free;
                pol commit read_X_A;
                pol commit read_X_pc;
                X = ((((read_X_A * A) + (read_X_pc * pc)) + X_const) + (X_read_free * X_free_value));
                pol commit Y_const;
                pol commit Y_read_free;
                pol commit read_Y_A;
                pol commit read_Y_pc;
                Y = ((((read_Y_A * A) + (read_Y_pc * pc)) + Y_const) + (Y_read_free * Y_free_value));
                pol constant first_step = [1] + [0]*;
                A' = ((((reg_write_X_A * X) + (reg_write_Y_A * Y)) + (instr__reset * 0)) + ((1 - ((reg_write_X_A + reg_write_Y_A) + instr__reset)) * A));
                pc' = ((1 - first_step') * ((((instr__jump_to_operation * _function_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
                pol constant p_line = [0, 1, 2, 3, 4] + [4]*;
                pol commit X_free_value(i) query match pc {  };
                pol commit Y_free_value(i) query match pc {  };
                pol constant p_X_const = [0, 0, 0, 0, 0] + [0]*;
                pol constant p_X_read_free = [0, 0, 0, 0, 0] + [0]*;
                pol constant p_Y_const = [0, 0, 0, 0, 0] + [0]*;
                pol constant p_Y_read_free = [0, 0, 1, 0, 0] + [0]*;
                pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0] + [0]*;
                pol constant p_instr__loop = [0, 0, 0, 0, 1] + [1]*;
                pol constant p_instr__reset = [1, 0, 0, 0, 0] + [0]*;
                pol constant p_instr_identity = [0, 0, 0, 0, 0] + [0]*;
                pol constant p_instr_nothing = [0, 0, 0, 0, 0] + [0]*;
                pol constant p_instr_one = [0, 0, 1, 0, 0] + [0]*;
                pol constant p_instr_return = [0, 0, 0, 1, 0] + [0]*;
                pol constant p_read_X_A = [0, 0, 0, 0, 0] + [0]*;
                pol constant p_read_X_pc = [0, 0, 0, 0, 0] + [0]*;
                pol constant p_read_Y_A = [0, 0, 0, 0, 0] + [0]*;
                pol constant p_read_Y_pc = [0, 0, 0, 0, 0] + [0]*;
                pol constant p_reg_write_X_A = [0, 0, 0, 0, 0] + [0]*;
                pol constant p_reg_write_Y_A = [0, 0, 1, 0, 0] + [0]*;
                { pc, reg_write_X_A, reg_write_Y_A, instr_identity, instr_one, instr_nothing, instr__jump_to_operation, instr__reset, instr__loop, instr_return, X_const, X_read_free, read_X_A, read_X_pc, Y_const, Y_read_free, read_Y_A, read_Y_pc } in { p_line, p_reg_write_X_A, p_reg_write_Y_A, p_instr_identity, p_instr_one, p_instr_nothing, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p_X_const, p_X_read_free, p_read_X_A, p_read_X_pc, p_Y_const, p_Y_read_free, p_read_Y_A, p_read_Y_pc };
        }

        constraints {
                pol constant _block_enforcer_last_step = [0]* + [1];
                pol commit _function_id_no_change;
                _function_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
                (_function_id_no_change * (_function_id' - _function_id)) = 0;
        }

        instr identity X -> Y { sub.identity }
        instr one  -> Y { sub.one }
        instr nothing { sub.nothing }
        function main<2> {

        }
}
```

It only contains constraints, external instructions and functions with no body. The result of running airgen is very similar, but contains specific instances of the different machines. External instructions have been turned into links to other instances.

```
// Object main
// Degree 16
pol commit _function_id;
pol commit _sigma;
pol constant _romgen_first_step = [1] + [0]*;
_sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
(_sigma * (_function_id - 4)) = 0;
pol commit pc;
pol commit X;
pol commit Y;
pol commit reg_write_X_A;
pol commit reg_write_Y_A;
pol commit A;
pol commit instr_identity;
pol commit instr_one;
pol commit instr_nothing;
pol commit instr__jump_to_operation;
pol commit instr__reset;
pol commit instr__loop;
pol commit instr_return;
pol commit X_const;
pol commit X_read_free;
pol commit read_X_A;
pol commit read_X_pc;
X = ((((read_X_A * A) + (read_X_pc * pc)) + X_const) + (X_read_free * X_free_value));
pol commit Y_const;
pol commit Y_read_free;
pol commit read_Y_A;
pol commit read_Y_pc;
Y = ((((read_Y_A * A) + (read_Y_pc * pc)) + Y_const) + (Y_read_free * Y_free_value));
pol constant first_step = [1] + [0]*;
A' = ((((reg_write_X_A * X) + (reg_write_Y_A * Y)) + (instr__reset * 0)) + ((1 - ((reg_write_X_A + reg_write_Y_A) + instr__reset)) * A));
pc' = ((1 - first_step') * ((((instr__jump_to_operation * _function_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4] + [4]*;
pol commit X_free_value(i) query match pc {  };
pol commit Y_free_value(i) query match pc {  };
pol constant p_X_const = [0, 0, 0, 0, 0] + [0]*;
pol constant p_X_read_free = [0, 0, 0, 0, 0] + [0]*;
pol constant p_Y_const = [0, 0, 0, 0, 0] + [0]*;
pol constant p_Y_read_free = [0, 0, 1, 0, 0] + [0]*;
pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0] + [0]*;
pol constant p_instr__loop = [0, 0, 0, 0, 1] + [1]*;
pol constant p_instr__reset = [1, 0, 0, 0, 0] + [0]*;
pol constant p_instr_identity = [0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_nothing = [0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_one = [0, 0, 1, 0, 0] + [0]*;
pol constant p_instr_return = [0, 0, 0, 1, 0] + [0]*;
pol constant p_read_X_A = [0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_pc = [0, 0, 0, 0, 0] + [0]*;
pol constant p_read_Y_A = [0, 0, 0, 0, 0] + [0]*;
pol constant p_read_Y_pc = [0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_A = [0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_Y_A = [0, 0, 1, 0, 0] + [0]*;
{ pc, reg_write_X_A, reg_write_Y_A, instr_identity, instr_one, instr_nothing, instr__jump_to_operation, instr__reset, instr__loop, instr_return, X_const, X_read_free, read_X_A, read_X_pc, Y_const, Y_read_free, read_Y_A, read_Y_pc } in { p_line, p_reg_write_X_A, p_reg_write_Y_A, p_instr_identity, p_instr_one, p_instr_nothing, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p_X_const, p_X_read_free, p_read_X_A, p_read_X_pc, p_Y_const, p_Y_read_free, p_read_Y_A, p_read_Y_pc };
pol constant _block_enforcer_last_step = [0]* + [1];
pol commit _function_id_no_change;
_function_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
(_function_id_no_change * (_function_id' - _function_id)) = 0;
// Links:
// // // instr identity with params  X -> Y links to // // function identity with id 2 with params  _input_0 -> _output_0 in object at location main_sub with latch "instr_return" and function_id "_function_id"
// // // instr one with params   -> Y links to // // function one with id 3 with params   -> _output_0 in object at location main_sub with latch "instr_return" and function_id "_function_id"
// // // instr nothing with params  links to // // function nothing with id 4 with params  in object at location main_sub with latch "instr_return" and function_id "_function_id"


// Object main_sub
pol commit _function_id;
pol commit _sigma;
pol constant _romgen_first_step = [1] + [0]*;
_sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
(_sigma * (_function_id - 5)) = 0;
pol commit pc;
pol commit _input_0;
pol commit _output_0;
pol commit instr__jump_to_operation;
pol commit instr__reset;
pol commit instr__loop;
pol commit instr_return;
pol commit _output_0_const;
pol commit _output_0_read_free;
pol commit read__output_0_pc;
pol commit read__output_0__input_0;
_output_0 = ((((read__output_0_pc * pc) + (read__output_0__input_0 * _input_0)) + _output_0_const) + (_output_0_read_free * _output_0_free_value));
pol constant first_step = [1] + [0]*;
((1 - instr__reset) * _input_0') = ((1 - instr__reset) * _input_0);
pc' = ((1 - first_step') * ((((instr__jump_to_operation * _function_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4, 5] + [5]*;
pol commit _output_0_free_value(i) query match pc {  };
pol constant p__output_0_const = [0, 0, 0, 1, 0, 0] + [0]*;
pol constant p__output_0_read_free = [0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0, 0] + [0]*;
pol constant p_instr__loop = [0, 0, 0, 0, 0, 1] + [1]*;
pol constant p_instr__reset = [1, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_return = [0, 0, 1, 1, 1, 0] + [0]*;
pol constant p_read__output_0__input_0 = [0, 0, 1, 0, 0, 0] + [0]*;
pol constant p_read__output_0_pc = [0, 0, 0, 0, 0, 0] + [0]*;
{ pc, instr__jump_to_operation, instr__reset, instr__loop, instr_return, _output_0_const, _output_0_read_free, read__output_0_pc, read__output_0__input_0 } in { p_line, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p__output_0_const, p__output_0_read_free, p_read__output_0_pc, p_read__output_0__input_0 };
pol constant _block_enforcer_last_step = [0]* + [1];
pol commit _function_id_no_change;
_function_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
(_function_id_no_change * (_function_id' - _function_id)) = 0;
```

### Linker

The linker takes a tree of machines and instanciates specific structures to make proofs about. In our current implementation, it simply collates all AIR objects next to each other in a single table.

For our example, it introduces two namespaces (one per machine instance) with the degree of the entry point (16), and turns the links into lookups between the two namespaces.

```
namespace main(16);
pol commit _function_id;
pol commit _sigma;
pol constant _romgen_first_step = [1] + [0]*;
_sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
(_sigma * (_function_id - 4)) = 0;
pol commit pc;
pol commit X;
pol commit Y;
pol commit reg_write_X_A;
pol commit reg_write_Y_A;
pol commit A;
pol commit instr_identity;
pol commit instr_one;
pol commit instr_nothing;
pol commit instr__jump_to_operation;
pol commit instr__reset;
pol commit instr__loop;
pol commit instr_return;
pol commit X_const;
pol commit X_read_free;
pol commit read_X_A;
pol commit read_X_pc;
X = ((((read_X_A * A) + (read_X_pc * pc)) + X_const) + (X_read_free * X_free_value));
pol commit Y_const;
pol commit Y_read_free;
pol commit read_Y_A;
pol commit read_Y_pc;
Y = ((((read_Y_A * A) + (read_Y_pc * pc)) + Y_const) + (Y_read_free * Y_free_value));
pol constant first_step = [1] + [0]*;
A' = ((((reg_write_X_A * X) + (reg_write_Y_A * Y)) + (instr__reset * 0)) + ((1 - ((reg_write_X_A + reg_write_Y_A) + instr__reset)) * A));
pc' = ((1 - first_step') * ((((instr__jump_to_operation * _function_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4] + [4]*;
pol commit X_free_value(i) query match pc {  };
pol commit Y_free_value(i) query match pc {  };
pol constant p_X_const = [0, 0, 0, 0, 0] + [0]*;
pol constant p_X_read_free = [0, 0, 0, 0, 0] + [0]*;
pol constant p_Y_const = [0, 0, 0, 0, 0] + [0]*;
pol constant p_Y_read_free = [0, 0, 1, 0, 0] + [0]*;
pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0] + [0]*;
pol constant p_instr__loop = [0, 0, 0, 0, 1] + [1]*;
pol constant p_instr__reset = [1, 0, 0, 0, 0] + [0]*;
pol constant p_instr_identity = [0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_nothing = [0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_one = [0, 0, 1, 0, 0] + [0]*;
pol constant p_instr_return = [0, 0, 0, 1, 0] + [0]*;
pol constant p_read_X_A = [0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_pc = [0, 0, 0, 0, 0] + [0]*;
pol constant p_read_Y_A = [0, 0, 0, 0, 0] + [0]*;
pol constant p_read_Y_pc = [0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_A = [0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_Y_A = [0, 0, 1, 0, 0] + [0]*;
{ pc, reg_write_X_A, reg_write_Y_A, instr_identity, instr_one, instr_nothing, instr__jump_to_operation, instr__reset, instr__loop, instr_return, X_const, X_read_free, read_X_A, read_X_pc, Y_const, Y_read_free, read_Y_A, read_Y_pc } in { p_line, p_reg_write_X_A, p_reg_write_Y_A, p_instr_identity, p_instr_one, p_instr_nothing, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p_X_const, p_X_read_free, p_read_X_A, p_read_X_pc, p_Y_const, p_Y_read_free, p_read_Y_A, p_read_Y_pc };
pol constant _block_enforcer_last_step = [0]* + [1];
pol commit _function_id_no_change;
_function_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
(_function_id_no_change * (_function_id' - _function_id)) = 0;
instr_identity { 2, X, Y } in main_sub.instr_return { main_sub._function_id, main_sub._input_0, main_sub._output_0 };
instr_one { 3, Y } in main_sub.instr_return { main_sub._function_id, main_sub._output_0 };
instr_nothing { 4 } in main_sub.instr_return { main_sub._function_id };
pol constant _linker_first_step = [1] + [0]*;
(_linker_first_step * (_function_id - 2)) = 0;
namespace main_sub(16);
pol commit _function_id;
pol commit _sigma;
pol constant _romgen_first_step = [1] + [0]*;
_sigma' = ((1 - _romgen_first_step') * (_sigma + instr_return));
(_sigma * (_function_id - 5)) = 0;
pol commit pc;
pol commit _input_0;
pol commit _output_0;
pol commit instr__jump_to_operation;
pol commit instr__reset;
pol commit instr__loop;
pol commit instr_return;
pol commit _output_0_const;
pol commit _output_0_read_free;
pol commit read__output_0_pc;
pol commit read__output_0__input_0;
_output_0 = ((((read__output_0_pc * pc) + (read__output_0__input_0 * _input_0)) + _output_0_const) + (_output_0_read_free * _output_0_free_value));
pol constant first_step = [1] + [0]*;
((1 - instr__reset) * _input_0') = ((1 - instr__reset) * _input_0);
pc' = ((1 - first_step') * ((((instr__jump_to_operation * _function_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4, 5] + [5]*;
pol commit _output_0_free_value(i) query match pc {  };
pol constant p__output_0_const = [0, 0, 0, 1, 0, 0] + [0]*;
pol constant p__output_0_read_free = [0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr__jump_to_operation = [0, 1, 0, 0, 0, 0] + [0]*;
pol constant p_instr__loop = [0, 0, 0, 0, 0, 1] + [1]*;
pol constant p_instr__reset = [1, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_return = [0, 0, 1, 1, 1, 0] + [0]*;
pol constant p_read__output_0__input_0 = [0, 0, 1, 0, 0, 0] + [0]*;
pol constant p_read__output_0_pc = [0, 0, 0, 0, 0, 0] + [0]*;
{ pc, instr__jump_to_operation, instr__reset, instr__loop, instr_return, _output_0_const, _output_0_read_free, read__output_0_pc, read__output_0__input_0 } in { p_line, p_instr__jump_to_operation, p_instr__reset, p_instr__loop, p_instr_return, p__output_0_const, p__output_0_read_free, p_read__output_0_pc, p_read__output_0__input_0 };
pol constant _block_enforcer_last_step = [0]* + [1];
pol commit _function_id_no_change;
_function_id_no_change = ((1 - _block_enforcer_last_step) * (1 - instr_return));
(_function_id_no_change * (_function_id' - _function_id)) = 0;
```