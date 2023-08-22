# Instructions

Instructions are declared as part of a powdr virtual machine. Their inputs and outputs are [assignment registers](./registers.md) as well as labels. Once defined, they can be called by any function in this machine.

# Local instructions

A local instruction is the simplest type of instruction. It is called local because its behavior is defined using constraints over registers and columns of the machine it is defined in.

```
instr add X, Y -> Z {
    X + Y = Z
}
```

Instructions feature:
- a name
- some inputs
- some outputs
- a set of PIL constraints to activate when the instruction is called

# External instructions

An external instruction delegates calls to a function inside a submachine of this machine. When it is called, a call is made to the submachine function. An example of an external instruction is the following:

```
instr assert_zero X = my_submachine.assert_zero // where `assert_zero` is a function defined in `my_submachine`
```

> Note that external instructions cannot currently link to functions of the same machine: they delegate computation to a submachine.

