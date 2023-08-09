Nova
===========
This implementation is based on Supernova https://eprint.iacr.org/2022/1758, a Non-Uniform IVC scheme to `solve a stateful machine with a particular instruction set (e.g., EVM, RISC-V)` under R1CS.

This implemetation is based on https://github.com/microsoft/Nova  with toggling "supernova" features

> 07 Aug 2024 Update: supernova PR still under review https://github.com/microsoft/Nova/pull/204. Therefore temporarily depends on private repo https://github.com/hero78119/SuperNova/tree/supernova_trait_mut

### folding schemes of powdr-asm
Recursive/folding logical boundary is cut on per instruction defined in powdr-asm. Each instruction will form a relaxed running instance in supernova. Therefore, there will be totally `#inst` (relaxed R1CS) running instance. More accurately, it's `#inst + 1` running instance, for extra `+1` is for folding secondary circuit on primary circuit. Detail is omitted and encourge to check Microsoft Nova repo https://github.com/microsoft/Nova for how 2 cycle of curve was implemented

### augmented circuit
Each instruction is compiled into a step circuit, following Nova(Supernova) paper terminology, it's also called F circuit.
An augmented circuit := step circuit + nova folding verification circuit.
Furthermore, an augmented circuit has it own isolated constraints system, means there will be no shared circuit among different augmented circuits. Due to the fact, we can also call it instruction-circuit. There will be `#inst` instruction-circuit (More accurate, `#inst + 1` for 2 cycle curve implementation)

### Nova state & constraints
Nova state layout as z0 = `(pc, [writable register...] ++ ROM)`
where the ROM is defined as an array `[rom_value_pc1, rom_value_pc2, rom_value_pc3...]`
Each round an instruction is invoked, and in instruction-circuit it will constraints
1. sequence constraints => `zi[offset + pc] - linear-combination([opcode_index, input param1, input param2,...output param1, ...], 1 << limb_width) = 0`
2. writable register read/write are value are constraint and match.

> While which instruction-circuit is invoked  determined by prover, an maliculous prover can not invoke arbitrary any instruction-circuit, otherwise sequence constraints will be failed to pass `is_sat` check in the final stage.

### Sequence constraints
As mentioned, to constraints the sequence, a ROM array is introduced and attach at the end of Nova state. For input params in different type, the linear combination strategy will be adjust accordingly.

- `reg index`, i.e. x2. `2` will be treat as unsigned index and put into the value
- `sign/unsigned` const. For unsigned value will be put in lc directly. While signed part, it will be convert to signed limb, and on circuit side, signed limb will be convert to negative field value accordingly.
- `label`. i.e. `loop_start`, label will be convert to integer

Since each instruction circuit has it own params type definition, different constraints circuit will be compiled to handle above situation automatically.

### R1CS constraints
An augmented circuit can be viewed as a individual constraint system. PIL in powdr-asm instruction definition body will be compile into respective R1CS constraints. More detail, constraints can be categorized into 2 group
1. sequence constraints + writable register RW (or signed/unsigned/label) => this constraints will be insert into R1CS circuit automatically and transparent to powdr-asm PIL.
2. Powdr-asm PIL constraints: will be compiled into R1CS constraints

Giving simple example

```
machine NovaZero {

    ...
    instr incr X -> Y {
        Y = X + 1,
    }

    instr decr X -> Y {
        Y = X - 1
    }

    instr loop {
        pc' = pc
    }

    instr assert_zero X {
        X = 0
    }

    // the main function assigns the first prover input to A, increments it, decrements it, and loops forever
    function main {
        x0 <=Z= incr(x0); // x0' = 1
        x0 <=Y= decr(x0) // x0' = 0
        assert_zero x0; // x0 == 0
        loop;
    }
}
```

It will be compiled to below R1CS instance
```
// in incr instruction circuit
(X*1 + 1*1) * 1 = Y
...

// in decr instruction circuit
(x*1 + 1.inv()) * 1 = Y
...

// in assert_zero circuit
(X*1) * 1 = 0
...
```

Note that, the only way to pass data to next round is via writable register, so the `<reg>next` will be handled by instruction assignment automatically. There is forbidden to have `<reg>next` usage in instruction body. Only exception is `pc'` value, since the only place to mark next pc value is in instruction body.

The circuit to constraints `Writable Register` will be generate automatically to constraints
- `zi[end_of_reg_offset + pc] - linear-combination([opcode_index, input reg1, input reg2,..., output reg1, output reg2,...], 1 << limb_width) = 0 // in R1CS`
- `zi[input reg1] - assignemnt_reg_1 = 0 // constraints equality with respective PIL polynomial reference`
- `zi[input reg2] - assignemnt_reg_2 = 0 // ...`
- ...
- `zi[output reg1] - assignemnt_reg_3 = 0 // output is optional`
- ...

> For R1CS in the future, a more efficient `memory commitment` should be also part of the Nova state. For example, merkle tree root, or KZG vector commitment. This enable to constraints RAM Read/Write consistency. For KZG, potientially it can fit into R1CS folding scheme by random linear combination and defering the pairing to the last step then SNARK it. See `https://eprint.iacr.org/2019/1047.pdf` `9.2 Optimizations for the polynomial commitment scheme` for pairing linear combination on LHS/RHS which is potientially applicable to folding scheme.


### witness assignment
For 2 groups of constraints in a instruction circuit
- sequence constraints
- pil constrains

Powdr framework can infer and provide pil constrains, so in witness assigment we just reuse it.
For sequence constraints, it's infer via bellperson R1CS constraints system automatically, since it's transparent in PIL logic.

> An fact is, there are also constraints related to (Super)Nova verifier circuit. Its also automatically infer via `bellperson R1CS constraints system` automatically, and also encaptulated as blackbox