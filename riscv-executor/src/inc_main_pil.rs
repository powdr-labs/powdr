use powdr_ast::analyzed::Analyzed;
use powdr_number::{FieldElement, GoldilocksField};

use crate::Callback;

use num_traits::{One, Zero, ToBytes};

use std::collections::{BTreeMap, HashMap};

pub fn execute<F: FieldElement>(
    length: usize,
    fixed: HashMap<String, Vec<F>>,
) -> Vec<(String, Vec<F>)> {

    println!("keys: {:?}", fixed.keys());

    let mut ctx = Context::new(length).with_fixed(fixed);

    ctx.run();

    vec![
        ("main._operation_id".to_string(), ctx.operation_id),
        ("main.pc".to_string(), ctx.pc),
        ("main.X".to_string(), ctx.x),
        ("main.Y".to_string(), ctx.y),
        ("main.reg_write_X_A".to_string(), ctx.reg_write_x_a),
        ("main.A".to_string(), ctx.a),
        ("main.instr_inc".to_string(), ctx.instr_inc),
        ("main.instr_dec".to_string(), ctx.instr_dec),
        ("main.instr_assert_eq".to_string(), ctx.instr_assert_eq),
        ("main.instr__jump_to_operation".to_string(), ctx.instr_jump_to_operation),
        ("main.instr__reset".to_string(), ctx.instr_reset),
        ("main.instr__loop".to_string(), ctx.instr_loop),
        ("main.X_const".to_string(), ctx.x_const),
        ("main.read_X_A".to_string(), ctx.read_x_a),
        ("main.Y_const".to_string(), ctx.y_const),
        ("main.X_free_value".to_string(), ctx.x_free_value),
        ("main.Y_free_value".to_string(), ctx.y_free_value),
        ("main._operation_id_no_change".to_string(), ctx.operation_id_no_change),
    ]
}

#[derive(Default, Debug)]
struct Context<F> {
    pub length: usize,
    pub current_row: usize,
    pub running: bool,

    pub fixed: HashMap<String, Vec<F>>,

    pub pc: Vec<F>,
    pub x: Vec<F>,
    pub y: Vec<F>,
    pub a: Vec<F>,

    pub reg_write_x_a: Vec<F>,
    pub instr_inc: Vec<F>,
    pub instr_dec: Vec<F>,
    pub instr_assert_eq: Vec<F>,
    pub instr_jump_to_operation: Vec<F>,
    pub instr_reset: Vec<F>,
    pub instr_loop: Vec<F>,
    pub x_const: Vec<F>,
    pub read_x_a: Vec<F>,
    pub y_const: Vec<F>,
    pub x_free_value: Vec<F>,
    pub y_free_value: Vec<F>,
    pub operation_id: Vec<F>,
    pub operation_id_no_change: Vec<F>,
}

impl<F: FieldElement> Context<F> {
    pub fn new(length: usize) -> Self {
        Self {
            length,
            ..Default::default()
        }
    }

    pub fn with_fixed(mut self, fixed: HashMap<String, Vec<F>>) -> Self {
        self.fixed = fixed;
        self
    }

    pub fn run(&mut self) {
        self.running = true;

        self.init();

        while self.current_row < self.length {
            self.update();
            self.current_row += 1;

            if self.current_row >= 6 {
                self.running = false;
            }
        }

        // TODO fix
        *self.a.first_mut().unwrap() = self.a.last().unwrap().clone();
    }

    fn init(&mut self) {
        self.pc.push(F::zero());
        println!("push inside init");
        self.a.push(F::zero());
    }

    fn inc(&mut self) {
        let a = self.a.last().unwrap();
        println!("push inside inc");
        self.a.push(a.clone() + F::one());
    }

    fn dec(&mut self) {
        let a = self.a.last().unwrap();
        println!("push inside dec");
        self.a.push(a.clone() - F::one());
    }

    fn assert_eq(&mut self) {
        let x = self.x.last().unwrap();
        let y = self.y.last().unwrap();
        assert_eq!(x, y);
    }

    fn run_instructions(&mut self) {
        let instr_inc = self.instr_inc.last().unwrap();
        let instr_dec = self.instr_dec.last().unwrap();
        let instr_assert_eq = self.instr_assert_eq.last().unwrap();
        // we ignore instr_jump_to_operation and instr_loop
        // because pc uses them directly

        if instr_inc == &F::one() {
            self.inc();
        } else if instr_dec == &F::one() {
            self.dec();
        } else if instr_assert_eq == &F::one() {
            self.assert_eq();
        } else {
            //println!("{:?}", self);
            //panic!("unknown instruction");
        }
    }

    fn update(&mut self) {
        // order matters here:
        // - the starting point is pc = 0, state registers = 0
        // - update the control flow flags
        // - update the instruction flags
        // - update writes from state registers into assignment registers
        // - run the instructions
        // - update writes from assignment registers into state registers
        // - all assignment registers should have been updated by now
        // - update pc

        self.update_inputs();
        self.update_control_flow_flags();
        self.update_flags();
        self.update_writes_to_assignment_registers();

        if self.current_row < self.length - 1 {
            self.run_instructions();
            self.update_writes_to_state_registers();
            self.update_pc();
        }

        println!("a = {:?}", self.a);
    }

    fn update_inputs(&mut self) {
        self.x_free_value.push(F::zero());
        self.y_free_value.push(F::zero());
    }

    fn update_writes_to_assignment_registers(&mut self) {
        let mut x_prime = self.x_const.last().unwrap().clone();

        let read_x_a = self.read_x_a.last().unwrap();

        if read_x_a == &F::one() {
            x_prime += *self.a.last().unwrap();
        }

        self.x.push(x_prime);

        self.y.push(self.y_const.last().unwrap().clone());
    }

    fn update_writes_to_state_registers(&mut self) {
        let reg_write_x_a = self.reg_write_x_a.last().unwrap();
        let instr_inc = self.instr_inc.last().unwrap();
        let instr_dec = self.instr_dec.last().unwrap();
        let instr_touches_a = instr_inc == &F::one() || instr_dec == &F::one();

        if !instr_touches_a {
            let a = self.a.last().cloned().unwrap_or_else(|| F::zero().clone());

            let a_prime = if reg_write_x_a == &F::one() {
                self.x.last().unwrap().clone()
            } else {
                a
            };

            println!("a_prime inside updates = {:?}", a_prime);
            self.a.push(a_prime);
        }
    }

    fn update_pc(&mut self) {
        let pc = self.pc.last().unwrap();
        let pc_prime = if self.instr_jump_to_operation.last().unwrap().is_one() {
            self.operation_id.last().unwrap().clone()
        } else if self.instr_loop.last().unwrap().is_one() {
            pc.clone()
        } else {
            pc.clone() + F::one()
        };
        self.pc.push(pc_prime);
    }

    fn update_control_flow_flags(&mut self) {
        self.operation_id.push(F::from(2));

        if self.current_row < self.length - 1 {
            self.operation_id_no_change.push(F::one());
        } else {
            self.operation_id_no_change.push(F::zero());
        }

        if self.current_row == 0 {
            self.instr_reset.push(F::one());
        } else {
            self.instr_reset.push(F::zero());
        }

        if self.running {
            self.instr_loop.push(F::zero());
        } else {
            self.instr_loop.push(F::one());
        }
    }

    fn update_flags(&mut self) {
        let pc = self
            .pc
            .last()
            .unwrap()
            .to_arbitrary_integer()
            .to_le_bytes();
        let pc = u32::from_le_bytes([
            *pc.get(0).unwrap_or_else(|| &0),
            *pc.get(1).unwrap_or_else(|| &0),
            *pc.get(2).unwrap_or_else(|| &0),
            *pc.get(3).unwrap_or_else(|| &0),
        ]);
        println!("pc: {pc}");

        self.reg_write_x_a
            .push(self.fixed.get("main.p_reg_write_X_A").unwrap()[pc as usize]);
        self.instr_inc
            .push(self.fixed.get("main.p_instr_inc").unwrap()[pc as usize]);
        self.instr_dec
            .push(self.fixed.get("main.p_instr_dec").unwrap()[pc as usize]);
        self.instr_assert_eq
            .push(self.fixed.get("main.p_instr_assert_eq").unwrap()[pc as usize]);
        self.instr_jump_to_operation
            .push(self.fixed.get("main.p_instr__jump_to_operation").unwrap()[pc as usize]);
        self.x_const
            .push(self.fixed.get("main.p_X_const").unwrap()[pc as usize]);
        self.read_x_a
            .push(self.fixed.get("main.p_read_X_A").unwrap()[pc as usize]);
        self.y_const
            .push(self.fixed.get("main.p_Y_const").unwrap()[pc as usize]);
        //self.x_free_value = self.fixed.get("main.p_X_free_value").unwrap()[pc as usize];
        //self.y_free_value = self.fixed.get("main.p_Y_free_value").unwrap()[pc as usize];
    }

    /*
    fn update_a(&mut self) {
        if self.current_row == 0 {
            self.a.push(F::zero());
            return;
        }

        let a = self.a.last().unwrap();
        let a_prime = if self.reg_write_x_a.last().unwrap().is_one() {
            self.x.last().unwrap().clone()
        } else if self.instr_inc.last().unwrap().is_one() {
            a.clone() + F::one()
        } else if self.instr_dec.last().unwrap().is_one() {
            a.clone() - F::one()
        } else {
            a.clone()
        };
        self.a.push(a_prime);
    }
    */
}
