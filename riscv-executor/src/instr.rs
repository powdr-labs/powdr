use powdr_number::{FieldElement, LargeInt};

use crate::poseidon_gl;
use crate::Elem;

pub trait Proc<F: FieldElement> {
    fn get_pc(&self) -> Elem<F>;
    fn set_pc(&mut self, pc: Elem<F>);
    fn get_mem(&self, addr: u32) -> u32;
    fn set_mem(&mut self, addr: u32, val: u32);
    fn get_reg(&self, name: &str) -> Elem<F>;
    fn set_reg(&mut self, idx: &str, value: impl Into<Elem<F>>);
}

pub fn exec_instruction<F: FieldElement>(
    name: &str,
    args: &[Elem<F>],
    proc: &mut impl Proc<F>,
) -> Vec<Elem<F>> {
    //println!("\n\ninside exec_instruction");
    //println!("pc = {}", proc.get_pc().to_string());
    //println!("executing instruction: {}", name);
    match name {
        "mstore" | "mstore_bootloader" => {
            let addr = args[0].bin() as u32;
            assert_eq!(addr % 4, 0);
            proc.set_mem(addr, args[1].u());

            //println!("mstore({:x}, {:x})", addr, args[1].u());

            Vec::new()
        }
        "mload" => {
            let addr = args[0].bin() as u32;
            let val = proc.get_mem(addr & 0xfffffffc);
            let rem = addr % 4;

            //println!("mload({:x}) = {:x}", addr, val);

            vec![val.into(), rem.into()]
        }
        /*
        "load_bootloader_input" => {
            let addr = args[0].bin() as usize;
            let val = self.bootloader_inputs[addr];

            vec![val]
        }
        "assert_bootloader_input" => {
            let addr = args[0].bin() as usize;
            let actual_val = self.bootloader_inputs[addr];

            assert_eq!(args[1], actual_val);

            vec![]
        }
        */
        "load_label" => args.to_vec(),
        "jump" | "jump_dyn" => {
            let next_pc = proc.get_pc().u() + 1;
            proc.set_pc(args[0]);

            vec![next_pc.into()]
        }
        /*
        "jump_to_bootloader_input" => {
            let bootloader_input_idx = args[0].bin() as usize;
            let addr = self.bootloader_inputs[bootloader_input_idx];
            proc.set_pc(addr);

            Vec::new()
        }
        */
        "branch_if_nonzero" => {
            if !args[0].is_zero() {
                proc.set_pc(args[1]);
            }

            Vec::new()
        }
        "branch_if_zero" => {
            if args[0].is_zero() {
                proc.set_pc(args[1]);
            }

            Vec::new()
        }
        "skip_if_zero" => {
            if args[0].is_zero() {
                let pc = proc.get_pc().s();
                proc.set_pc((pc + args[1].s() + 1).into());
            }

            Vec::new()
        }
        "branch_if_positive" => {
            if args[0].bin() > 0 {
                proc.set_pc(args[1]);
            }

            Vec::new()
        }
        "is_positive" => {
            let r = if args[0].bin() as i32 > 0 { 1 } else { 0 };

            vec![r.into()]
        }
        "is_equal_zero" => {
            let r = if args[0].is_zero() { 1 } else { 0 };

            vec![r.into()]
        }
        "is_not_equal_zero" => {
            let r = if !args[0].is_zero() { 1 } else { 0 };

            vec![r.into()]
        }
        "wrap" | "wrap16" => {
            // don't use .u() here: we are deliberately discarding the
            // higher bits
            let r = args[0].bin() as u32;

            vec![r.into()]
        }
        "wrap_signed" => {
            let r = (args[0].bin() + 0x100000000) as u32;

            vec![r.into()]
        }
        "sign_extend_byte" => {
            let r = args[0].u() as i8 as u32;

            vec![r.into()]
        }
        "sign_extend_16_bits" => {
            let r = args[0].u() as i16 as u32;

            vec![r.into()]
        }
        "to_signed" => {
            let r = args[0].u() as i32;

            vec![r.into()]
        }
        "fail" => {
            // TODO: handle it better
            panic!("reached a fail instruction")
        }
        "divremu" => {
            let y = args[0].u();
            let x = args[1].u();
            let div;
            let rem;
            if x != 0 {
                div = y / x;
                rem = y % x;
            } else {
                div = 0xffffffff;
                rem = y;
            }

            vec![div.into(), rem.into()]
        }
        "mul" => {
            let r = args[0].u() as u64 * args[1].u() as u64;
            let lo = r as u32;
            let hi = (r >> 32) as u32;

            vec![lo.into(), hi.into()]
        }
        "and" => vec![(args[0].u() & args[1].u()).into()],
        "or" => vec![(args[0].u() | args[1].u()).into()],
        "xor" => vec![(args[0].u() ^ args[1].u()).into()],
        "shl" => vec![(args[0].u() << args[1].u()).into()],
        "shr" => vec![(args[0].u() >> args[1].u()).into()],
        "split_gl" => {
            let value = args[0].into_fe().to_integer();
            // This instruction is only for Goldilocks, so the value must
            // fit into a u64.
            let value = value.try_into_u64().unwrap();
            let lo = (value & 0xffffffff) as u32;
            let hi = (value >> 32) as u32;

            vec![lo.into(), hi.into()]
        }
        "poseidon_gl" => {
            assert!(args.is_empty());
            let inputs = (0..12)
                .map(|i| proc.get_reg(format!("P{}", i).as_str()).into_fe())
                .collect::<Vec<_>>();
            let result = poseidon_gl::poseidon_gl(&inputs);
            (0..4).for_each(|i| proc.set_reg(format!("P{}", i).as_str(), Elem::Field(result[i])));
            vec![]
        }
        instr => {
            panic!("unknown instruction: {instr}");
        }
    }
}

pub fn to_u32<F: FieldElement>(val: &F) -> Option<u32> {
    val.to_arbitrary_integer().try_into().ok().or_else(|| {
        // Number is negative, gets it binary representation as u32.
        let modulus = F::modulus().to_arbitrary_integer();
        let diff = modulus - val.to_arbitrary_integer();
        if diff <= 0x80000000u32.into() {
            let negated: i64 = diff.try_into().unwrap();
            Some((-negated) as u32)
        } else {
            None
        }
    })
}
