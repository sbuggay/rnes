use instructions::{process_opcode, Instruction, OpInput};
use memory::MEMORY_SIZE;

use std::time;

pub enum Flags {
	Carry = 0b00000001,
	Zero = 0b00000010,
	Interrupt = 0b00000100,
	Decimal = 0b00001000,
	Break = 0b00010000,
	Overflow = 0b01000000,
	Sign = 0b10000000,
}

// The number of cycles that each machine operation takes. Indexed by opcode number.
//
/// This is copied from FCEU.
static CYCLE_TABLE: [u8; 256] = [
    /*0x00*/ 7,6,2,8,3,3,5,5,3,2,2,2,4,4,6,6,
    /*0x10*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
    /*0x20*/ 6,6,2,8,3,3,5,5,4,2,2,2,4,4,6,6,
    /*0x30*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
    /*0x40*/ 6,6,2,8,3,3,5,5,3,2,2,2,3,4,6,6,
    /*0x50*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
    /*0x60*/ 6,6,2,8,3,3,5,5,4,2,2,2,5,4,6,6,
    /*0x70*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
    /*0x80*/ 2,6,2,6,3,3,3,3,2,2,2,2,4,4,4,4,
    /*0x90*/ 2,6,2,6,4,4,4,4,2,5,2,5,5,5,5,5,
    /*0xA0*/ 2,6,2,6,3,3,3,3,2,2,2,2,4,4,4,4,
    /*0xB0*/ 2,5,2,5,4,4,4,4,2,4,2,4,4,4,4,4,
    /*0xC0*/ 2,6,2,8,3,3,5,5,2,2,2,2,4,4,6,6,
    /*0xD0*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
    /*0xE0*/ 2,6,3,8,3,3,5,5,2,2,2,2,4,4,6,6,
    /*0xF0*/ 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
];

pub struct CPU {
	pub pc: u16,	// program counter
	pub a: u8,		// accumulator
	pub x: u8,		// index register x
	pub y: u8,		// index register y
	pub st: u8,		// processor status (flags)
	pub sp: u8, 	// stack pointer
	pub cycle: u16, // number of cycles
	pub op: u8,		// last run operation (for testing)
	pub mem: Vec<u8>,
}

impl CPU {
	// todo: split out registers/pc/mem
	pub fn new() -> CPU {
		CPU {
			pc: 0xC000,
			a: 0,
			x: 0,
			y: 0,
			st: 0x24,
			sp: 0xFD,
			cycle: 0,
			op: 0,
			mem: vec![0; MEMORY_SIZE], //65535
		}
	}

	pub fn dump(&self) {
		print!("A:{:X} ", &self.a);
		print!("X:{:X} ", &self.x);
		print!("Y:{:X} ", &self.y);
		print!("P:{:X} ", &self.st);
		print!("SP:{:X} ", &self.sp);
		println!("CYC:{}", &self.cycle);
	}

	pub fn step(&mut self) {
		let opcode = self.mem[self.pc as usize];
		let parsed_opcode = process_opcode(opcode);
		let cycles = CYCLE_TABLE[opcode as usize];
		self.op = opcode;

		print!("{:X}   ", self.pc);
		print!("{:X} ", opcode);

		let constructed_opcode = match parsed_opcode {
			Some((instruction, amode)) => {
				let extra_bytes = amode.extra_bytes();
				let slice = &self.mem
					[((self.pc + 1) as usize)..(((self.pc + 1) + extra_bytes as u16) as usize)];
				let opinput = amode.process(self, slice);
				self.pc += 1 + extra_bytes as u16;
				(instruction, opinput)
			}
			None => {
				println!(
					"Invalid opcode {:02X} (IP = {:04X})!",
					self.mem[self.pc as usize], self.pc
				);
				self.pc += 1;
				return;
			}
		};

		print!("\t\t");
		self.dump();

		match constructed_opcode {
			(Instruction::ADC, OpInput::Immediate(val)) => self.adc(val as i8),
			(Instruction::ADC, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.adc(val as i8);
			}
			(Instruction::AND, OpInput::Immediate(val)) => self.and(val),
			(Instruction::AND, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.and(val);
			}
			(Instruction::ASL, OpInput::Implied) => self.asl(self.st),
			(Instruction::ASL, OpInput::Address(val)) => self.asl(val as u8),
			(Instruction::BCC, OpInput::Relative(val)) => self.bcc(val),
			(Instruction::BCS, OpInput::Relative(val)) => self.bcs(val),
			(Instruction::BEQ, OpInput::Relative(val)) => self.beq(val),
			(Instruction::BIT, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.bit(val);
			}
			// Instruction::BRK => 1, // self.brk(),
			(Instruction::BMI, OpInput::Relative(val)) => self.bmi(val),
			(Instruction::BNE, OpInput::Relative(val)) => self.bne(val),
			(Instruction::BPL, OpInput::Relative(val)) => self.bpl(val),
			(Instruction::BVC, OpInput::Relative(val)) => self.bvc(val),
			(Instruction::BVS, OpInput::Relative(val)) => self.bvs(val),
			(Instruction::CLC, OpInput::Implied) => self.st &= !(Flags::Carry as u8),
			(Instruction::CLD, OpInput::Implied) => self.st &= !(Flags::Decimal as u8),
			(Instruction::CLI, OpInput::Implied) => self.st &= !(Flags::Interrupt as u8),
			(Instruction::CLV, OpInput::Implied) => self.st &= !(Flags::Overflow as u8),
			(Instruction::CMP, OpInput::Immediate(val)) => self.cmp(val),
			(Instruction::CMP, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.cmp(val);
			}
			(Instruction::CPX, OpInput::Immediate(val)) => self.cpx(val),
			(Instruction::CPX, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.cpx(val);
			}
			(Instruction::CPY, OpInput::Immediate(val)) => self.cpy(val),
			(Instruction::CPY, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.cpy(val);
			}
			(Instruction::DEC, OpInput::Address(val)) => self.dec(val),
			(Instruction::DEX, OpInput::Implied) => self.dex(),
			(Instruction::EOR, OpInput::Immediate(val)) => self.eor(val),
			(Instruction::EOR, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.eor(val);
			}
			(Instruction::INC, OpInput::Address(val)) => self.inc(val),
			(Instruction::INX, OpInput::Implied) => {
				let val = self.x + 1;
				self.inx(val);
			}
			(Instruction::INY, OpInput::Implied) => {
				let val = self.y + 1;
				self.iny(val);
			}
			(Instruction::JMP, OpInput::Address(val)) => self.jump(val),
			(Instruction::JSR, OpInput::Address(val)) => self.jsr(val),
			(Instruction::LDA, OpInput::Immediate(val)) => self.lda(val),
			(Instruction::LDA, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.lda(val);
			}
			(Instruction::LDX, OpInput::Immediate(val)) => self.ldx(val),
			(Instruction::LDX, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.ldx(val);
			}
			(Instruction::LDY, OpInput::Immediate(val)) => self.ldy(val),
			(Instruction::LDY, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.ldy(val);
			}
			(Instruction::LSR, OpInput::Implied) => {
				let val = self.a;
				self.lsr(val);
			}
			(Instruction::LSR, OpInput::Address(val)) => self.lsr(val as u8), // 16b -> 8b?
			(Instruction::ORA, OpInput::Immediate(val)) => self.ora(val),
			(Instruction::ORA, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.ora(val);
			}
			(Instruction::PHA, OpInput::Implied) => {
				let a = self.a;
				self.push_byte(a);
			}
			(Instruction::PHP, OpInput::Implied) => {
				let st = self.st;
				self.push_byte(st | Flags::Break as u8);
			}
			(Instruction::PLA, OpInput::Implied) => {
				let val = self.pop_byte();
				self.a = self.set_zn(val);
			}
			(Instruction::PLP, OpInput::Implied) => {
				let val = self.pop_byte();
				self.st = self.set_zn(val);
			}
			(Instruction::ROL, OpInput::Implied) => self.rol(self.a),
			(Instruction::ROL, OpInput::Address(val)) => self.rol(val as u8),
			(Instruction::ROR, OpInput::Implied) => {
				let val = self.a;
				self.ror(val);
			}
			(Instruction::ROR, OpInput::Address(val)) => self.ror(val as u8),
			(Instruction::RTS, OpInput::Implied) => self.rts(),
			(Instruction::SBC, OpInput::Immediate(val)) => self.sbc(val as i8),
			(Instruction::SBC, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.sbc(val as i8);
			}
			(Instruction::SEC, OpInput::Implied) => self.st |= Flags::Carry as u8,
			(Instruction::SED, OpInput::Implied) => self.st |= Flags::Decimal as u8,
			(Instruction::SEI, OpInput::Implied) => self.st |= Flags::Interrupt as u8,
			(Instruction::STA, OpInput::Address(val)) => self.mem[val as usize] = self.a as u8,
			(Instruction::STX, OpInput::Address(val)) => self.mem[val as usize] = self.x as u8,
			(Instruction::STY, OpInput::Address(val)) => self.mem[val as usize] = self.y as u8,
			(Instruction::TAX, OpInput::Implied) => self.x = self.a,
			(Instruction::TAY, OpInput::Implied) => self.y = self.a,
			(Instruction::TSX, OpInput::Implied) => self.x = self.sp,
			(Instruction::TXA, OpInput::Implied) => self.a = self.x,
			(Instruction::TXS, OpInput::Implied) => self.sp = self.x,
			(Instruction::TYA, OpInput::Implied) => self.a = self.y,
			(_, _) => {
				// println!(
				// 	"no mapped instruction for {:?} {:?}",
				// 	constructed_opcode.0, constructed_opcode.1
				// );
			}
		};

		
	}

	fn adc(&mut self, val: i8) {
		println!("adc {} + {}", self.a, val);
		let mut result = self.a as i32 + val as i32;
		if self.get_flag(Flags::Carry as u8) {
			result += 1;
		}

		println!("result {}", result);

		self.set_flag(Flags::Carry as u8, (result & 0x100) != 0);
		// complete flag sets
		self.a = result as u8;
	}

	fn and(&mut self, val: u8) {
		let result = val & self.a;
		println!("{:b} & {:b} = {:b}", val, self.a, result);
		self.a = self.set_zn(result);
	}

	fn asl(&self, val: u8) {
	}

	fn bcc(&mut self, val: i8) {
		if (self.st & Flags::Carry as u8) == 0 {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bcs(&mut self, val: i8) {
		if (self.st & Flags::Carry as u8) > 0 {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn beq(&mut self, val: i8) {
		if (self.st & Flags::Zero as u8) > 0 {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bit(&mut self, val: u8) {
		let a = self.a;
		self.set_flag(Flags::Zero as u8, (val & a) == 0);
		self.set_flag(Flags::Sign as u8, (val & 0x80) != 0);
		self.set_flag(Flags::Overflow as u8, (val & 0x40) != 0);
	}

	fn bne(&mut self, val: i8) {
		if !self.get_flag(Flags::Zero as u8) {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bmi(&mut self, val: i8) {
		if (self.st & Flags::Sign as u8) > 0 {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bpl(&mut self, val: i8) {
		if (self.st & Flags::Sign as u8) == 0 {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bvc(&mut self, val: i8) {
		if (self.st & Flags::Overflow as u8) == 0 {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bvs(&mut self, val: i8) {
		if (self.st & Flags::Overflow as u8) > 0 {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn cmp_base(&mut self, x: u8, y: u8) {
		let result = x as u32 - y as u32;
		self.set_flag(Flags::Carry as u8, (result & 0x100) == 0);
		self.set_zn(result as u8);
	}

	fn cmp(&mut self, val: u8) {
		let a = self.a;
		self.cmp_base(a, val);
	}

	fn cpx(&mut self, val: u8) {
		let x = self.x;
		self.cmp_base(x, val);
	}

	fn cpy(&mut self, val: u8) {
		let y = self.y;
		self.cmp_base(y, val);
	}

	fn dec(&mut self, val: u16) {
		let result = self.get_mem(val);
		let result = self.set_zn(result - 1);
		self.store_mem(val, result);
	}

	fn dex(&mut self) {
		let result = self.x - 1;
		self.x = self.set_zn(result);
	}

	fn eor(&mut self, val: u8) {
		let result = val ^ self.a;
		self.a = self.set_zn(result);
	}

	fn inc(&mut self, val: u16) {
		let result = self.get_mem(val);
		let result = self.set_zn(result + 1);
		self.store_mem(val, result);
	}

	fn inx(&mut self, val: u8) {
		let result = self.x + 1;
		self.x = self.set_zn(result);
	}

	fn iny(&mut self, val: u8) {
		let result = self.x + 1;
		self.x = self.set_zn(result);
	}

	fn jump(&mut self, val: u16) {
		self.pc = val;
	}

	fn jsr(&mut self, val: u16) {
		let pc = self.pc - 1;
		self.push_word(pc);
		self.pc = val;
	}

	fn lda(&mut self, val: u8) {
		self.a = self.set_zn(val as u8);
	}

	fn ldx(&mut self, val: u8) {
		self.x = self.set_zn(val as u8);
	}

	fn ldy(&mut self, val: u8) {
		self.y = self.set_zn(val as u8);
	}

	fn lsr(&mut self, val: u8) {
	}

	fn ora(&mut self, val: u8) {
	}

	fn ror(&mut self, val: u8) {
	}

	fn rol(&self, val: u8) {
	}

	fn rts(&mut self) {
		let addr = self.pop_word();
		self.pc = addr + 1;
	}

	fn sbc(&mut self, val: i8) {
		let a = self.a;
		let mut result = a as u32 - val as u32;
		if !self.get_flag(Flags::Carry as u8) {
			result -= 1;
		}

		self.set_flag(Flags::Carry as u8, (result & 0x100) == 0);

		let result = result as u8;
		let a = self.a;
		// self.set_flag(Flags::Overflow as u8, (a ^ result) & 0x80 != 0 && (a ^ val) & 0x80 == 0x80);
		self.a = self.set_zn(result);
	}

	fn push_byte(&mut self, val: u8) {
		self.sp -= 1;
		let addr = self.sp as u16;
		self.store_mem(addr, val);
	}

	fn pop_byte(&mut self) -> u8 {
		let ret = self.get_mem(self.sp as u16);
		self.sp += 1;
		ret
	}

	fn push_word(&mut self, val: u16) {
		let lobyte = val & 0xFF;
		let hibyte = (val & 0xFF00) >> 8;
		self.push_byte(hibyte as u8);
		self.push_byte(lobyte as u8)
	}

	fn pop_word(&mut self) -> u16 {
		let lobyte = self.pop_byte();
		let hibyte = self.pop_byte();
		((hibyte as u16) << 8) | (lobyte as u16)
	}

	fn get_flag(&self, flag: u8) -> bool {
		(self.st & flag) != 0
	}

	fn set_flag(&mut self, flag: u8, on: bool) {
		if on {
			self.st |= flag;
		} else {
			self.st &= !flag;
		}
	}

	fn set_zn(&mut self, val: u8) -> u8 {
		self.set_flag(Flags::Zero as u8, val == 0);
		self.set_flag(Flags::Sign as u8, (val & 0x80) != 0);
		val
	}

	pub fn get_mem(&self, addr: u16) -> u8 {
		self.mem[addr as usize]
	}

	pub fn store_mem(&mut self, addr: u16, val: u8) {
		self.mem[addr as usize] = val;
	}
}
