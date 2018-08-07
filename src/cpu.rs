use instructions::{process_opcode, Instruction, OpInput};
use memory::MEMORY_SIZE;

use std::time;
use std::num::Wrapping;

use disasm::{disassmble};

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
		let opcode = self.get_mem(self.pc);
		let parsed_opcode = process_opcode(opcode);
		
		let cycles = CYCLE_TABLE[opcode as usize];
		self.op = opcode;

		print!("{:X} ", self.pc);
		print!("{:X} ", opcode);
		

		let constructed_opcode = match parsed_opcode {
			(instruction, amode) => {
				let extra_bytes = amode.extra_bytes();
				let slice = &self.mem
					[((self.pc + 1) as usize)..(((self.pc + 1) + extra_bytes as u16) as usize)];
				let opinput = amode.process(self, slice);
				self.pc += 1 + extra_bytes as u16;
				(instruction, opinput)
			}
		};

		print!("{}", disassmble(constructed_opcode));

		print!("\t");
		self.dump();

		match constructed_opcode {
			(Instruction::ADC, OpInput::Immediate(val)) => self.adc(val),
			(Instruction::ADC, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.adc(val);
			}
			(Instruction::AND, OpInput::Immediate(val)) => self.and(val),
			(Instruction::AND, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.and(val);
			}
			(Instruction::ASL, OpInput::Implied) => {
				let val = self.a;
				self.a = self.shl_base(false, val);
			}
			(Instruction::ASL, OpInput::Address(val)) => {
				let mut m = self.get_mem(val);
				m = self.shl_base(false, m);
				self.store_mem(val, m);
			}
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
			(Instruction::DEY, OpInput::Implied) => self.dey(),
			(Instruction::EOR, OpInput::Immediate(val)) => self.eor(val),
			(Instruction::EOR, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.eor(val);
			}
			(Instruction::INC, OpInput::Address(val)) => self.inc(val),
			(Instruction::INX, OpInput::Implied) => self.inx(),
			(Instruction::INY, OpInput::Implied) => self.iny(),
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
				self.a = self.shr_base(false, val);
			}
			(Instruction::LSR, OpInput::Address(val)) => {
				let mut m = self.get_mem(val);
				m = self.shr_base(false, m);
				self.store_mem(val, m);
			}
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
			(Instruction::ROL, OpInput::Implied) => {
				let val = self.a;
				self.a = self.rol(val);
			},
			(Instruction::ROL, OpInput::Address(addr)) => {
				let val = self.get_mem(addr);
				let ret = self.rol(val);
				self.store_mem(addr, ret)
			}
			(Instruction::ROR, OpInput::Implied) => {
				let val = self.a;
				self.a = self.ror(val);
			}
			(Instruction::ROR, OpInput::Address(addr)) => {
				let val = self.get_mem(addr);
				let ret = self.ror(val);
				self.store_mem(addr, ret)
			}
			(Instruction::RTI, OpInput::Implied) => {
				self.st = self.pop_byte();
				self.pc = self.pop_word();
			}
			(Instruction::RTS, OpInput::Implied) => self.rts(),
			(Instruction::SBC, OpInput::Immediate(val)) => self.sbc(val),
			(Instruction::SBC, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.sbc(val);
			}
			(Instruction::SEC, OpInput::Implied) => self.st |= Flags::Carry as u8,
			(Instruction::SED, OpInput::Implied) => self.st |= Flags::Decimal as u8,
			(Instruction::SEI, OpInput::Implied) => self.st |= Flags::Interrupt as u8,
			(Instruction::STA, OpInput::Address(val)) => self.mem[val as usize] = self.a as u8,
			(Instruction::STX, OpInput::Address(val)) => self.mem[val as usize] = self.x as u8,
			(Instruction::STY, OpInput::Address(val)) => self.mem[val as usize] = self.y as u8,
			(Instruction::TAX, OpInput::Implied) => {
				let val = self.a;
				self.x = self.set_zn(val);
			}
			(Instruction::TAY, OpInput::Implied) => {
				let val = self.a;
				self.y = self.set_zn(val);
			}
			(Instruction::TSX, OpInput::Implied) => {
				let val = self.sp;
				self.x = self.set_zn(val);
			}
			(Instruction::TXA, OpInput::Implied) => {
				let val = self.x;
				self.a = self.set_zn(val);
			}
			(Instruction::TXS, OpInput::Implied) => self.sp = self.x,
			(Instruction::TYA, OpInput::Implied) => {
				let val = self.y;
				self.a = self.set_zn(val);
			}

			// unsupported
			(Instruction::DCP, OpInput::Address(val)) => {
				self.dec(val);
				let ret = self.get_mem(val);
				self.cmp(ret);
			}
			(Instruction::ISC, OpInput::Address(val)) => {
				self.inc(val);
				let ret = self.get_mem(val);
				self.sbc(ret);
				// self.set_zn(ret);
			}
			(Instruction::LAX, OpInput::Address(val)) => {
				let val = self.get_mem(val);
				self.a = val;
				self.x = val;
				self.set_zn(val);
			}
			(Instruction::RLA, OpInput::Address(val)) => {
				let ret = self.get_mem(val);
				let ret = self.rol(ret);
				self.store_mem(val, ret);
				self.and(ret);
			}
			(Instruction::RRA, OpInput::Address(val)) => {
				let ret = self.get_mem(val);
				let ret = self.ror(ret);
				self.store_mem(val, ret);
				self.adc(ret);
			}
			(Instruction::SAX, OpInput::Address(val)) => {
				let ret = self.a & self.x;
				self.store_mem(val, ret); 
			}
			(Instruction::SLO, OpInput::Address(val)) => {
				let mut m = self.get_mem(val);
				m = self.shl_base(false, m);
				self.store_mem(val, m);
				self.ora(m);
			}
			(Instruction::SRE, OpInput::Address(val)) => {
				let mut m = self.get_mem(val);
				m = self.shr_base(false, m);
				self.store_mem(val, m);
				self.eor(m);
			}

			(Instruction::KIL, _) => {
				::std::process::exit(1);
			}
			(_, _) => {
				// println!(
				// 	"no mapped instruction for {:?} {:?}",
				// 	constructed_opcode.0, constructed_opcode.1
				// );
			}
		};
	}

	fn adc(&mut self, val: u8) {
		println!("adc {} + {}", self.a as u32, val as u32);
		let mut result = (self.a as u32) + (val as u32);
		if self.get_flag(Flags::Carry) { 
			result += 1;
		}

		println!("result {:X}", result);

		self.set_flag(Flags::Carry, result > 0xFF);
		
		let result = result as u8;
		let a = self.a;

		println!("result {:X}", result);

		self.set_flag(Flags::Overflow, !(((a ^ val as u8) & 0x80) != 0) && (((a ^ result as u8) & 0x80) != 0));
		// complete flag sets
		self.a = self.set_zn(result);
	}

	fn and(&mut self, val: u8) {
		let result = val & self.a;
		self.a = self.set_zn(result);
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
		self.set_flag(Flags::Zero, (val & a) == 0);
		self.set_flag(Flags::Sign, (val & 0x80) != 0);
		self.set_flag(Flags::Overflow, (val & 0x40) != 0);
	}

	fn bne(&mut self, val: i8) {
		if !self.get_flag(Flags::Zero) {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bmi(&mut self, val: i8) {
		if self.get_flag(Flags::Sign) {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bpl(&mut self, val: i8) {
		if !self.get_flag(Flags::Sign) {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bvc(&mut self, val: i8) {
		if !self.get_flag(Flags::Overflow) {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bvs(&mut self, val: i8) {
		if self.get_flag(Flags::Overflow) {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn cmp_base(&mut self, x: u8, y: u8) {
		let result = (x as i16) - (y as i16);
		self.set_flag(Flags::Carry, (result & 0x100) == 0);
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
		let result = Wrapping(self.get_mem(val)) - Wrapping(1u8);
		let result = self.set_zn(result.0);
		self.store_mem(val, result);
	}

	fn dex(&mut self) {
		let result = Wrapping(self.x) - Wrapping(1u8);
		self.x = self.set_zn(result.0);
	}

	fn dey(&mut self) {
		let result = Wrapping(self.y) - Wrapping(1u8);
		self.y = self.set_zn(result.0);
	}

	fn eor(&mut self, val: u8) {
		let result = val ^ self.a;
		self.a = self.set_zn(result);
	}

	fn inc(&mut self, val: u16) {
		let result = Wrapping(self.get_mem(val)) + Wrapping(1u8);
		let result = self.set_zn(result.0);
		self.store_mem(val, result);
	}

	// increase x wrapping
	fn inx(&mut self) {
		let result = Wrapping(self.x) + Wrapping(1u8);
		self.x = self.set_zn(result.0);
	}

	// increase y wrapping
	fn iny(&mut self) {
		let result = Wrapping(self.y) + Wrapping(1u8);
		self.y = self.set_zn(result.0);
	}

	// jump
	fn jump(&mut self, val: u16) {
		self.pc = val;
	}

	// jump to subroutine
	fn jsr(&mut self, val: u16) {
		let pc = self.pc - 1;
		self.push_word(pc);
		self.pc = val;
	}

	// load into a
	fn lda(&mut self, val: u8) {
		self.a = self.set_zn(val as u8);
	}

	// load into x
	fn ldx(&mut self, val: u8) {
		self.x = self.set_zn(val as u8);
	}

	// load into x
	fn ldy(&mut self, val: u8) {
		self.y = self.set_zn(val as u8);
	}

	fn shl_base(&mut self, lsb: bool, val: u8) -> u8 {
		let new_carry = (val & 0x80) != 0;
		let mut result = val << 1;
		if lsb {
			result |= 1;
		}
		self.set_flag(Flags::Carry, new_carry);
		self.set_zn(result)
	}

	fn shr_base(&mut self, msb: bool, val: u8) ->u8 {
		let new_carry = (val & 0x1) != 0;
		let mut result = val >> 1;
		if msb {
			result |= 0x80;
		}
		self.set_flag(Flags::Carry, new_carry);
		self.set_zn(result)
	}

	fn ora(&mut self, val: u8) {
		let result = val | self.a;
		self.a = self.set_zn(result);
	}

	fn ror(&mut self, val: u8) -> u8 {
		let c = self.get_flag(Flags::Carry);
		self.shr_base(c, val)
	}

	fn rol(&mut self, val: u8) -> u8 {
		let c = self.get_flag(Flags::Carry);
		self.shl_base(c, val)
	}

	fn rts(&mut self) {
		let addr = self.pop_word();
		self.pc = addr + 1;
	}

	fn sbc(&mut self, val: u8) {
		let a = self.a;
		let mut result = a as i32 - val as i32;
		if !self.get_flag(Flags::Carry) {
			result -= 1;
		}

		self.set_flag(Flags::Carry, (result & 0x100) == 0);

		let result = result as u8;
		let a = self.a;
		self.set_flag(Flags::Overflow, (((a ^ val as u8) & 0x80) != 0) && (((a ^ result as u8) & 0x80) != 0));
		self.a = self.set_zn(result);
	}

	fn push_byte(&mut self, val: u8) {
		let addr = self.sp as u16;
		self.store_mem(0x100 + addr, val);
		self.sp -= 1;
	}

	fn pop_byte(&mut self) -> u8 {
		self.sp += 1;
		let ret = self.get_mem(0x100 + self.sp as u16);
		ret
	}

	fn push_word(&mut self, val: u16) {
		let lobyte = val & 0xFF;
		let hibyte = (val & 0xFF00) >> 8;
		self.push_byte(hibyte as u8);
		self.push_byte(lobyte as u8);
	}

	fn pop_word(&mut self) -> u16 {
		let lobyte = self.pop_byte();
		let hibyte = self.pop_byte();
		((hibyte as u16) << 8) | (lobyte as u16)
	}

	fn get_flag(&self, flag: Flags) -> bool {
		(self.st & flag as u8) != 0
	}

	fn set_flag(&mut self, flag: Flags, on: bool) {
		if on {
			self.st |= flag as u8;
		} else {
			self.st &= !(flag as u8);
		}
	}

	fn set_zn(&mut self, val: u8) -> u8 {
		self.set_flag(Flags::Zero, val == 0);
		self.set_flag(Flags::Sign, (val & 0x80) != 0);
		val
	}

	pub fn get_mem(&self, addr: u16) -> u8 {
		let mut new_addr = addr;
		if (addr >= 0x800) && (addr <= 0x1FFF) {
			new_addr = addr % 0x800;
			println!("getting mirrored addr {:X}", new_addr);
		}
		
		self.mem[new_addr as usize]
	}

	pub fn store_mem(&mut self, addr: u16, val: u8) {
		let mut new_addr = addr;
		if (addr >= 0x800) && (addr <= 0x1FFF) {
			new_addr = addr % 0x800;
			println!("setting mirrored addr {:X}", new_addr);
		}
		self.mem[new_addr as usize] = val;
	}
}
