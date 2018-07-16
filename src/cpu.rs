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

pub struct CPU {
	pub pc: u16, // program counter
	pub a: u8,   // accumulator
	pub x: u8,   // index register x
	pub y: u8,   // index register y
	pub st: u8,  // processor status (flags)
	pub sp: u8,
	pub mem: Vec<u8>,
}

impl CPU {
	pub fn new() -> CPU {
		CPU {
			pc: 0,
			a: 0,
			x: 0,
			y: 0,
			st: 0x20,
			sp: 0xFF,
			mem: vec![0; MEMORY_SIZE], //65535
		}
	}

	pub fn dump(&self) {
		println!("program counter:\t{}", &self.pc);
		println!("accumulator:\t\t{}", &self.a);
		println!("x:\t\t\t{}", &self.x);
		println!("y:\t\t\t{}", &self.y);
		println!("status:\t\t\t{}", &self.st);
		println!{"mem capacity:\t\t{}", &self.mem.capacity()};
	}

	pub fn emulate(&mut self) {
		let opcode = self.mem[self.pc as usize];
		let parsed_opcode = process_opcode(opcode);

		println!("# {:X}", opcode);

		let constructed_opcode = match parsed_opcode {
			Some((instruction, amode)) => {
				let extra_bytes = amode.extra_bytes();
				let slice = &self.mem
					[((self.pc + 1) as usize)..(((self.pc + 1) + extra_bytes as u16) as usize)];
				let opinput = amode.process(self, slice);
				// println!("{} + {}", self.pc, extra_bytes);
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

		match constructed_opcode {
			(Instruction::ADC, OpInput::Immediate(val)) => self.adc(val as i8),
			(Instruction::ADC, OpInput::Address(val)) 	=> {
				let x = self.mem[val as usize] as i8;
				self.adc(x);
			}
			(Instruction::AND, OpInput::Immediate(val)) => self.and(val as i8),
			(Instruction::AND, OpInput::Address(val)) 	=> self.and(self.mem[val as usize] as i8),
			(Instruction::ASL, OpInput::Implied) 		=> self.asl(self.st),
			(Instruction::ASL, OpInput::Address(val)) 	=> self.asl(val as u8),
			(Instruction::BCC, OpInput::Relative(val)) 	=> self.bcc(val),
			(Instruction::BCS, OpInput::Relative(val)) 	=> self.bcs(val),
			(Instruction::BEQ, OpInput::Relative(val)) 	=> self.beq(val),
			(Instruction::BIT, OpInput::Address(val)) 	=> self.bit(self.mem[val as usize]),
			// Instruction::BRK => 1, // self.brk(),
			(Instruction::BMI, OpInput::Relative(val)) 	=> self.bmi(self.pc + val as u16),
			(Instruction::BPL, OpInput::Relative(val)) 	=> self.bmi(self.pc + val as u16),
			(Instruction::BVC, OpInput::Relative(val)) 	=> self.bmi(self.pc + val as u16),
			(Instruction::BVS, OpInput::Relative(val)) 	=> self.bmi(self.pc + val as u16),
			(Instruction::CLC, OpInput::Implied)		=> self.st &= !(Flags::Carry as u8),
			(Instruction::CLD, OpInput::Implied) 		=> self.st &= !(Flags::Decimal as u8),
			(Instruction::CLI, OpInput::Implied) 		=> self.st &= !(Flags::Interrupt as u8),
			(Instruction::CLV, OpInput::Implied) 		=> self.st &= !(Flags::Overflow as u8),
			(Instruction::CMP, OpInput::Immediate(val)) => self.cmp(val),
			(Instruction::CMP, OpInput::Address(val)) 	=> self.cmp(self.mem[val as usize]),
			(Instruction::CPX, OpInput::Immediate(val)) => self.cpx(val),
			(Instruction::CPX, OpInput::Address(val)) 	=> self.cpx(self.mem[val as usize]),
			(Instruction::CPY, OpInput::Immediate(val)) => self.cpy(val),
			(Instruction::CPY, OpInput::Address(val)) 	=> self.cpy(self.mem[val as usize]),
			(Instruction::DEC, OpInput::Address(val)) 	=> self.dec(val),
			(Instruction::DEX, OpInput::Implied) 		=> self.dex(),
			(Instruction::EOR, OpInput::Immediate(val)) => self.eor(val),
			(Instruction::EOR, OpInput::Address(val)) 	=> self.eor(self.mem[val as usize]),
			(Instruction::INC, OpInput::Address(val)) 	=> self.inc(val),
			(Instruction::INX, OpInput::Implied) 		=> self.inx(self.x + 1),
			(Instruction::INY, OpInput::Implied) 		=> self.iny(self.y + 1),
			(Instruction::JMP, OpInput::Address(val)) 	=> self.jump(val),
			(Instruction::JSR, OpInput::Address(val)) 	=> self.jsr(val),
			(Instruction::LDA, OpInput::Immediate(val)) => self.lda(val as i8),
			(Instruction::LDA, OpInput::Address(val)) 	=> self.lda(self.mem[val as usize] as i8),
			(Instruction::LDX, OpInput::Immediate(val)) => self.ldx(val as i8),
			(Instruction::LDX, OpInput::Address(val)) 	=> self.ldx(self.mem[val as usize] as i8),
			(Instruction::LDY, OpInput::Immediate(val)) => self.ldy(val as i8),
			(Instruction::LDY, OpInput::Address(val)) 	=> self.ldy(self.mem[val as usize] as i8),
			(Instruction::LSR, OpInput::Implied) 		=> self.lsr(self.st),
			(Instruction::LSR, OpInput::Address(val)) 	=> self.lsr(val as u8), // 16b -> 8b?
			(Instruction::ORA, OpInput::Immediate(val)) => self.ora(val),
			(Instruction::ORA, OpInput::Address(val)) 	=> self.ora(self.mem[val as usize]),
			(Instruction::PHA, OpInput::Implied) => {
				let a = self.a;
				self.push_byte(a);
			}
			(Instruction::PHP, OpInput::Implied) => {
				let st = self.st;
				self.push_byte(st);
			}
			(Instruction::PLA, OpInput::Implied) => {
				let val = self.pop_byte();
				self.a = val;
			}
			(Instruction::PLP, OpInput::Implied) => {
				let val = self.pop_byte();
				self.st = val;
			}
			(Instruction::ROL, OpInput::Implied)=> 		self.rol(self.a),
			(Instruction::ROL, OpInput::Address(val)) 	=> self.rol(val as u8),
			(Instruction::ROR, OpInput::Implied) 		=> self.ror(self.a),
			(Instruction::ROR, OpInput::Address(val)) 	=> self.ror(val as u8),
			(Instruction::SBC, OpInput::Immediate(val)) => self.sbc(val as i8),
			(Instruction::SBC, OpInput::Address(val)) 	=> self.sbc(self.mem[val as usize] as i8),
			(Instruction::SEC, OpInput::Implied) 		=> self.st |= Flags::Carry as u8,
			(Instruction::SED, OpInput::Implied) 		=> self.st |= Flags::Decimal as u8,
			(Instruction::SEI, OpInput::Implied) 		=> self.st |= Flags::Interrupt as u8,
			(Instruction::STA, OpInput::Address(val)) 	=> self.mem[val as usize] = self.a as u8,
			(Instruction::STX, OpInput::Address(val)) 	=> self.mem[val as usize] = self.x as u8,
			(Instruction::STY, OpInput::Address(val)) 	=> self.mem[val as usize] = self.y as u8,
			(Instruction::TAX, OpInput::Implied) 		=> self.x = self.a,
			(Instruction::TAY, OpInput::Implied) 		=> self.y = self.a,
			(Instruction::TSX, OpInput::Implied) 		=> self.x = self.sp,
			(Instruction::TXA, OpInput::Implied) 		=> self.a = self.x,
			(Instruction::TXS, OpInput::Implied) 		=> self.sp = self.x,
			(Instruction::TYA, OpInput::Implied) 		=> self.a = self.y,
			(_, _) => {
				println!(
					"no mapped instruction for {:?} {:?}",
					constructed_opcode.0, constructed_opcode.1
				);
			}
		};
	}

	fn adc(&mut self, val: i8) {
		println!("ADC {}", val);
		let mut result = self.a as u32 + val as u32;
		if self.get_flag(Flags::Carry as u8) {
			result += 1;
		}

		self.set_flag(Flags::Carry as u8, (result & 0x100) != 0);


		// complete flag sets
		self.a = result as u8;
	}

	fn and(&self, val: i8) {
		println!("AND {}", val);
	}

	fn asl(&self, val: u8) {
		println!("ASL {}", val);
	}

	fn bcc(&mut self, val: i8) {
		println!("BCC {}", val);
		if (self.st & Flags::Carry as u8) == 0 {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bcs(&mut self, val: i8) {
		println!("BCC {}", val);
		if (self.st & Flags::Carry as u8) > 0 {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn beq(&mut self, val: i8) {
		println!("BEQ {}", val);
		if (self.st & Flags::Zero as u8) > 0 {
			self.pc = (self.pc as i32 + val as i32) as u16;
		}
	}

	fn bit(&self, val: u8) {
		println!("BIT");
	}

	fn bmi(&self, val: u16) {
		println!("BMI {}", val);
	}

	fn cmp(&self, val: u8) {
		println!("CMP {}", val);
	}

	fn cpx(&self, val: u8) {}

	fn cpy(&self, val: u8) {}

	fn dec(&self, val: u16) {
		println!("Dec {}", val);
	}

	fn dex(&self) {
		println!("DEX");
	}

	fn eor(&self, val: u8) {
		println!("EOR {}", val);
	}

	fn inc(&self, val: u16) {
		println!("INC {}", val);
	}

	fn inx(&self, val: u8) {
		println!("INX {}", val);
	}

	fn iny(&self, val: u8) {
		println!("INY {}", val);
	}

	fn jump(&mut self, val: u16) {
		println!("JUMP {}", val);
		self.pc = val;
	}

	fn jsr(&mut self, val: u16) {
		println!("JSR {}", val);
		let pc = self.pc - 1;
		self.push_word(pc);
		self.pc = val;
	}

	fn lda(&self, val: i8) {
		println!("LDA {}", val);
	}

	fn ldx(&self, val: i8) {
		println!("LDX {}", val);
	}

	fn ldy(&self, val: i8) {
		println!("LDY {}", val);
	}

	fn lsr(&self, val: u8) {
		println!("LSR {}", val);
	}

	fn ora(&self, val: u8) {
		println!("ORA {}", val);
	}

	fn ror(&self, val: u8) {
		println!("ORA {}", val);
	}

	fn rol(&self, val: u8) {
		println!("ORA {}", val);
	}

	fn sbc(&self, val: i8) {
		println!("SBC {}", val);
	}

	fn push_byte(&mut self, val: u8) {
		self.sp -= 1;
		self.mem[self.sp as usize] = val;
	}

	fn pop_byte(&mut self) -> u8 {
		let ret = self.mem[self.sp as usize];
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
}
