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
		let now = time::Instant::now();

		let opcode = match process_opcode(self.mem[self.pc as usize]) {
			Some(op) => op,
			None => {
				println!(
					"Invalid opcode {:02X} (IP = {:04X})!",
					self.mem[self.pc as usize], self.pc
				);
				return;
			}
		};

		match opcode {
			(Instruction::ADC, OpInput::Immediate(val)) => self.adc(val as i8),
			(Instruction::ADC, OpInput::Address(val)) => self.adc(self.mem[val as usize] as i8),
			(Instruction::AND, OpInput::Immediate(val)) => self.and(val as i8),
			(Instruction::AND, OpInput::Address(val)) => self.and(self.mem[val as usize] as i8),
			(Instruction::ASL, OpInput::Implied) => self.asl(self.st),
			(Instruction::ASL, OpInput::Address(val)) => self.asl(val as u8), // 16b -> 8b?
			(Instruction::BCC, OpInput::Relative(val)) => self.bcc((self.pc + val as u16) as i32), // 32b?
			(Instruction::BCS, OpInput::Relative(val)) => self.bcs((self.pc + val as u16) as i32), // 32b?
			(Instruction::BEQ, OpInput::Relative(val)) => self.beq((self.pc + val as u16) as i32), // 32b?
			(Instruction::BIT, OpInput::Address(val)) => self.bit(),
			// Instruction::BRK => 1, // self.brk(),
			(Instruction::BMI, OpInput::Relative(val)) => self.bmi((self.pc + val as u16) as i32), // 32b?
			(Instruction::BPL, OpInput::Relative(val)) => self.bmi((self.pc + val as u16) as i32), // 32b?
			(Instruction::BVC, OpInput::Relative(val)) => self.bmi((self.pc + val as u16) as i32), // 32b?
			(Instruction::BVS, OpInput::Relative(val)) => self.bmi((self.pc + val as u16) as i32), // 32b?

			// Instruction::CLC => 1, // self.clc(),
			// Instruction::CLD => 1, // self.cld(),
			// Instruction::CLI => 1, // self.cli(),
			// Instruction::CLV => 1, // self.clv(),
			// Instruction::CMP => 1, // self.cmp(val),
			// Instruction::CPX => 1, // self.cpx(val),
			// Instruction::CPY => 1, // self.cpy(val),
			// Instruction::DEC => 1, // self.dec(addr),
			// Instruction::DEX => 1, // self.dex(),
			// Instruction::DEY => 1, // self.dey(),
			// Instruction::EOR => 1, // self.eor(val),
			// Instruction::INC => 1, // self.inc(addr),
			// Instruction::INX => 1, // self.inx(),
			// Instruction::INY => 1, // self.iny(),
			// Instruction::JMP => 1, // self.jmp(addr),
			// Instruction::JSR => 1, // self.jsr(addr),
			// Instruction::LDA => 1, // self.lda(val),
			// Instruction::LDX => 1, // self.ldx(val),
			// Instruction::LDY => 1, // self.ldy(val),
			// Instruction::LSR => 1, // self.lsr(opr),
			// Instruction::NOP => 1, // self.nop(),
			// Instruction::ORA => 1, // self.ora(val),
			// Instruction::PHA => 1, // self.pha(),
			// Instruction::PHP => 1, // self.php(),
			// Instruction::PLA => 1, // self.pla(),
			// Instruction::PLP => 1, // self.plp(),
			// Instruction::ROL => 1, // self.rol(opr),
			// Instruction::ROR => 1, // self.ror(opr),
			// Instruction::RTI => 1, // self.rti(),
			// Instruction::RTS => 1, // self.rts(),
			// Instruction::SBC => 1, // self.sbc(val),
			// Instruction::SEC => 1, // self.sec(),
			// Instruction::SED => 1, // self.sed(),
			// Instruction::SEI => 1, // self.sei(),
			// Instruction::STA => 1, // self.sta(addr),
			// Instruction::STX => 1, // self.stx(addr),
			// Instruction::STY => 1, // self.sty(addr),
			// Instruction::TAX => 1, // self.tax(),
			// Instruction::TAY => 1, // self.tay(),
			// Instruction::TSX => 1, // self.tsx(),
			// Instruction::TXA => 1, // self.txa(),
			// Instruction::TYA => 1, // self.tya(),
			// Instruction::TXS => 1, // self.txs(),
			(_, _) => {
				println!("invalid instruction");
			}
		};

		println!("matching and invocation of {:?} took {}", opcode.0, now.elapsed().subsec_micros());

		self.pc += 1;
	}

	fn adc(&self, val: i8) {
		println!("ADC {}", val);
	}

	fn and(&self, val: i8) {
		println!("AND {}", val);
	}

	fn asl(&self, val: u8) {
		println!("ASL {}", val);
	}

	fn bcc(&self, val: i32) {
		println!("BCC {}", val);
	}

	fn bcs(&self, val: i32) {
		println!("BCS {}", val);
	}

	fn beq(&self, val: i32) {
		println!("BEQ {}", val);
	}

	fn bit(&self) {
		println!("BIT");
	}

	fn bmi(&self, val: i32) {
		println!("BMI {}", val);
	}
}
