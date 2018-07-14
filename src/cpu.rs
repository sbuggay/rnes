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
		
		
		let constructed_opcode = match parsed_opcode {
			Some((instruction, amode)) => {
				let extra_bytes = amode.extra_bytes();
				let slice = &self.mem[(self.pc as usize)..((self.pc + extra_bytes as u16) as usize)];
				let opinput = amode.process(self, slice);
				self.pc += extra_bytes as u16;
				(instruction, opinput)
			},
			None => {
				println!(
					"Invalid opcode {:02X} (IP = {:04X})!",
					self.mem[self.pc as usize], self.pc
				);
				return;
			}
		};

		match constructed_opcode {
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
			(Instruction::CLC, OpInput::Implied) => self.st |= Flags::Carry as u8,
			(Instruction::CLD, OpInput::Implied) => self.st |= Flags::Decimal as u8,
			(Instruction::CLI, OpInput::Implied) => self.st |= Flags::Interrupt as u8,
			(Instruction::CLV, OpInput::Implied) => self.st |= Flags::Overflow as u8,
			(Instruction::CMP, OpInput::Immediate(val)) => self.cmp(val),
			(Instruction::CMP, OpInput::Address(val)) => self.cmp(self.mem[val as usize]),
			(Instruction::CPX, OpInput::Immediate(val)) => self.cpx(val),
			(Instruction::CPX, OpInput::Address(val)) => self.cpx(self.mem[val as usize]),
			(Instruction::CPY, OpInput::Immediate(val)) => self.cpy(val),
			(Instruction::CPY, OpInput::Address(val)) => self.cpy(self.mem[val as usize]),
			(Instruction::DEC, OpInput::Address(val)) => self.dec(val),
			(Instruction::DEX, OpInput::Implied) => self.dex(),
			(Instruction::EOR, OpInput::Immediate(val)) => self.eor(val),
			(Instruction::EOR, OpInput::Address(val)) => self.eor(self.mem[val as usize]),
			(Instruction::INC, OpInput::Address(val)) => self.inc(val),
			(Instruction::INX, OpInput::Implied) => self.inx(self.x + 1),
			(Instruction::INY, OpInput::Implied) => self.iny(self.y + 1),
			(Instruction::JMP, OpInput::Address(val)) => self.jump(val),
			(Instruction::LDA, OpInput::Immediate(val)) => self.lda(val as i8),
			(Instruction::LDA, OpInput::Address(val)) => self.lda(self.mem[val as usize] as i8),
			(Instruction::LDX, OpInput::Immediate(val)) => self.ldx(val as i8),
			(Instruction::LDX, OpInput::Address(val)) => self.ldx(self.mem[val as usize] as i8),
			(Instruction::LDY, OpInput::Immediate(val)) => self.ldy(val as i8),
			(Instruction::LDY, OpInput::Address(val)) => self.ldy(self.mem[val as usize] as i8),
			(Instruction::LSR, OpInput::Implied) => self.lsr(self.st),
			(Instruction::LSR, OpInput::Address(val)) => self.lsr(val as u8), // 16b -> 8b?
			(Instruction::ORA, OpInput::Immediate(val)) => self.ora(val),
			(Instruction::ORA, OpInput::Address(val)) => self.ora(self.mem[val as usize]),

			// (Instruction::PHA, OpInput::UseImplied) => {
			// 	// Push accumulator
			// 	let val = self.registers.accumulator as u8;
			// 	self.push_on_stack(val);
			// }
			// (Instruction::PHP, OpInput::UseImplied) => {
			// 	// Push status
			// 	let val = self.registers.status.bits();
			// 	self.push_on_stack(val);
			// }
			// (Instruction::PLA, OpInput::UseImplied) => {
			// 	// Pull accumulator
			// 	let val: u8 = self.pull_from_stack();
			// 	self.registers.accumulator = val as i8;
			// }
			// (Instruction::PLP, OpInput::UseImplied) => {
			// 	// Pull status
			// 	let val: u8 = self.pull_from_stack();
			// 	self.registers.status = Status::from_bits_truncate(val);
			// }

			(Instruction::ROL, OpInput::Implied) => self.rol(self.a),
			(Instruction::ROL, OpInput::Address(val)) => self.rol(val as u8),
			(Instruction::ROR, OpInput::Implied) => self.ror(self.a),
			(Instruction::ROR, OpInput::Address(val)) => self.ror(val as u8),


			// (Instruction::SBC, OpInput::UseImmediate(val)) => {
			// 	debug!("subtract with carry immediate: {}", val);
			// 	self.subtract_with_carry(val as i8);
			// }
			// (Instruction::SBC, OpInput::UseAddress(addr)) => {
			// 	let val = self.memory.get_byte(addr) as i8;
			// 	debug!("subtract with carry. address: {:?}. value: {}", addr, val);
			// 	self.subtract_with_carry(val);
			// }

			// (Instruction::SEC, OpInput::UseImplied) => {
			// 	self.registers.status.or(PS_CARRY);
			// }
			// (Instruction::SED, OpInput::UseImplied) => {
			// 	self.registers.status.or(PS_DECIMAL_MODE);
			// }
			// (Instruction::SEI, OpInput::UseImplied) => {
			// 	self.registers.status.or(PS_DISABLE_INTERRUPTS);
			// }

			// (Instruction::STA, OpInput::UseAddress(addr)) => {
			// 	self.memory.set_byte(addr, self.registers.accumulator as u8);
			// }
			// (Instruction::STX, OpInput::UseAddress(addr)) => {
			// 	self.memory.set_byte(addr, self.registers.index_x as u8);
			// }
			// (Instruction::STY, OpInput::UseAddress(addr)) => {
			// 	self.memory.set_byte(addr, self.registers.index_y as u8);
			// }

			// (Instruction::TAX, OpInput::UseImplied) => {
			// 	let val = self.registers.accumulator;
			// 	self.load_x_register(val);
			// }
			// (Instruction::TAY, OpInput::UseImplied) => {
			// 	let val = self.registers.accumulator;
			// 	self.load_y_register(val);
			// }
			// (Instruction::TSX, OpInput::UseImplied) => {
			// 	let StackPointer(val) = self.registers.stack_pointer;
			// 	let val = val as i8;
			// 	self.load_x_register(val);
			// }
			// (Instruction::TXA, OpInput::UseImplied) => {
			// 	let val = self.registers.index_x;
			// 	self.load_accumulator(val);
			// }
			// (Instruction::TXS, OpInput::UseImplied) => {
			// 	// Note that this is the only 'transfer' instruction that does
			// 	// NOT set the zero and negative flags. (Because the target
			// 	// is the stack pointer)
			// 	let val = self.registers.index_x;
			// 	self.registers.stack_pointer = StackPointer(val as u8);
			// }
			// (Instruction::TYA, OpInput::UseImplied) => {
			// 	let val = self.registers.index_y;
			// 	self.load_accumulator(val);
			// }
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

	fn jump(&self, val: u16) {
		println!("JUMP {}", val);
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
}
