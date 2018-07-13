use instructions::Instruction;
use memory::MEMORY_SIZE;

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
	pub a: u8, // accumulator
	pub x: u8, // index register x
	pub y: u8, // index register y
	pub st: u8, // processor status (flags)
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
			mem: vec![0; MEMORY_SIZE] //65535
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

	pub fn single_step(&self) {
		// let opcode = match get_opcode(self.mem[self.pc as usize]) {
		// 	Some(op) => op,
		// 	None => {
		// 		return Err(format!("Invalid opcode {:02X} (IP = {:04X})!",
		// 						   self.mem[self.pc as usize],
		// 						   self.pc))
		// 	}
		// };

		// let opr: Operand = self.fetch_operand(opcode);
		// let val: u8 = self.operand_source(&opr);
		// let addr: u16 = self.operand_target(&opr);

		let opcode: Instruction = Instruction::ADC;

		let output = match opcode {
			Instruction::ADC => 1,// self.adc(val),
			Instruction::AND => 1,// self.and(val),
			Instruction::ASL => 1,// self.asl(opr),
			Instruction::BCC => 1,// self.bcc(val),
			Instruction::BCS => 1,// self.bcs(val),
			Instruction::BEQ => 1,// self.beq(val),
			Instruction::BIT => 1,// self.bit(val),
			Instruction::BRK => 1,// self.brk(),
			Instruction::BMI => 1,// self.bmi(val),
			Instruction::BNE => 1,// self.bne(val),
			Instruction::BPL => 1,// self.bpl(val),
			Instruction::BVC => 1,// self.bvc(val),
			Instruction::BVS => 1,// self.bvs(val),
			Instruction::CLC => 1,// self.clc(),
			Instruction::CLD => 1,// self.cld(),
			Instruction::CLI => 1,// self.cli(),
			Instruction::CLV => 1,// self.clv(),
			Instruction::CMP => 1,// self.cmp(val),
			Instruction::CPX => 1,// self.cpx(val),
			Instruction::CPY => 1,// self.cpy(val),
			Instruction::DEC => 1,// self.dec(addr),
			Instruction::DEX => 1,// self.dex(),
			Instruction::DEY => 1,// self.dey(),
			Instruction::EOR => 1,// self.eor(val),
			Instruction::INC => 1,// self.inc(addr),
			Instruction::INX => 1,// self.inx(),
			Instruction::INY => 1,// self.iny(),
			Instruction::JMP => 1,// self.jmp(addr),
			Instruction::JSR => 1,// self.jsr(addr),
			Instruction::LDA => 1,// self.lda(val),
			Instruction::LDX => 1,// self.ldx(val),
			Instruction::LDY => 1,// self.ldy(val),
			Instruction::LSR => 1,// self.lsr(opr),
			Instruction::NOP => 1,// self.nop(),
			Instruction::ORA => 1,// self.ora(val),
			Instruction::PHA => 1,// self.pha(),
			Instruction::PHP => 1,// self.php(),
			Instruction::PLA => 1,// self.pla(),
			Instruction::PLP => 1,// self.plp(),
			Instruction::ROL => 1,// self.rol(opr),
			Instruction::ROR => 1,// self.ror(opr),
			Instruction::RTI => 1,// self.rti(),
			Instruction::RTS => 1,// self.rts(),
			Instruction::SBC => 1,// self.sbc(val),
			Instruction::SEC => 1,// self.sec(),
			Instruction::SED => 1,// self.sed(),
			Instruction::SEI => 1,// self.sei(),
			Instruction::STA => 1,// self.sta(addr),
			Instruction::STX => 1,// self.stx(addr),
			Instruction::STY => 1,// self.sty(addr),
			Instruction::TAX => 1,// self.tax(),
			Instruction::TAY => 1,// self.tay(),
			Instruction::TSX => 1,// self.tsx(),
			Instruction::TXA => 1,// self.txa(),
			Instruction::TYA => 1,// self.tya(),
			Instruction::TXS => 1,// self.txs(),
		};

		println!("running opcode {:?}", opcode);
	}
}