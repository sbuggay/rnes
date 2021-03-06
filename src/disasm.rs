use instructions::{Instruction, OpInput};

pub fn disassmble(constructed_opcode: (Instruction, OpInput)) -> String {
	match constructed_opcode {
		(Instruction::ADC, OpInput::Immediate(val)) => format!("ADC {:X}", val),
		(Instruction::ADC, OpInput::Address(val)) => format!("ADC ${:X}", val),
		(Instruction::AND, OpInput::Immediate(val)) => format!("AND {:X}", val),
		(Instruction::AND, OpInput::Address(val)) => format!("AND ${:X}", val),
		(Instruction::ASL, OpInput::Implied) => format!("ASL"),
		(Instruction::ASL, OpInput::Address(val)) => format!("ASL {:X}", val),
		(Instruction::BCC, OpInput::Relative(val)) => format!("BCC {:X}", val),
		(Instruction::BCS, OpInput::Relative(val)) => format!("BCS {:X}", val),
		(Instruction::BEQ, OpInput::Relative(val)) => format!("BEQ {:X}", val),
		(Instruction::BIT, OpInput::Address(val)) => format!("BIT ${:X}", val),
		// Instruction::BRK => 1, // self.brk(),
		(Instruction::BMI, OpInput::Relative(val)) => format!("BMI {:X}", val),
		(Instruction::BNE, OpInput::Relative(val)) => format!("BNE {:X}", val),
		(Instruction::BPL, OpInput::Relative(val)) => format!("BPL {:X}", val),
		(Instruction::BVC, OpInput::Relative(val)) => format!("BVC {:X}", val),
		(Instruction::BVS, OpInput::Relative(val)) => format!("BVS {:X}", val),
		(Instruction::CLC, OpInput::Implied) => format!("CLC"),
		(Instruction::CLD, OpInput::Implied) => format!("CLD"),
		(Instruction::CLI, OpInput::Implied) => format!("CLI"),
		(Instruction::CLV, OpInput::Implied) => format!("CLV"),
		(Instruction::CMP, OpInput::Immediate(val)) => format!("CMP {:X}", val),
		(Instruction::CMP, OpInput::Address(val)) => format!("CMP ${:X}", val),
		(Instruction::CPX, OpInput::Immediate(val)) => format!("CPX {:X}", val),
		(Instruction::CPX, OpInput::Address(val)) => format!("CPX ${:X}", val),
		(Instruction::CPY, OpInput::Immediate(val)) => format!("CPY {:X}", val),
		(Instruction::CPY, OpInput::Address(val)) => format!("CPY ${:X}", val),
		(Instruction::DEC, OpInput::Address(val)) => format!("DEC ${:X}", val),
		(Instruction::DEX, OpInput::Implied) => format!("DEX"),
		(Instruction::DEY, OpInput::Implied) => format!("DEY"),
		(Instruction::EOR, OpInput::Immediate(val)) => format!("EOR {:X}", val),
		(Instruction::EOR, OpInput::Address(val)) => format!("EOR ${:X}", val),
		(Instruction::INC, OpInput::Address(val)) => format!("INC ${:X}", val),
		(Instruction::INX, OpInput::Implied) => format!("INX"),
		(Instruction::INY, OpInput::Implied) => format!("INY"),
		(Instruction::JMP, OpInput::Address(val)) => format!("JMP ${:X}", val),
		(Instruction::JSR, OpInput::Address(val)) =>format!("JSR ${:X}", val),
		(Instruction::LDA, OpInput::Immediate(val)) => format!("LDA {:X}", val),
		(Instruction::LDA, OpInput::Address(val)) => format!("LDA ${:X}", val),
		(Instruction::LDX, OpInput::Immediate(val)) => format!("LDX {:X}", val),
		(Instruction::LDX, OpInput::Address(val)) => format!("LDX ${:X}", val),
		(Instruction::LDY, OpInput::Immediate(val)) => format!("LDY {:X}", val),
		(Instruction::LDY, OpInput::Address(val)) => format!("LDY ${:X}", val),
		(Instruction::LSR, OpInput::Implied) => format!("LSR"),
		(Instruction::LSR, OpInput::Address(val)) => format!("LSR ${:X}", val),
		(Instruction::ORA, OpInput::Immediate(val)) => format!("ORA {:X}", val),
		(Instruction::ORA, OpInput::Address(val)) => format!("ORA ${:X}", val),
		(Instruction::PHA, OpInput::Implied) => format!("PHA"),
		(Instruction::PHP, OpInput::Implied) => format!("PHP"),
		(Instruction::PLA, OpInput::Implied) => format!("PLA"),
		(Instruction::PLP, OpInput::Implied) => format!("PLP"),
		(Instruction::ROL, OpInput::Implied) => format!("ROL"),
		(Instruction::ROL, OpInput::Address(val)) => format!("ROL ${:X}", val),
		(Instruction::ROR, OpInput::Implied) => format!("ROR"),
		(Instruction::ROR, OpInput::Address(val)) => format!("ROR ${:X}", val),
		(Instruction::RTI, OpInput::Implied) => format!("RTI"),
		(Instruction::RTS, OpInput::Implied) => format!("RTS"),
		(Instruction::SBC, OpInput::Immediate(val)) => format!("SBC {:X}", val),
		(Instruction::SBC, OpInput::Address(val)) => format!("SBC ${:X}", val),
		(Instruction::SEC, OpInput::Implied) => format!("SEC"),
		(Instruction::SED, OpInput::Implied) => format!("SED"),
		(Instruction::SEI, OpInput::Implied) => format!("SEI"),
		(Instruction::STA, OpInput::Address(val)) => format!("STA ${:X}", val),
		(Instruction::STX, OpInput::Address(val)) => format!("STX ${:X}", val),
		(Instruction::STY, OpInput::Address(val)) => format!("STY ${:X}", val),
		(Instruction::TAX, OpInput::Implied) => format!("TAX"),
		(Instruction::TAY, OpInput::Implied) => format!("TAY"),
		(Instruction::TSX, OpInput::Implied) => format!("TSX"),
		(Instruction::TXA, OpInput::Implied) => format!("TXA"),
		(Instruction::TXS, OpInput::Implied) => format!("TXS"),
		(Instruction::TYA, OpInput::Implied) => format!("TYA"),

		(Instruction::LAX, OpInput::Address(val)) => format!("*LAX ${}", val),
		(_, _) => format!("?{:?}", constructed_opcode.0)
	}.to_string()
}