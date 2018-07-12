mod cpu;

use cpu::CPU;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
	ADC, // ADd with Carry................ | NV ...ZC A            = A + M + C
	AND, // logical AND (bitwise)......... | N. ...Z. A            = A && M
	ASL, // Arithmetic Shift Left......... | N. ...ZC A            = M << 1    BCC // Branch if Carry Clear......... | .. .....         PC   = !C
	BCC, // Branch if Carry Clear......... | .. .....         PC   = !C
	BCS, // Branch if Carry Set........... | .. .....         PC   = C
	BEQ, // Branch if Equal (to zero?).... | .. .....         PC   = Z
	BIT, // BIT test...................... | NV ...Z.              = A & M
	BMI, // Branch if Minus............... | .. .....         PC   = N
	BNE, // Branch if Not Equal........... | .. .....         PC   = !Z
	BPL, // Branch if Positive............ | .. .....         PC   = Z
	BRK, // BReaK......................... | .. B....       S PC   =
	BVC, // Branch if oVerflow Clear...... | .. .....         PC   = !V
	BVS, // Branch if oVerflow Set........ | .. .....         PC   = V
	CLC, // CLear Carry flag.............. | .. ....C              = 0
	CLD, // Clear Decimal Mode............ | .. .D...              = 0
	CLI, // Clear Interrupt Disable....... | .. ..I..              = 0
	CLV, // Clear oVerflow flag........... | .V .....              = 0
	CMP, // Compare....................... | N. ...ZC              = A - M
	CPX, // Compare X register............ | N. ...ZC              = X - M
	CPY, // Compare Y register............ | N. ...ZC              = Y - M
	DEC, // DECrement memory.............. | N. ...Z.            M = M - 1
	DEX, // DEcrement X register.......... | N. ...Z.   X          = X - 1
	DEY, // DEcrement Y register.......... | N. ...Z.     Y        = Y - 1
	EOR, // Exclusive OR (bitwise)........ | N. ...Z. A            = A ^ M
	INC, // INCrement memory.............. | N. ...Z.            M = M + 1
	INX, // INcrement X register.......... | N. ...Z.   X          = X + 1
	INY, // INcrement Y register.......... | N. ...Z.     Y        = Y + 1
	JMP, // JuMP.......................... | .. .....       S PC   =
	JSR, // Jump to SubRoutine............ | .. .....       S PC   =
	LDA, // LoaD Accumulator.............. | N. ...Z. A            = M
	LDX, // LoaD X register............... | N. ...Z.   X          = M
	LDY, // LoaD Y register............... | N. ...Z.     Y        = M
	LSR, // Logical Shift Right........... | N. ...ZC A            = A/2     //                               or N. ...ZC            M = M/2
	NOP, // No OPeration.................. | .. .....              =
	ORA, // inclusive OR (bitwise)........ | N. ...Z. A            = A | M
	PHA, // PusH Accumulator.............. | .. .....       S    M = A
	PHP, // PusH Processor status......... | .. .....       S    M = F
	PLA, // PuLl Accumulator.............. | N. ...Z. A     S      = M (stack)
	PLP, // PuLl Processor status......... | NV BDIZC       S      = M (stack)
	ROL, // ROtate Left................... | N. ...ZC A            = C A rotated     //                               or N. ...ZC            M = C M rotated
	ROR, // ROtate Right.................. | N. ...ZC A            = C A rotated     //                               or N. ...ZC            M = C M rotated
	RTI, // ReTurn from Interrupt......... | NV BDIZC         PC   = M (stack)
	RTS, // ReTurn from Subroutine........ | .. .....         PC   = M (stack)
	SBC, // SuBtract with Carry........... | NV ...ZC A            = A-M-(1-C)
	SEC, // SEt Carry flag................ | .. ....C              = 1
	SED, // SEt Decimal flag.............. | .. .D...              = 1
	SEI, // SEt Interrupt disable......... | .. ..I..              = 1
	STA, // STore Accumulator............. | .. .....            M = A
	STX, // STore X register.............. | .. .....            M = X
	STY, // STore Y register.............. | .. .....            M = Y
	TAX, // Transfer Accumulator to X..... | N. ...Z.   X          = A
	TAY, // Transfer Accumulator to Y..... | N. ...Z.     Y        = A
	TSX, // Transfer Stack pointer to X... | N. ...Z.   X          = S
	TXA, // Transfer X to Accumulator..... | N. ...Z. A            = X
	TXS, // Transfer X to Stack pointer... | .. .....       S      = X
	TYA, // Transfer Y to Accumulator..... | N. ...Z. A            = Y
}


#[derive(Copy, Clone)]
pub enum OpInput {
	UseImplied,
	UseImmediate(u8),
	UseRelative(i8),
	UseAddress(u16),
}


#[derive(Copy, Clone)]
pub enum AMode {
	Accumulator,      // 1    LSR A        work directly on accumulator
	Implied,          // 1    BRK
	Immediate,        // 2    LDA #10      8-bit constant in instruction
	ZeroPage,         // 2    LDA $00      zero-page address
	ZeroPageX,        // 2    LDA $80,X    address is X register + 8-bit constant
	ZeroPageY,        // 2    LDX $10,Y    address is Y register + 8-bit constant
	Relative,         // 2    BNE LABEL    branch target as signed relative offset
	Absolute,         // 3    JMP $1000    full 16-bit address
	AbsoluteX,        // 3    STA $1000,X  full 16-bit address plus X register
	AbsoluteY,        // 3    STA $1000,Y  full 16-bit address plus Y register
	Indirect,         // 3    JMP ($1000)  jump to address stored at address
	IndexedIndirectX, // 2    LDA ($10,X)  load from address stored at (constant
	IndirectIndexedY, // 2    LDA ($10),Y  load from (address stored at constant
}

fn arr_to_addr(arr: &[u8]) -> u16 {
	debug_assert!(arr.len() == 2);

	let x = (arr[0] as u16) + ((arr[1] as u16) << 8);
	x
}

impl AMode {
	pub fn extra_bytes(self) -> i32 {
		match self {
			AMode::Accumulator => 0,
			AMode::Implied => 0,
			AMode::Immediate => 1,
			AMode::ZeroPage => 1,
			AMode::ZeroPageX => 1,
			AMode::ZeroPageY => 1,
			AMode::Relative => 1,
			AMode::Absolute => 2,
			AMode::AbsoluteX => 2,
			AMode::AbsoluteY => 2,
			AMode::Indirect => 2,
			AMode::IndexedIndirectX => 1,
			AMode::IndirectIndexedY => 1,
		}
	}

	pub fn process(self, cpu: &CPU, arr: &[u8]) -> OpInput {
		let x = cpu.x as u8;
		let y = cpu.y as u8;

		let memory = &cpu.mem;

		match self {
			AMode::Accumulator | AMode::Implied => OpInput::UseImplied,
			AMode::Immediate => OpInput::UseImmediate(arr[0]), // Use [u8, ..1] specified in instruction as input
			AMode::ZeroPage => OpInput::UseAddress(arr[0] as u16), // Interpret as zero page address
			AMode::ZeroPageX => OpInput::UseAddress((arr[0] + x) as u16), // Add to X register (as u8 -- the final address is in 0-page)
			AMode::ZeroPageY => OpInput::UseAddress((arr[0] + y) as u16), // Add to Y register (as u8 -- the final address is in 0-page)
			AMode::Relative => OpInput::UseRelative(arr[0] as i8), // Use [u8, ..1] from instruction
			AMode::Absolute => OpInput::UseAddress(arr_to_addr(arr)),
			AMode::AbsoluteX => OpInput::UseAddress(arr_to_addr(arr) + x as u16),
			AMode::AbsoluteY => OpInput::UseAddress(arr_to_addr(arr) + y as u16),
			AMode::Indirect => {
				// Use [u8, ..2] from instruction as an address. Interpret the
				// two bytes starting at that address as an address.
				// (Output: a 16-bit address)
				// let slice = memory.get_slice(arr_to_addr(arr), 2);
				// OpInput::UseAddress(arr_to_addr(slice))
				OpInput::UseAddress((arr[0] + x) as u16)
			}
			AMode::IndexedIndirectX => {
				// Use [u8, ..1] from instruction
				// Add to X register with 0-page wraparound, like ZeroPageX.
				// This is where the absolute (16-bit) target address is stored.
				// (Output: a 16-bit address)
				// let start = arr[0] + x;
				// let slice = memory.get_slice((start as u16), 2);
				// OpInput::UseAddress(arr_to_addr(slice))
				OpInput::UseAddress((arr[0] + x) as u16)
			}
			AMode::IndirectIndexedY => {
				// Use [u8, ..1] from instruction
				// This is where the absolute (16-bit) target address is stored.
				// Add Y register to this address to get the final address
				// (Output: a 16-bit address)
				// let start = arr[0];
				// let slice = memory.get_slice(start as u16, 2);
				// OpInput::UseAddress(arr_to_addr(slice) + y as i32)
				OpInput::UseAddress((arr[0] + x) as u16)
			}
		}
	}
}

pub type DecodedInstr = (Instruction, OpInput);

pub fn process_opcode(op: u8) -> Option<DecodedInstr> {
	match OPCODES[op as usize] {
		Some((instruction, am)) => {
			let opinput = OpInput::UseImplied;
			Some((instruction, opinput))
		}
		_ => None,
	}
}

pub static OPCODES: [Option<(Instruction, AMode)>; 256] = [
	/*0x00*/ Some((Instruction::BRK, AMode::Implied)),
	/*0x01*/ Some((Instruction::ORA, AMode::IndexedIndirectX)),
	/*0x02*/ None,
	/*0x03*/ None,
	/*0x04*/ None,
	/*0x05*/ Some((Instruction::ORA, AMode::ZeroPage)),
	/*0x06*/ Some((Instruction::ASL, AMode::ZeroPage)),
	/*0x07*/ None,
	/*0x08*/ Some((Instruction::PHP, AMode::Implied)),
	/*0x09*/ Some((Instruction::ORA, AMode::Immediate)),
	/*0x0A*/ Some((Instruction::ASL, AMode::Accumulator)),
	/*0x0B*/ None,
	/*0x0C*/ None,
	/*0x0D*/ Some((Instruction::ORA, AMode::Absolute)),
	/*0x0E*/ Some((Instruction::ASL, AMode::Absolute)),
	/*0x0F*/ None,
	/*0x10*/ Some((Instruction::BPL, AMode::Relative)),
	/*0x11*/ Some((Instruction::ORA, AMode::IndirectIndexedY)),
	/*0x12*/ None,
	/*0x13*/ None,
	/*0x14*/ None,
	/*0x15*/ Some((Instruction::ORA, AMode::ZeroPageX)),
	/*0x16*/ Some((Instruction::ASL, AMode::ZeroPageX)),
	/*0x17*/ None,
	/*0x18*/ Some((Instruction::CLC, AMode::Implied)),
	/*0x19*/ Some((Instruction::ORA, AMode::AbsoluteY)),
	/*0x1A*/ None,
	/*0x1B*/ None,
	/*0x1C*/ None,
	/*0x1D*/ Some((Instruction::ORA, AMode::AbsoluteX)),
	/*0x1E*/ Some((Instruction::ASL, AMode::AbsoluteX)),
	/*0x1F*/ None,
	/*0x20*/ Some((Instruction::JSR, AMode::Absolute)),
	/*0x21*/ Some((Instruction::AND, AMode::IndexedIndirectX)),
	/*0x22*/ None,
	/*0x23*/ None,
	/*0x24*/ Some((Instruction::BIT, AMode::ZeroPage)),
	/*0x25*/ Some((Instruction::AND, AMode::ZeroPage)),
	/*0x26*/ Some((Instruction::ROL, AMode::ZeroPage)),
	/*0x27*/ None,
	/*0x28*/ Some((Instruction::PLP, AMode::Implied)),
	/*0x29*/ Some((Instruction::AND, AMode::Immediate)),
	/*0x2A*/ Some((Instruction::ROL, AMode::Accumulator)),
	/*0x2B*/ None,
	/*0x2C*/ Some((Instruction::BIT, AMode::Absolute)),
	/*0x2D*/ Some((Instruction::AND, AMode::Absolute)),
	/*0x2E*/ Some((Instruction::ROL, AMode::Absolute)),
	/*0x2F*/ None,
	/*0x30*/ Some((Instruction::BMI, AMode::Relative)),
	/*0x31*/ Some((Instruction::AND, AMode::IndirectIndexedY)),
	/*0x32*/ None,
	/*0x33*/ None,
	/*0x34*/ None,
	/*0x35*/ Some((Instruction::AND, AMode::ZeroPageX)),
	/*0x36*/ Some((Instruction::ROL, AMode::ZeroPageX)),
	/*0x37*/ None,
	/*0x38*/ Some((Instruction::SEC, AMode::Implied)),
	/*0x39*/ Some((Instruction::AND, AMode::AbsoluteY)),
	/*0x3A*/ None,
	/*0x3B*/ None,
	/*0x3C*/ None,
	/*0x3D*/ Some((Instruction::AND, AMode::AbsoluteX)),
	/*0x3E*/ Some((Instruction::ROL, AMode::AbsoluteX)),
	/*0x3F*/ None,
	/*0x40*/ Some((Instruction::RTI, AMode::Implied)),
	/*0x41*/ Some((Instruction::EOR, AMode::IndexedIndirectX)),
	/*0x42*/ None,
	/*0x43*/ None,
	/*0x44*/ None,
	/*0x45*/ Some((Instruction::EOR, AMode::ZeroPage)),
	/*0x46*/ Some((Instruction::LSR, AMode::ZeroPage)),
	/*0x47*/ None,
	/*0x48*/ Some((Instruction::PHA, AMode::Implied)),
	/*0x49*/ Some((Instruction::EOR, AMode::Immediate)),
	/*0x4A*/ Some((Instruction::LSR, AMode::Accumulator)),
	/*0x4B*/ None,
	/*0x4C*/ Some((Instruction::JMP, AMode::Absolute)),
	/*0x4D*/ Some((Instruction::EOR, AMode::Absolute)),
	/*0x4E*/ Some((Instruction::LSR, AMode::Absolute)),
	/*0x4F*/ None,
	/*0x50*/ Some((Instruction::BVC, AMode::Relative)),
	/*0x51*/ Some((Instruction::EOR, AMode::IndirectIndexedY)),
	/*0x52*/ None,
	/*0x53*/ None,
	/*0x54*/ None,
	/*0x55*/ Some((Instruction::EOR, AMode::ZeroPageX)),
	/*0x56*/ Some((Instruction::LSR, AMode::ZeroPageX)),
	/*0x57*/ None,
	/*0x58*/ None,
	/*0x59*/ Some((Instruction::EOR, AMode::AbsoluteY)),
	/*0x5A*/ None,
	/*0x5B*/ None,
	/*0x5C*/ None,
	/*0x5D*/ Some((Instruction::EOR, AMode::AbsoluteX)),
	/*0x5E*/ Some((Instruction::LSR, AMode::AbsoluteX)),
	/*0x5F*/ None,
	/*0x60*/ Some((Instruction::RTS, AMode::Implied)),
	/*0x61*/ Some((Instruction::ADC, AMode::IndexedIndirectX)),
	/*0x62*/ None,
	/*0x63*/ None,
	/*0x64*/ None,
	/*0x65*/ Some((Instruction::ADC, AMode::ZeroPage)),
	/*0x66*/ Some((Instruction::ROR, AMode::ZeroPage)),
	/*0x67*/ None,
	/*0x68*/ Some((Instruction::PLA, AMode::Implied)),
	/*0x69*/ Some((Instruction::ADC, AMode::Immediate)),
	/*0x6A*/ Some((Instruction::ROR, AMode::Accumulator)),
	/*0x6B*/ None,
	/*0x6C*/ Some((Instruction::JMP, AMode::Indirect)),
	/*0x6D*/ Some((Instruction::ADC, AMode::Absolute)),
	/*0x6E*/ Some((Instruction::ROR, AMode::Absolute)),
	/*0x6F*/ None,
	/*0x70*/ Some((Instruction::BVS, AMode::Relative)),
	/*0x71*/ Some((Instruction::ADC, AMode::IndirectIndexedY)),
	/*0x72*/ None,
	/*0x73*/ None,
	/*0x74*/ None,
	/*0x75*/ Some((Instruction::ADC, AMode::ZeroPageX)),
	/*0x76*/ Some((Instruction::ROR, AMode::ZeroPageX)),
	/*0x77*/ None,
	/*0x78*/ Some((Instruction::SEI, AMode::Implied)),
	/*0x79*/ Some((Instruction::ADC, AMode::AbsoluteY)),
	/*0x7A*/ None,
	/*0x7B*/ None,
	/*0x7C*/ None,
	/*0x7D*/ Some((Instruction::ADC, AMode::AbsoluteX)),
	/*0x7E*/ Some((Instruction::ROR, AMode::AbsoluteX)),
	/*0x7F*/ None,
	/*0x80*/ None,
	/*0x81*/ Some((Instruction::STA, AMode::IndexedIndirectX)),
	/*0x82*/ None,
	/*0x83*/ None,
	/*0x84*/ Some((Instruction::STY, AMode::ZeroPage)),
	/*0x85*/ Some((Instruction::STA, AMode::ZeroPage)),
	/*0x86*/ Some((Instruction::STX, AMode::ZeroPage)),
	/*0x87*/ None,
	/*0x88*/ Some((Instruction::DEY, AMode::Implied)),
	/*0x89*/ None,
	/*0x8A*/ Some((Instruction::TXA, AMode::Implied)),
	/*0x8B*/ None,
	/*0x8C*/ Some((Instruction::STY, AMode::Absolute)),
	/*0x8D*/ Some((Instruction::STA, AMode::Absolute)),
	/*0x8E*/ Some((Instruction::STX, AMode::Absolute)),
	/*0x8F*/ None,
	/*0x90*/ Some((Instruction::BCC, AMode::Relative)),
	/*0x91*/ Some((Instruction::STA, AMode::IndirectIndexedY)),
	/*0x92*/ None,
	/*0x93*/ None,
	/*0x94*/ Some((Instruction::STY, AMode::ZeroPageX)),
	/*0x95*/ Some((Instruction::STA, AMode::ZeroPageX)),
	/*0x96*/ Some((Instruction::STX, AMode::ZeroPageY)),
	/*0x97*/ None,
	/*0x98*/ Some((Instruction::TYA, AMode::Implied)),
	/*0x99*/ Some((Instruction::STA, AMode::AbsoluteY)),
	/*0x9A*/ Some((Instruction::TXS, AMode::Implied)),
	/*0x9B*/ None,
	/*0x9C*/ None,
	/*0x9D*/ Some((Instruction::STA, AMode::AbsoluteX)),
	/*0x9E*/ None,
	/*0x9F*/ None,
	/*0xA0*/ Some((Instruction::LDY, AMode::Immediate)),
	/*0xA1*/ Some((Instruction::LDA, AMode::IndexedIndirectX)),
	/*0xA2*/ Some((Instruction::LDX, AMode::Immediate)),
	/*0xA3*/ None,
	/*0xA4*/ Some((Instruction::LDY, AMode::ZeroPage)),
	/*0xA5*/ Some((Instruction::LDA, AMode::ZeroPage)),
	/*0xA6*/ Some((Instruction::LDX, AMode::ZeroPage)),
	/*0xA7*/ None,
	/*0xA8*/ Some((Instruction::TAY, AMode::Implied)),
	/*0xA9*/ Some((Instruction::LDA, AMode::Immediate)),
	/*0xAA*/ Some((Instruction::TAX, AMode::Implied)),
	/*0xAB*/ None,
	/*0xAC*/ Some((Instruction::LDY, AMode::Absolute)),
	/*0xAD*/ Some((Instruction::LDA, AMode::Absolute)),
	/*0xAE*/ Some((Instruction::LDX, AMode::Absolute)),
	/*0xAF*/ None,
	/*0xB0*/ Some((Instruction::BCS, AMode::Relative)),
	/*0xB1*/ Some((Instruction::LDA, AMode::IndirectIndexedY)),
	/*0xB2*/ None,
	/*0xB3*/ None,
	/*0xB4*/ Some((Instruction::LDY, AMode::ZeroPageX)),
	/*0xB5*/ Some((Instruction::LDA, AMode::ZeroPageX)),
	/*0xB6*/ Some((Instruction::LDX, AMode::ZeroPageY)),
	/*0xB7*/ None,
	/*0xB8*/ Some((Instruction::CLV, AMode::Implied)),
	/*0xB9*/ Some((Instruction::LDA, AMode::AbsoluteY)),
	/*0xBA*/ Some((Instruction::TSX, AMode::Implied)),
	/*0xBB*/ None,
	/*0xBC*/ Some((Instruction::LDY, AMode::AbsoluteX)),
	/*0xBD*/ Some((Instruction::LDA, AMode::AbsoluteX)),
	/*0xBE*/ Some((Instruction::LDX, AMode::AbsoluteY)),
	/*0xBF*/ None,
	/*0xC0*/ Some((Instruction::CPY, AMode::Immediate)),
	/*0xC1*/ Some((Instruction::CMP, AMode::IndexedIndirectX)),
	/*0xC2*/ None,
	/*0xC3*/ None,
	/*0xC4*/ Some((Instruction::CPY, AMode::ZeroPage)),
	/*0xC5*/ Some((Instruction::CMP, AMode::ZeroPage)),
	/*0xC6*/ Some((Instruction::DEC, AMode::ZeroPage)),
	/*0xC7*/ None,
	/*0xC8*/ Some((Instruction::INY, AMode::Implied)),
	/*0xC9*/ Some((Instruction::CMP, AMode::Immediate)),
	/*0xCA*/ Some((Instruction::DEX, AMode::Implied)),
	/*0xCB*/ None,
	/*0xCC*/ Some((Instruction::CPY, AMode::Absolute)),
	/*0xCD*/ Some((Instruction::CMP, AMode::Absolute)),
	/*0xCE*/ Some((Instruction::DEC, AMode::Absolute)),
	/*0xCF*/ None,
	/*0xD0*/ Some((Instruction::BNE, AMode::Relative)),
	/*0xD1*/ Some((Instruction::CMP, AMode::IndirectIndexedY)),
	/*0xD2*/ None,
	/*0xD3*/ None,
	/*0xD4*/ None,
	/*0xD5*/ Some((Instruction::CMP, AMode::ZeroPageX)),
	/*0xD6*/ Some((Instruction::DEC, AMode::ZeroPageX)),
	/*0xD7*/ None,
	/*0xD8*/ Some((Instruction::CLD, AMode::Implied)),
	/*0xD9*/ Some((Instruction::CMP, AMode::AbsoluteY)),
	/*0xDA*/ None,
	/*0xDB*/ None,
	/*0xDC*/ None,
	/*0xDD*/ Some((Instruction::CMP, AMode::AbsoluteX)),
	/*0xDE*/ Some((Instruction::DEC, AMode::AbsoluteX)),
	/*0xDF*/ None,
	/*0xE0*/ Some((Instruction::CPX, AMode::Immediate)),
	/*0xE1*/ Some((Instruction::SBC, AMode::IndexedIndirectX)),
	/*0xE2*/ None,
	/*0xE3*/ None,
	/*0xE4*/ Some((Instruction::CPX, AMode::ZeroPage)),
	/*0xE5*/ Some((Instruction::SBC, AMode::ZeroPage)),
	/*0xE6*/ Some((Instruction::INC, AMode::ZeroPage)),
	/*0xE7*/ None,
	/*0xE8*/ Some((Instruction::INX, AMode::Implied)),
	/*0xE9*/ Some((Instruction::SBC, AMode::Immediate)),
	/*0xEA*/ Some((Instruction::NOP, AMode::Implied)),
	/*0xEB*/ None,
	/*0xEC*/ Some((Instruction::CPX, AMode::Absolute)),
	/*0xED*/ Some((Instruction::SBC, AMode::Absolute)),
	/*0xEE*/ Some((Instruction::INC, AMode::Absolute)),
	/*0xEF*/ None,
	/*0xF0*/ Some((Instruction::BEQ, AMode::Relative)),
	/*0xF1*/ Some((Instruction::SBC, AMode::IndirectIndexedY)),
	/*0xF2*/ None,
	/*0xF3*/ None,
	/*0xF4*/ None,
	/*0xF5*/ Some((Instruction::SBC, AMode::ZeroPageX)),
	/*0xF6*/ Some((Instruction::INC, AMode::ZeroPageX)),
	/*0xF7*/ None,
	/*0xF8*/ Some((Instruction::SED, AMode::Implied)),
	/*0xF9*/ Some((Instruction::SBC, AMode::AbsoluteY)),
	/*0xFA*/ None,
	/*0xFB*/ None,
	/*0xFC*/ None,
	/*0xFD*/ Some((Instruction::SBC, AMode::AbsoluteX)),
	/*0xFE*/ Some((Instruction::INC, AMode::AbsoluteX)),
	/*0xFF*/ None,
];
