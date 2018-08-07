use cpu::CPU;
use std::fmt;

use std::num::Wrapping;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
	ADC, // ADd with Carry................ | NV ...ZC A            = A + M + C
	AND, // logical AND (bitwise)......... | N. ...Z. A            = A && M
	ASL, // Arithmetic Shift Left......... | N. ...ZC A            = M << 1
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

	ALR,
	AHX,
	ARR,
	ANC,
	AXS,
	DCP,
	ISC,
	LAS,
	LAX,
	RLA,
	RRA,
	SAX,
	SHX,
	SHY,
	SRE,
	SLO,
	TAS,
	XAA,

	KIL
}

#[derive(Copy, Clone, Debug)]
pub enum OpInput {
	Implied,
	Immediate(u8),
	Relative(i8),
	Address(u16),
}

#[derive(Copy, Clone, Debug)]
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
	println!("{:X}", x);

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
		
		match self {
			AMode::Accumulator | AMode::Implied => OpInput::Implied,
			AMode::Immediate => OpInput::Immediate(arr[0]), // Use [u8, ..1] specified in instruction as input
			AMode::ZeroPage => OpInput::Address(arr[0] as u16), // Interpret as zero page address
			AMode::ZeroPageX => OpInput::Address((Wrapping(arr[0]) + Wrapping(x)).0 as u16), // Add to X register (as u8 -- the final address is in 0-page)
			AMode::ZeroPageY => OpInput::Address((Wrapping(arr[0]) + Wrapping(y)).0 as u16), // Add to Y register (as u8 -- the final address is in 0-page)
			AMode::Relative => OpInput::Relative(arr[0] as i8),        // Use [u8, ..1] from instruction
			AMode::Absolute => OpInput::Address(arr_to_addr(arr)),
			AMode::AbsoluteX => OpInput::Address((Wrapping(arr_to_addr(arr)) + Wrapping(x as u16)).0),
			AMode::AbsoluteY => OpInput::Address((Wrapping(arr_to_addr(arr)) + Wrapping(y as u16)).0),
			AMode::Indirect => {
				// Use [u8, ..2] from instruction as an address. Interpret the
				// two bytes starting at that address as an address.
				// (Output: a 16-bit address)
				let addr = arr_to_addr(arr);
				let start = cpu.mem[addr as usize];
				let mut end = cpu.mem[(addr + 1) as usize];
				if addr & 0xFF == 0xFF {
					end = cpu.mem[(addr & 0xFF00) as usize];
				}
				// let slice = &cpu.mem[start..end];
				OpInput::Address(arr_to_addr(&[start, end]))
			}	
			AMode::IndexedIndirectX => {
				// Use [u8, ..1] from instruction
				// Add to X register with 0-page wraparound, like ZeroPageX.
				// This is where the absolute (16-bit) target address is stored.
				// (Output: a 16-bit address)
				let start = (((arr[0] as usize) + x as usize) & 0xFF) as usize;	
				let end = (start + 1) & 0xFF as usize;
				let slice = &[cpu.mem[start], cpu.mem[end]];
				OpInput::Address(arr_to_addr(slice))
			}
			AMode::IndirectIndexedY => {
				// Use [u8, ..1] from instruction
				// This is where the absolute (16-bit) target address is stored.
				// Add Y register to this address to get the final address
				// (Output: a 16-bit address)
				let start = (((arr[0] as usize)) & 0xFF) as usize;
				let end = (start + 1) & 0xFF as usize;	
				let slice = &[cpu.mem[start], cpu.mem[end]];
				let addr = Wrapping(arr_to_addr(slice)) + Wrapping(y as u16);
				OpInput::Address(addr.0)
			}
		}
	}
}

pub fn process_opcode(op: u8) -> (Instruction, AMode) {
	OPCODES[op as usize]
}

pub static OPCODES: [(Instruction, AMode); 256] = [
	/*0x00*/ (Instruction::BRK, AMode::Implied),
	/*0x01*/ (Instruction::ORA, AMode::IndexedIndirectX),
	/*0x02*/ (Instruction::KIL, AMode::Implied),
	/*0x03*/ (Instruction::SLO, AMode::IndexedIndirectX),
	/*0x04*/ (Instruction::NOP, AMode::ZeroPage),
	/*0x05*/ (Instruction::ORA, AMode::ZeroPage),
	/*0x06*/ (Instruction::ASL, AMode::ZeroPage),
	/*0x07*/ (Instruction::SLO, AMode::ZeroPage),
	/*0x08*/ (Instruction::PHP, AMode::Implied),
	/*0x09*/ (Instruction::ORA, AMode::Immediate),
	/*0x0A*/ (Instruction::ASL, AMode::Accumulator),
	/*0x0B*/ (Instruction::ANC, AMode::Immediate),
	/*0x0C*/ (Instruction::NOP, AMode::Absolute),
	/*0x0D*/ (Instruction::ORA, AMode::Absolute),
	/*0x0E*/ (Instruction::ASL, AMode::Absolute),
	/*0x0F*/ (Instruction::SLO, AMode::Absolute),
	/*0x10*/ (Instruction::BPL, AMode::Relative),
	/*0x11*/ (Instruction::ORA, AMode::IndirectIndexedY),
	/*0x12*/ (Instruction::KIL, AMode::Implied),
	/*0x13*/ (Instruction::SLO, AMode::IndirectIndexedY),
	/*0x14*/ (Instruction::NOP, AMode::ZeroPageX),
	/*0x15*/ (Instruction::ORA, AMode::ZeroPageX),
	/*0x16*/ (Instruction::ASL, AMode::ZeroPageX),
	/*0x17*/ (Instruction::SLO, AMode::ZeroPageX),
	/*0x18*/ (Instruction::CLC, AMode::Implied),
	/*0x19*/ (Instruction::ORA, AMode::AbsoluteY),
	/*0x1A*/ (Instruction::NOP, AMode::Implied),
	/*0x1B*/ (Instruction::SLO, AMode::AbsoluteY),
	/*0x1C*/ (Instruction::NOP, AMode::AbsoluteX),
	/*0x1D*/ (Instruction::ORA, AMode::AbsoluteX),
	/*0x1E*/ (Instruction::ASL, AMode::AbsoluteX),
	/*0x1F*/ (Instruction::SLO, AMode::AbsoluteX),
	/*0x20*/ (Instruction::JSR, AMode::Absolute),
	/*0x21*/ (Instruction::AND, AMode::IndexedIndirectX),
	/*0x22*/ (Instruction::KIL, AMode::Implied),
	/*0x23*/ (Instruction::RLA, AMode::IndexedIndirectX),
	/*0x24*/ (Instruction::BIT, AMode::ZeroPage),
	/*0x25*/ (Instruction::AND, AMode::ZeroPage),
	/*0x26*/ (Instruction::ROL, AMode::ZeroPage),
	/*0x27*/ (Instruction::RLA, AMode::ZeroPage),
	/*0x28*/ (Instruction::PLP, AMode::Implied),
	/*0x29*/ (Instruction::AND, AMode::Immediate),
	/*0x2A*/ (Instruction::ROL, AMode::Accumulator),
	/*0x2B*/ (Instruction::ANC, AMode::Immediate),
	/*0x2C*/ (Instruction::BIT, AMode::Absolute),
	/*0x2D*/ (Instruction::AND, AMode::Absolute),
	/*0x2E*/ (Instruction::ROL, AMode::Absolute),
	/*0x2F*/ (Instruction::RLA, AMode::Absolute),
	/*0x30*/ (Instruction::BMI, AMode::Relative),
	/*0x31*/ (Instruction::AND, AMode::IndirectIndexedY),
	/*0x32*/ (Instruction::KIL, AMode::Implied),
	/*0x33*/ (Instruction::RLA, AMode::IndirectIndexedY),
	/*0x34*/ (Instruction::NOP, AMode::ZeroPageX),
	/*0x35*/ (Instruction::AND, AMode::ZeroPageX),
	/*0x36*/ (Instruction::ROL, AMode::ZeroPageX),
	/*0x37*/ (Instruction::RLA, AMode::ZeroPageX),
	/*0x38*/ (Instruction::SEC, AMode::Implied),
	/*0x39*/ (Instruction::AND, AMode::AbsoluteY),
	/*0x3A*/ (Instruction::NOP, AMode::Implied),
	/*0x3B*/ (Instruction::RLA, AMode::AbsoluteY),
	/*0x3C*/ (Instruction::NOP, AMode::AbsoluteX),
	/*0x3D*/ (Instruction::AND, AMode::AbsoluteX),
	/*0x3E*/ (Instruction::ROL, AMode::AbsoluteX),
	/*0x3F*/ (Instruction::RLA, AMode::AbsoluteX),
	/*0x40*/ (Instruction::RTI, AMode::Implied),
	/*0x41*/ (Instruction::EOR, AMode::IndexedIndirectX),
	/*0x42*/ (Instruction::KIL, AMode::Implied),
	/*0x43*/ (Instruction::SRE, AMode::IndexedIndirectX),
	/*0x44*/ (Instruction::NOP, AMode::ZeroPage),
	/*0x45*/ (Instruction::EOR, AMode::ZeroPage),
	/*0x46*/ (Instruction::LSR, AMode::ZeroPage),
	/*0x47*/ (Instruction::SRE, AMode::ZeroPage),
	/*0x48*/ (Instruction::PHA, AMode::Implied),
	/*0x49*/ (Instruction::EOR, AMode::Immediate),
	/*0x4A*/ (Instruction::LSR, AMode::Accumulator),
	/*0x4B*/ (Instruction::ALR, AMode::Immediate),
	/*0x4C*/ (Instruction::JMP, AMode::Absolute),
	/*0x4D*/ (Instruction::EOR, AMode::Absolute),
	/*0x4E*/ (Instruction::LSR, AMode::Absolute),
	/*0x4F*/ (Instruction::SRE, AMode::Absolute),
	/*0x50*/ (Instruction::BVC, AMode::Relative),
	/*0x51*/ (Instruction::EOR, AMode::IndirectIndexedY),
	/*0x52*/ (Instruction::KIL, AMode::Implied),
	/*0x53*/ (Instruction::SRE, AMode::IndirectIndexedY),
	/*0x54*/ (Instruction::NOP, AMode::ZeroPageX),
	/*0x55*/ (Instruction::EOR, AMode::ZeroPageX),
	/*0x56*/ (Instruction::LSR, AMode::ZeroPageX),
	/*0x57*/ (Instruction::SRE, AMode::ZeroPageX),
	/*0x58*/ (Instruction::CLI, AMode::Implied),
	/*0x59*/ (Instruction::EOR, AMode::AbsoluteY),
	/*0x5A*/ (Instruction::NOP, AMode::Implied),
	/*0x5B*/ (Instruction::SRE, AMode::AbsoluteY),
	/*0x5C*/ (Instruction::NOP, AMode::AbsoluteX),
	/*0x5D*/ (Instruction::EOR, AMode::AbsoluteX),
	/*0x5E*/ (Instruction::LSR, AMode::AbsoluteX),
	/*0x5F*/ (Instruction::SRE, AMode::AbsoluteX),
	/*0x60*/ (Instruction::RTS, AMode::Implied),
	/*0x61*/ (Instruction::ADC, AMode::IndexedIndirectX),
	/*0x62*/ (Instruction::KIL, AMode::Implied),
	/*0x63*/ (Instruction::RRA, AMode::IndexedIndirectX),
	/*0x64*/ (Instruction::NOP, AMode::ZeroPage),
	/*0x65*/ (Instruction::ADC, AMode::ZeroPage),
	/*0x66*/ (Instruction::ROR, AMode::ZeroPage),
	/*0x67*/ (Instruction::RRA, AMode::ZeroPage),
	/*0x68*/ (Instruction::PLA, AMode::Implied),
	/*0x69*/ (Instruction::ADC, AMode::Immediate),
	/*0x6A*/ (Instruction::ROR, AMode::Accumulator),
	/*0x6B*/ (Instruction::ARR, AMode::Immediate),
	/*0x6C*/ (Instruction::JMP, AMode::Indirect),
	/*0x6D*/ (Instruction::ADC, AMode::Absolute),
	/*0x6E*/ (Instruction::ROR, AMode::Absolute),
	/*0x6F*/ (Instruction::RRA, AMode::Absolute),
	/*0x70*/ (Instruction::BVS, AMode::Relative),
	/*0x71*/ (Instruction::ADC, AMode::IndirectIndexedY),
	/*0x72*/ (Instruction::KIL, AMode::Implied),
	/*0x73*/ (Instruction::RRA, AMode::IndirectIndexedY),
	/*0x74*/ (Instruction::NOP, AMode::ZeroPageX),
	/*0x75*/ (Instruction::ADC, AMode::ZeroPageX),
	/*0x76*/ (Instruction::ROR, AMode::ZeroPageX),
	/*0x77*/ (Instruction::RRA, AMode::ZeroPageX),
	/*0x78*/ (Instruction::SEI, AMode::Implied),
	/*0x79*/ (Instruction::ADC, AMode::AbsoluteY),
	/*0x7A*/ (Instruction::NOP, AMode::Implied),
	/*0x7B*/ (Instruction::RRA, AMode::AbsoluteY),
	/*0x7C*/ (Instruction::NOP, AMode::AbsoluteX),
	/*0x7D*/ (Instruction::ADC, AMode::AbsoluteX),
	/*0x7E*/ (Instruction::ROR, AMode::AbsoluteX),
	/*0x7F*/ (Instruction::RRA, AMode::AbsoluteX),
	/*0x80*/ (Instruction::NOP, AMode::Immediate),
	/*0x81*/ (Instruction::STA, AMode::IndexedIndirectX),
	/*0x82*/ (Instruction::KIL, AMode::Implied),
	/*0x83*/ (Instruction::SAX, AMode::IndexedIndirectX),
	/*0x84*/ (Instruction::STY, AMode::ZeroPage),
	/*0x85*/ (Instruction::STA, AMode::ZeroPage),
	/*0x86*/ (Instruction::STX, AMode::ZeroPage),
	/*0x87*/ (Instruction::SAX, AMode::ZeroPage),
	/*0x88*/ (Instruction::DEY, AMode::Implied),
	/*0x89*/ (Instruction::KIL, AMode::Implied),
	/*0x8A*/ (Instruction::TXA, AMode::Implied),
	/*0x8B*/ (Instruction::XAA, AMode::Immediate),
	/*0x8C*/ (Instruction::STY, AMode::Absolute),
	/*0x8D*/ (Instruction::STA, AMode::Absolute),
	/*0x8E*/ (Instruction::STX, AMode::Absolute),
	/*0x8F*/ (Instruction::SAX, AMode::Absolute),
	/*0x90*/ (Instruction::BCC, AMode::Relative),
	/*0x91*/ (Instruction::STA, AMode::IndirectIndexedY),
	/*0x92*/ (Instruction::KIL, AMode::Implied),
	/*0x93*/ (Instruction::AHX, AMode::IndirectIndexedY),
	/*0x94*/ (Instruction::STY, AMode::ZeroPageX),
	/*0x95*/ (Instruction::STA, AMode::ZeroPageX),
	/*0x96*/ (Instruction::STX, AMode::ZeroPageY),
	/*0x97*/ (Instruction::SAX, AMode::ZeroPageY),
	/*0x98*/ (Instruction::TYA, AMode::Implied),
	/*0x99*/ (Instruction::STA, AMode::AbsoluteY),
	/*0x9A*/ (Instruction::TXS, AMode::Implied),
	/*0x9B*/ (Instruction::TAS, AMode::AbsoluteY),
	/*0x9C*/ (Instruction::SHY, AMode::AbsoluteX),
	/*0x9D*/ (Instruction::STA, AMode::AbsoluteX),
	/*0x9E*/ (Instruction::SHX, AMode::AbsoluteY),
	/*0x9F*/ (Instruction::AHX, AMode::AbsoluteY),
	/*0xA0*/ (Instruction::LDY, AMode::Immediate),
	/*0xA1*/ (Instruction::LDA, AMode::IndexedIndirectX),
	/*0xA2*/ (Instruction::LDX, AMode::Immediate),
	/*0xA3*/ (Instruction::LAX, AMode::IndexedIndirectX),
	/*0xA4*/ (Instruction::LDY, AMode::ZeroPage),
	/*0xA5*/ (Instruction::LDA, AMode::ZeroPage),
	/*0xA6*/ (Instruction::LDX, AMode::ZeroPage),
	/*0xA7*/ (Instruction::LAX, AMode::ZeroPage),
	/*0xA8*/ (Instruction::TAY, AMode::Implied),
	/*0xA9*/ (Instruction::LDA, AMode::Immediate),
	/*0xAA*/ (Instruction::TAX, AMode::Implied),
	/*0xAB*/ (Instruction::LAX, AMode::Immediate),
	/*0xAC*/ (Instruction::LDY, AMode::Absolute),
	/*0xAD*/ (Instruction::LDA, AMode::Absolute),
	/*0xAE*/ (Instruction::LDX, AMode::Absolute),
	/*0xAF*/ (Instruction::LAX, AMode::Absolute),
	/*0xB0*/ (Instruction::BCS, AMode::Relative),
	/*0xB1*/ (Instruction::LDA, AMode::IndirectIndexedY),
	/*0xB2*/ (Instruction::KIL, AMode::Implied),
	/*0xB3*/ (Instruction::LAX, AMode::IndirectIndexedY),
	/*0xB4*/ (Instruction::LDY, AMode::ZeroPageX),
	/*0xB5*/ (Instruction::LDA, AMode::ZeroPageX),
	/*0xB6*/ (Instruction::LDX, AMode::ZeroPageY),
	/*0xB7*/ (Instruction::LAX, AMode::ZeroPageY),
	/*0xB8*/ (Instruction::CLV, AMode::Implied),
	/*0xB9*/ (Instruction::LDA, AMode::AbsoluteY),
	/*0xBA*/ (Instruction::TSX, AMode::Implied),
	/*0xBB*/ (Instruction::LAS, AMode::AbsoluteY),
	/*0xBC*/ (Instruction::LDY, AMode::AbsoluteX),
	/*0xBD*/ (Instruction::LDA, AMode::AbsoluteX),
	/*0xBE*/ (Instruction::LDX, AMode::AbsoluteY),
	/*0xBF*/ (Instruction::LAX, AMode::AbsoluteY),
	/*0xC0*/ (Instruction::CPY, AMode::Immediate),
	/*0xC1*/ (Instruction::CMP, AMode::IndexedIndirectX),
	/*0xC2*/ (Instruction::NOP, AMode::Immediate),
	/*0xC3*/ (Instruction::DCP, AMode::IndexedIndirectX),
	/*0xC4*/ (Instruction::CPY, AMode::ZeroPage),
	/*0xC5*/ (Instruction::CMP, AMode::ZeroPage),
	/*0xC6*/ (Instruction::DEC, AMode::ZeroPage),
	/*0xC7*/ (Instruction::DCP, AMode::ZeroPage),
	/*0xC8*/ (Instruction::INY, AMode::Implied),
	/*0xC9*/ (Instruction::CMP, AMode::Immediate),
	/*0xCA*/ (Instruction::DEX, AMode::Implied),
	/*0xCB*/ (Instruction::AXS, AMode::Immediate),
	/*0xCC*/ (Instruction::CPY, AMode::Absolute),
	/*0xCD*/ (Instruction::CMP, AMode::Absolute),
	/*0xCE*/ (Instruction::DEC, AMode::Absolute),
	/*0xCF*/ (Instruction::DCP, AMode::Absolute),
	/*0xD0*/ (Instruction::BNE, AMode::Relative),
	/*0xD1*/ (Instruction::CMP, AMode::IndirectIndexedY),
	/*0xD2*/ (Instruction::KIL, AMode::Implied),
	/*0xD3*/ (Instruction::DCP, AMode::IndirectIndexedY),
	/*0xD4*/ (Instruction::NOP, AMode::ZeroPageX),
	/*0xD5*/ (Instruction::CMP, AMode::ZeroPageX),
	/*0xD6*/ (Instruction::DEC, AMode::ZeroPageX),
	/*0xD7*/ (Instruction::DCP, AMode::ZeroPageX),
	/*0xD8*/ (Instruction::CLD, AMode::Implied),
	/*0xD9*/ (Instruction::CMP, AMode::AbsoluteY),
	/*0xDA*/ (Instruction::NOP, AMode::Implied),
	/*0xDB*/ (Instruction::DCP, AMode::AbsoluteY),
	/*0xDC*/ (Instruction::NOP, AMode::AbsoluteX),
	/*0xDD*/ (Instruction::CMP, AMode::AbsoluteX),
	/*0xDE*/ (Instruction::DEC, AMode::AbsoluteX),
	/*0xDF*/ (Instruction::DCP, AMode::AbsoluteX),
	/*0xE0*/ (Instruction::CPX, AMode::Immediate),
	/*0xE1*/ (Instruction::SBC, AMode::IndexedIndirectX),
	/*0xE2*/ (Instruction::NOP, AMode::Implied),
	/*0xE3*/ (Instruction::ISC, AMode::IndexedIndirectX),
	/*0xE4*/ (Instruction::CPX, AMode::ZeroPage),
	/*0xE5*/ (Instruction::SBC, AMode::ZeroPage),
	/*0xE6*/ (Instruction::INC, AMode::ZeroPage),
	/*0xE7*/ (Instruction::ISC, AMode::ZeroPage),
	/*0xE8*/ (Instruction::INX, AMode::Implied),
	/*0xE9*/ (Instruction::SBC, AMode::Immediate),
	/*0xEA*/ (Instruction::NOP, AMode::Implied),
	/*0xEB*/ (Instruction::SBC, AMode::Immediate),
	/*0xEC*/ (Instruction::CPX, AMode::Absolute),
	/*0xED*/ (Instruction::SBC, AMode::Absolute),
	/*0xEE*/ (Instruction::INC, AMode::Absolute),
	/*0xEF*/ (Instruction::ISC, AMode::Absolute),
	/*0xF0*/ (Instruction::BEQ, AMode::Relative),
	/*0xF1*/ (Instruction::SBC, AMode::IndirectIndexedY),
	/*0xF2*/ (Instruction::KIL, AMode::Implied),
	/*0xF3*/ (Instruction::ISC, AMode::IndirectIndexedY),
	/*0xF4*/ (Instruction::NOP, AMode::ZeroPageX),
	/*0xF5*/ (Instruction::SBC, AMode::ZeroPageX),
	/*0xF6*/ (Instruction::INC, AMode::ZeroPageX),
	/*0xF7*/ (Instruction::ISC, AMode::ZeroPageX),
	/*0xF8*/ (Instruction::SED, AMode::Implied),
	/*0xF9*/ (Instruction::SBC, AMode::AbsoluteY),
	/*0xFA*/ (Instruction::NOP, AMode::Implied),
	/*0xFB*/ (Instruction::ISC, AMode::AbsoluteY),
	/*0xFC*/ (Instruction::NOP, AMode::AbsoluteX),
	/*0xFD*/ (Instruction::SBC, AMode::AbsoluteX),
	/*0xFE*/ (Instruction::INC, AMode::AbsoluteX),
	/*0xFF*/ (Instruction::ISC, AMode::AbsoluteX),
];
