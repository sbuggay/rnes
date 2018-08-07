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

pub fn process_opcode(op: u8) -> Option<(Instruction, AMode)> {
	OPCODES[op as usize]
}

pub static OPCODES: [Option<(Instruction, AMode)>; 256] = [
	/*0x00*/ Some((Instruction::BRK, AMode::Implied)),
	/*0x01*/ Some((Instruction::ORA, AMode::IndexedIndirectX)),
	/*0x02*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x03*/ Some((Instruction::SLO, AMode::IndexedIndirectX)),
	/*0x04*/ Some((Instruction::NOP, AMode::ZeroPage)),
	/*0x05*/ Some((Instruction::ORA, AMode::ZeroPage)),
	/*0x06*/ Some((Instruction::ASL, AMode::ZeroPage)),
	/*0x07*/ Some((Instruction::SLO, AMode::ZeroPage)),
	/*0x08*/ Some((Instruction::PHP, AMode::Implied)),
	/*0x09*/ Some((Instruction::ORA, AMode::Immediate)),
	/*0x0A*/ Some((Instruction::ASL, AMode::Accumulator)),
	/*0x0B*/ Some((Instruction::ANC, AMode::Immediate)),
	/*0x0C*/ Some((Instruction::NOP, AMode::Absolute)),
	/*0x0D*/ Some((Instruction::ORA, AMode::Absolute)),
	/*0x0E*/ Some((Instruction::ASL, AMode::Absolute)),
	/*0x0F*/ Some((Instruction::SLO, AMode::Absolute)),
	/*0x10*/ Some((Instruction::BPL, AMode::Relative)),
	/*0x11*/ Some((Instruction::ORA, AMode::IndirectIndexedY)),
	/*0x12*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x13*/ Some((Instruction::SLO, AMode::IndirectIndexedY)),
	/*0x14*/ Some((Instruction::NOP, AMode::ZeroPageX)),
	/*0x15*/ Some((Instruction::ORA, AMode::ZeroPageX)),
	/*0x16*/ Some((Instruction::ASL, AMode::ZeroPageX)),
	/*0x17*/ Some((Instruction::SLO, AMode::ZeroPageX)),
	/*0x18*/ Some((Instruction::CLC, AMode::Implied)),
	/*0x19*/ Some((Instruction::ORA, AMode::AbsoluteY)),
	/*0x1A*/ Some((Instruction::NOP, AMode::Implied)),
	/*0x1B*/ Some((Instruction::SLO, AMode::AbsoluteY)),
	/*0x1C*/ Some((Instruction::NOP, AMode::AbsoluteX)),
	/*0x1D*/ Some((Instruction::ORA, AMode::AbsoluteX)),
	/*0x1E*/ Some((Instruction::ASL, AMode::AbsoluteX)),
	/*0x1F*/ Some((Instruction::SLO, AMode::AbsoluteX)),
	/*0x20*/ Some((Instruction::JSR, AMode::Absolute)),
	/*0x21*/ Some((Instruction::AND, AMode::IndexedIndirectX)),
	/*0x22*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x23*/ Some((Instruction::RLA, AMode::IndexedIndirectX)),
	/*0x24*/ Some((Instruction::BIT, AMode::ZeroPage)),
	/*0x25*/ Some((Instruction::AND, AMode::ZeroPage)),
	/*0x26*/ Some((Instruction::ROL, AMode::ZeroPage)),
	/*0x27*/ Some((Instruction::RLA, AMode::ZeroPage)),
	/*0x28*/ Some((Instruction::PLP, AMode::Implied)),
	/*0x29*/ Some((Instruction::AND, AMode::Immediate)),
	/*0x2A*/ Some((Instruction::ROL, AMode::Accumulator)),
	/*0x2B*/ Some((Instruction::ANC, AMode::Immediate)),
	/*0x2C*/ Some((Instruction::BIT, AMode::Absolute)),
	/*0x2D*/ Some((Instruction::AND, AMode::Absolute)),
	/*0x2E*/ Some((Instruction::ROL, AMode::Absolute)),
	/*0x2F*/ Some((Instruction::RLA, AMode::Absolute)),
	/*0x30*/ Some((Instruction::BMI, AMode::Relative)),
	/*0x31*/ Some((Instruction::AND, AMode::IndirectIndexedY)),
	/*0x32*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x33*/ Some((Instruction::RLA, AMode::IndirectIndexedY)),
	/*0x34*/ Some((Instruction::NOP, AMode::ZeroPageX)),
	/*0x35*/ Some((Instruction::AND, AMode::ZeroPageX)),
	/*0x36*/ Some((Instruction::ROL, AMode::ZeroPageX)),
	/*0x37*/ Some((Instruction::RLA, AMode::ZeroPageX)),
	/*0x38*/ Some((Instruction::SEC, AMode::Implied)),
	/*0x39*/ Some((Instruction::AND, AMode::AbsoluteY)),
	/*0x3A*/ Some((Instruction::NOP, AMode::Implied)),
	/*0x3B*/ Some((Instruction::RLA, AMode::AbsoluteY)),
	/*0x3C*/ Some((Instruction::NOP, AMode::AbsoluteX)),
	/*0x3D*/ Some((Instruction::AND, AMode::AbsoluteX)),
	/*0x3E*/ Some((Instruction::ROL, AMode::AbsoluteX)),
	/*0x3F*/ Some((Instruction::RLA, AMode::AbsoluteX)),
	/*0x40*/ Some((Instruction::RTI, AMode::Implied)),
	/*0x41*/ Some((Instruction::EOR, AMode::IndexedIndirectX)),
	/*0x42*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x43*/ Some((Instruction::SRE, AMode::IndexedIndirectX)),
	/*0x44*/ Some((Instruction::NOP, AMode::ZeroPage)),
	/*0x45*/ Some((Instruction::EOR, AMode::ZeroPage)),
	/*0x46*/ Some((Instruction::LSR, AMode::ZeroPage)),
	/*0x47*/ Some((Instruction::SRE, AMode::ZeroPage)),
	/*0x48*/ Some((Instruction::PHA, AMode::Implied)),
	/*0x49*/ Some((Instruction::EOR, AMode::Immediate)),
	/*0x4A*/ Some((Instruction::LSR, AMode::Accumulator)),
	/*0x4B*/ Some((Instruction::ALR, AMode::Immediate)),
	/*0x4C*/ Some((Instruction::JMP, AMode::Absolute)),
	/*0x4D*/ Some((Instruction::EOR, AMode::Absolute)),
	/*0x4E*/ Some((Instruction::LSR, AMode::Absolute)),
	/*0x4F*/ Some((Instruction::SRE, AMode::Absolute)),
	/*0x50*/ Some((Instruction::BVC, AMode::Relative)),
	/*0x51*/ Some((Instruction::EOR, AMode::IndirectIndexedY)),
	/*0x52*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x53*/ Some((Instruction::SRE, AMode::IndirectIndexedY)),
	/*0x54*/ Some((Instruction::NOP, AMode::ZeroPageX)),
	/*0x55*/ Some((Instruction::EOR, AMode::ZeroPageX)),
	/*0x56*/ Some((Instruction::LSR, AMode::ZeroPageX)),
	/*0x57*/ Some((Instruction::SRE, AMode::ZeroPageX)),
	/*0x58*/ Some((Instruction::CLI, AMode::Implied)),
	/*0x59*/ Some((Instruction::EOR, AMode::AbsoluteY)),
	/*0x5A*/ Some((Instruction::NOP, AMode::Implied)),
	/*0x5B*/ Some((Instruction::SRE, AMode::AbsoluteY)),
	/*0x5C*/ Some((Instruction::NOP, AMode::AbsoluteX)),
	/*0x5D*/ Some((Instruction::EOR, AMode::AbsoluteX)),
	/*0x5E*/ Some((Instruction::LSR, AMode::AbsoluteX)),
	/*0x5F*/ Some((Instruction::SRE, AMode::AbsoluteX)),
	/*0x60*/ Some((Instruction::RTS, AMode::Implied)),
	/*0x61*/ Some((Instruction::ADC, AMode::IndexedIndirectX)),
	/*0x62*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x63*/ Some((Instruction::RRA, AMode::IndexedIndirectX)),
	/*0x64*/ Some((Instruction::NOP, AMode::ZeroPage)),
	/*0x65*/ Some((Instruction::ADC, AMode::ZeroPage)),
	/*0x66*/ Some((Instruction::ROR, AMode::ZeroPage)),
	/*0x67*/ Some((Instruction::RRA, AMode::ZeroPage)),
	/*0x68*/ Some((Instruction::PLA, AMode::Implied)),
	/*0x69*/ Some((Instruction::ADC, AMode::Immediate)),
	/*0x6A*/ Some((Instruction::ROR, AMode::Accumulator)),
	/*0x6B*/ Some((Instruction::ARR, AMode::Immediate)),
	/*0x6C*/ Some((Instruction::JMP, AMode::Indirect)),
	/*0x6D*/ Some((Instruction::ADC, AMode::Absolute)),
	/*0x6E*/ Some((Instruction::ROR, AMode::Absolute)),
	/*0x6F*/ Some((Instruction::RRA, AMode::Absolute)),
	/*0x70*/ Some((Instruction::BVS, AMode::Relative)),
	/*0x71*/ Some((Instruction::ADC, AMode::IndirectIndexedY)),
	/*0x72*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x73*/ Some((Instruction::RRA, AMode::IndirectIndexedY)),
	/*0x74*/ Some((Instruction::NOP, AMode::ZeroPageX)),
	/*0x75*/ Some((Instruction::ADC, AMode::ZeroPageX)),
	/*0x76*/ Some((Instruction::ROR, AMode::ZeroPageX)),
	/*0x77*/ Some((Instruction::RRA, AMode::ZeroPageX)),
	/*0x78*/ Some((Instruction::SEI, AMode::Implied)),
	/*0x79*/ Some((Instruction::ADC, AMode::AbsoluteY)),
	/*0x7A*/ Some((Instruction::NOP, AMode::Implied)),
	/*0x7B*/ Some((Instruction::RRA, AMode::AbsoluteY)),
	/*0x7C*/ Some((Instruction::NOP, AMode::AbsoluteX)),
	/*0x7D*/ Some((Instruction::ADC, AMode::AbsoluteX)),
	/*0x7E*/ Some((Instruction::ROR, AMode::AbsoluteX)),
	/*0x7F*/ Some((Instruction::RRA, AMode::AbsoluteX)),
	/*0x80*/ Some((Instruction::NOP, AMode::Immediate)),
	/*0x81*/ Some((Instruction::STA, AMode::IndexedIndirectX)),
	/*0x82*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x83*/ Some((Instruction::SAX, AMode::IndexedIndirectX)),
	/*0x84*/ Some((Instruction::STY, AMode::ZeroPage)),
	/*0x85*/ Some((Instruction::STA, AMode::ZeroPage)),
	/*0x86*/ Some((Instruction::STX, AMode::ZeroPage)),
	/*0x87*/ Some((Instruction::SAX, AMode::ZeroPage)),
	/*0x88*/ Some((Instruction::DEY, AMode::Implied)),
	/*0x89*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x8A*/ Some((Instruction::TXA, AMode::Implied)),
	/*0x8B*/ Some((Instruction::XAA, AMode::Immediate)),
	/*0x8C*/ Some((Instruction::STY, AMode::Absolute)),
	/*0x8D*/ Some((Instruction::STA, AMode::Absolute)),
	/*0x8E*/ Some((Instruction::STX, AMode::Absolute)),
	/*0x8F*/ Some((Instruction::SAX, AMode::Absolute)),
	/*0x90*/ Some((Instruction::BCC, AMode::Relative)),
	/*0x91*/ Some((Instruction::STA, AMode::IndirectIndexedY)),
	/*0x92*/ Some((Instruction::KIL, AMode::Implied)),
	/*0x93*/ Some((Instruction::AHX, AMode::IndirectIndexedY)),
	/*0x94*/ Some((Instruction::STY, AMode::ZeroPageX)),
	/*0x95*/ Some((Instruction::STA, AMode::ZeroPageX)),
	/*0x96*/ Some((Instruction::STX, AMode::ZeroPageY)),
	/*0x97*/ Some((Instruction::SAX, AMode::ZeroPageY)),
	/*0x98*/ Some((Instruction::TYA, AMode::Implied)),
	/*0x99*/ Some((Instruction::STA, AMode::AbsoluteY)),
	/*0x9A*/ Some((Instruction::TXS, AMode::Implied)),
	/*0x9B*/ Some((Instruction::TAS, AMode::AbsoluteY)),
	/*0x9C*/ Some((Instruction::SHY, AMode::AbsoluteX)),
	/*0x9D*/ Some((Instruction::STA, AMode::AbsoluteX)),
	/*0x9E*/ Some((Instruction::SHX, AMode::AbsoluteY)),
	/*0x9F*/ Some((Instruction::AHX, AMode::AbsoluteY)),
	/*0xA0*/ Some((Instruction::LDY, AMode::Immediate)),
	/*0xA1*/ Some((Instruction::LDA, AMode::IndexedIndirectX)),
	/*0xA2*/ Some((Instruction::LDX, AMode::Immediate)),
	/*0xA3*/ Some((Instruction::LAX, AMode::IndexedIndirectX)),
	/*0xA4*/ Some((Instruction::LDY, AMode::ZeroPage)),
	/*0xA5*/ Some((Instruction::LDA, AMode::ZeroPage)),
	/*0xA6*/ Some((Instruction::LDX, AMode::ZeroPage)),
	/*0xA7*/ Some((Instruction::LAX, AMode::ZeroPage)),
	/*0xA8*/ Some((Instruction::TAY, AMode::Implied)),
	/*0xA9*/ Some((Instruction::LDA, AMode::Immediate)),
	/*0xAA*/ Some((Instruction::TAX, AMode::Implied)),
	/*0xAB*/ Some((Instruction::LAX, AMode::Immediate)),
	/*0xAC*/ Some((Instruction::LDY, AMode::Absolute)),
	/*0xAD*/ Some((Instruction::LDA, AMode::Absolute)),
	/*0xAE*/ Some((Instruction::LDX, AMode::Absolute)),
	/*0xAF*/ Some((Instruction::LAX, AMode::Absolute)),
	/*0xB0*/ Some((Instruction::BCS, AMode::Relative)),
	/*0xB1*/ Some((Instruction::LDA, AMode::IndirectIndexedY)),
	/*0xB2*/ Some((Instruction::KIL, AMode::Implied)),
	/*0xB3*/ Some((Instruction::LAX, AMode::IndirectIndexedY)),
	/*0xB4*/ Some((Instruction::LDY, AMode::ZeroPageX)),
	/*0xB5*/ Some((Instruction::LDA, AMode::ZeroPageX)),
	/*0xB6*/ Some((Instruction::LDX, AMode::ZeroPageY)),
	/*0xB7*/ Some((Instruction::LAX, AMode::ZeroPageY)),
	/*0xB8*/ Some((Instruction::CLV, AMode::Implied)),
	/*0xB9*/ Some((Instruction::LDA, AMode::AbsoluteY)),
	/*0xBA*/ Some((Instruction::TSX, AMode::Implied)),
	/*0xBB*/ Some((Instruction::LAS, AMode::AbsoluteY)),
	/*0xBC*/ Some((Instruction::LDY, AMode::AbsoluteX)),
	/*0xBD*/ Some((Instruction::LDA, AMode::AbsoluteX)),
	/*0xBE*/ Some((Instruction::LDX, AMode::AbsoluteY)),
	/*0xBF*/ Some((Instruction::LAX, AMode::AbsoluteY)),
	/*0xC0*/ Some((Instruction::CPY, AMode::Immediate)),
	/*0xC1*/ Some((Instruction::CMP, AMode::IndexedIndirectX)),
	/*0xC2*/ Some((Instruction::NOP, AMode::Immediate)),
	/*0xC3*/ Some((Instruction::DCP, AMode::IndexedIndirectX)),
	/*0xC4*/ Some((Instruction::CPY, AMode::ZeroPage)),
	/*0xC5*/ Some((Instruction::CMP, AMode::ZeroPage)),
	/*0xC6*/ Some((Instruction::DEC, AMode::ZeroPage)),
	/*0xC7*/ Some((Instruction::DCP, AMode::ZeroPage)),
	/*0xC8*/ Some((Instruction::INY, AMode::Implied)),
	/*0xC9*/ Some((Instruction::CMP, AMode::Immediate)),
	/*0xCA*/ Some((Instruction::DEX, AMode::Implied)),
	/*0xCB*/ Some((Instruction::AXS, AMode::Immediate)),
	/*0xCC*/ Some((Instruction::CPY, AMode::Absolute)),
	/*0xCD*/ Some((Instruction::CMP, AMode::Absolute)),
	/*0xCE*/ Some((Instruction::DEC, AMode::Absolute)),
	/*0xCF*/ Some((Instruction::DCP, AMode::Absolute)),
	/*0xD0*/ Some((Instruction::BNE, AMode::Relative)),
	/*0xD1*/ Some((Instruction::CMP, AMode::IndirectIndexedY)),
	/*0xD2*/ Some((Instruction::KIL, AMode::Implied)),
	/*0xD3*/ Some((Instruction::DCP, AMode::IndirectIndexedY)),
	/*0xD4*/ Some((Instruction::NOP, AMode::ZeroPageX)),
	/*0xD5*/ Some((Instruction::CMP, AMode::ZeroPageX)),
	/*0xD6*/ Some((Instruction::DEC, AMode::ZeroPageX)),
	/*0xD7*/ Some((Instruction::DCP, AMode::ZeroPageX)),
	/*0xD8*/ Some((Instruction::CLD, AMode::Implied)),
	/*0xD9*/ Some((Instruction::CMP, AMode::AbsoluteY)),
	/*0xDA*/ Some((Instruction::NOP, AMode::Implied)),
	/*0xDB*/ Some((Instruction::DCP, AMode::AbsoluteY)),
	/*0xDC*/ Some((Instruction::NOP, AMode::AbsoluteX)),
	/*0xDD*/ Some((Instruction::CMP, AMode::AbsoluteX)),
	/*0xDE*/ Some((Instruction::DEC, AMode::AbsoluteX)),
	/*0xDF*/ Some((Instruction::DCP, AMode::AbsoluteX)),
	/*0xE0*/ Some((Instruction::CPX, AMode::Immediate)),
	/*0xE1*/ Some((Instruction::SBC, AMode::IndexedIndirectX)),
	/*0xE2*/ Some((Instruction::NOP, AMode::Implied)),
	/*0xE3*/ Some((Instruction::ISC, AMode::IndexedIndirectX)),
	/*0xE4*/ Some((Instruction::CPX, AMode::ZeroPage)),
	/*0xE5*/ Some((Instruction::SBC, AMode::ZeroPage)),
	/*0xE6*/ Some((Instruction::INC, AMode::ZeroPage)),
	/*0xE7*/ Some((Instruction::ISC, AMode::ZeroPage)),
	/*0xE8*/ Some((Instruction::INX, AMode::Implied)),
	/*0xE9*/ Some((Instruction::SBC, AMode::Immediate)),
	/*0xEA*/ Some((Instruction::NOP, AMode::Implied)),
	/*0xEB*/ Some((Instruction::SBC, AMode::Immediate)),
	/*0xEC*/ Some((Instruction::CPX, AMode::Absolute)),
	/*0xED*/ Some((Instruction::SBC, AMode::Absolute)),
	/*0xEE*/ Some((Instruction::INC, AMode::Absolute)),
	/*0xEF*/ Some((Instruction::ISC, AMode::Absolute)),
	/*0xF0*/ Some((Instruction::BEQ, AMode::Relative)),
	/*0xF1*/ Some((Instruction::SBC, AMode::IndirectIndexedY)),
	/*0xF2*/ Some((Instruction::KIL, AMode::Implied)),
	/*0xF3*/ Some((Instruction::ISC, AMode::IndirectIndexedY)),
	/*0xF4*/ Some((Instruction::NOP, AMode::ZeroPageX)),
	/*0xF5*/ Some((Instruction::SBC, AMode::ZeroPageX)),
	/*0xF6*/ Some((Instruction::INC, AMode::ZeroPageX)),
	/*0xF7*/ Some((Instruction::ISC, AMode::ZeroPageX)),
	/*0xF8*/ Some((Instruction::SED, AMode::Implied)),
	/*0xF9*/ Some((Instruction::SBC, AMode::AbsoluteY)),
	/*0xFA*/ Some((Instruction::NOP, AMode::Implied)),
	/*0xFB*/ Some((Instruction::ISC, AMode::AbsoluteY)),
	/*0xFC*/ Some((Instruction::NOP, AMode::AbsoluteX)),
	/*0xFD*/ Some((Instruction::SBC, AMode::AbsoluteX)),
	/*0xFE*/ Some((Instruction::INC, AMode::AbsoluteX)),
	/*0xFF*/ Some((Instruction::ISC, AMode::AbsoluteX)),
];
