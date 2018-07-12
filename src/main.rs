mod cpu;
mod opcodes;

fn main() {
	let cpu = cpu::CPU::new();
	cpu.dump();
}