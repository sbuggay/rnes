mod cpu;

fn main() {
	let cpu = cpu::CPU::new();
	cpu.dump();
}