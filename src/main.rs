mod cpu;
mod instructions;
mod memory;
mod rom;


fn main() {
	let cpu = cpu::CPU::new();
	cpu.dump();
	cpu.single_step();

	let data = rom::read_rom("C:/Users/sbugg/Projects/nes/target/debug/nestest.nes");
}
