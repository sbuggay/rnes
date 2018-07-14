mod cpu;
mod instructions;
mod memory;
mod rom;

use std::fs;

fn main() {
	let mut cpu = cpu::CPU::new();
	
	let mut file = fs::File::open("testroms/nestest.nes").expect("No file found");
	let r = rom::Rom::load(&mut file);

	println!("{:?}", r);

	r.dump();

	cpu.mem = r.rom;

	cpu.dump();
	cpu.emulate();
	cpu.emulate();
	cpu.emulate();
	cpu.emulate();
	cpu.emulate();
	cpu.emulate();
	cpu.emulate();
	cpu.emulate();
	cpu.emulate();
}
