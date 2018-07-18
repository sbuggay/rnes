use std::fmt;
use std::io::BufReader;
use std::io::{BufRead, Error, ErrorKind, Read, Seek, SeekFrom};

// [todo] reuse CPU struct
pub struct CpuState {
	pub pc: u16,	// program counter
	pub a: u8,		// accumulator
	pub x: u8,		// index register x
	pub y: u8,		// index register y
	pub st: u8,		// processor status (flags)
	pub sp: u8,		// stack pointer
	pub cycle: u16,	// number of cycles
	pub op: u8,		// op currently being run
}

pub struct Simulate {
	pub states: Vec<CpuState>,
}

impl Simulate {
	pub fn load<T: Read + Seek>(fp: &mut T) -> Self {
		let mut vec = Vec::new();

		let reader = BufReader::new(fp);
		for line in reader.lines() {
			let line = line.expect("exists");
			vec.push(parse_line(&line));
		}
		Simulate { states: vec }
	}
}

pub fn parse_line(line: &String) -> CpuState {

	let data: Vec<&str> = line.split_whitespace().collect();

	let data_offset = data.len() - 6;

	let pc = data[0];
	let op = data[1];
	let a = data[data_offset];
	let x = data[data_offset + 1];
	let y = data[data_offset + 2];
	let st = data[data_offset + 3];
	let sp = data[data_offset + 4];
	let cycle = data[data_offset + 5];

	println!("{} {} {} {} {} {} {} {}", pc, op, a, x, y, st, sp, cycle);

	CpuState {
		pc: 0,
		a: 0, 
		x: 0,
		y: 0,
		st: 0,
		sp: 0,
		cycle: 0,
		op: 0,
	}
}
