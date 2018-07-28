use std::fmt;
use std::io::BufReader;
use std::io::{BufRead, Error, ErrorKind, Read, Seek, SeekFrom};

use std::u16;
use std::u8;

// [todo] reuse CPU struct
#[derive(Copy, Clone, PartialEq)]
pub struct CpuState {
	pub pc: u16,    // program counter
	pub a: u8,      // accumulator
	pub x: u8,      // index register x
	pub y: u8,      // index register y
	pub st: u8,     // processor status (flags)
	pub sp: u8,     // stack pointer
	pub cycle: u16, // number of cycles
	pub op: u8,     // op currently being run
}

impl fmt::Debug for CpuState {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		try!(write!(
			fmt,
			"<CpuState op:{:X} pc:{:X} a:{:X} x:{:X} y:{:X} st:{:b} sp:{:X}>",
			self.op, self.pc, self.a, self.x, self.y, self.st, self.sp
		));

		Ok(())
	}
}

pub struct Simulate {
	pub index: usize,
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
		Simulate {
			index: 0,
			states: vec,
		}
	}

	pub fn step(&mut self) -> CpuState {
		let state = self.states[self.index].clone();
		self.index += 1;
		state
	}
}

pub fn parse_line(line: &String) -> CpuState {
	let data: Vec<&str> = line.split_whitespace().collect();

	let mut data_offset = data.len() - 6;

	if data[data.len() - 2] == "CYC:" {
		data_offset -= 1;
	}

	let pc = u16::from_str_radix(data[0], 16).unwrap();
	let op = u8::from_str_radix(data[1], 16).unwrap();
	let a = u8::from_str_radix(&data[data_offset][2..], 16).unwrap();
	let x = u8::from_str_radix(&data[data_offset + 1][2..], 16).unwrap();
	let y = u8::from_str_radix(&data[data_offset + 2][2..], 16).unwrap();
	let st = u8::from_str_radix(&data[data_offset + 3][2..], 16).unwrap();
	let sp = u8::from_str_radix(&data[data_offset + 4][3..], 16).unwrap();
	// let cycle = u16::from_str_radix(data[data_offset + 6], 10).unwrap();

	CpuState {
		pc,
		a,
		x,
		y,
		st,
		sp,
		cycle: 0,
		op,
	}
}
