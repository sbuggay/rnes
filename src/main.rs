extern crate sdl2;

mod cpu;
mod instructions;
mod memory;
mod rom;
mod simulate;

use std::fs;
use std::time::*;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;

fn main() {
	let mut cpu = cpu::CPU::new();

	let mut file = fs::File::open("testroms/nestest.nes").expect("No file found");
	let r = rom::Rom::load(&mut file);

	let mut nestest = fs::File::open("testroms/nestest.log").expect("No file found");

	let mut simulated_cpu = simulate::Simulate::load(&mut nestest);

	println!("{:?}", r);

	// [perf]
	let mut i = 0x8000;
	for b in r.rom.into_iter() {
		cpu.mem[i] = b;
		cpu.mem[i + 0x4000] = b;
		i += 1;
	}

	cpu.pc = 0xC000;

	let sdl_context = sdl2::init().unwrap();
	let video_subsystem = sdl_context.video().unwrap();

	let window = video_subsystem
		.window("rnes", 256, 240)
		.position_centered()
		.build()
		.unwrap();

	let mut canvas = window.into_canvas().build().unwrap();

	canvas.set_draw_color(Color::RGB(0, 0, 0));
	canvas.clear();
	canvas.present();
	let mut event_pump = sdl_context.event_pump().unwrap();


	let mut last_frame = std::time::Instant::now();
	let mut frames = 0;

	let mut i = 1;

	loop {
		print!("{}: ", i);
		
		let op = cpu.get_mem(cpu.pc);
		// build cpu state
		let machine_state = simulate::CpuState {
			pc: cpu.pc,
			a: cpu.a,
			x: cpu.x,
			y: cpu.y,
			st: cpu.st,
			sp: cpu.sp,
			cycle: 0,
			op,
		};
		cpu.step();
		// get simulated from test rom
		let test_state = simulated_cpu.step();

		if machine_state != test_state {
			println!("State mismatch! States do not match at log:{}", simulated_cpu.index - 1);
			println!("Machine\t{:?}\nTest\t{:?}\n", machine_state, test_state);
			break;
		}
		//compare
		
		i += 1;
	}

	// 'running: loop {
	// 	// cpu step
		
	// 	// ppu step

	// 	// nmi

	// 	// irq

	// 	// apu

	// 	for event in event_pump.poll_iter() {
	// 		match event {
	// 			Event::Quit { .. }
	// 			| Event::KeyDown {
	// 				keycode: Some(Keycode::Escape),
	// 				..
	// 			} => break 'running,
	// 			_ => {}
	// 		}
	// 	}

	// 	// println!("{} {:?}", frames, last_frame.elapsed().subsec_millis());
	// 	last_frame = std::time::Instant::now();
	// 	frames += 1;
	// }
	
}
