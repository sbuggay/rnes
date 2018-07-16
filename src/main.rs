extern crate sdl2;

mod cpu;
mod instructions;
mod memory;
mod rom;

use std::fs;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use std::time::Duration;

fn main() {
	let mut cpu = cpu::CPU::new();

	let mut file = fs::File::open("testroms/nestest.nes").expect("No file found");
	let r = rom::Rom::load(&mut file);

	println!("{:?}", r);

	r.dump();

	// [perf]
	let mut i = 0x8000;
	for b in r.rom.into_iter() {
		cpu.mem[i] = b;
		cpu.mem[i + 0x4000] = b;
		i += 1;
	}

	cpu.pc = 0x8000;

	cpu.dump();

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

	'running: loop {
		for event in event_pump.poll_iter() {
			match event {
				Event::Quit { .. }
				| Event::KeyDown {
					keycode: Some(Keycode::Escape),
					..
				} => break 'running,
				_ => {}
			}
		}
		::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));

		cpu.emulate();
	}
}
