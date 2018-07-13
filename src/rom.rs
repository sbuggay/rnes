use std::fs;


pub fn read_rom(filename: &str) -> Vec<u8> {
	println!("loading rom {}", filename);
	let data = fs::read(filename).expect("Unable to read file");
	println!("loaded {}[{}]", filename, data.len());

	print!("{}", data[0] as char);
	print!("{}", data[1] as char);
	print!("{}", data[2] as char);
	println!("{:X}", data[3]);
	data
}
