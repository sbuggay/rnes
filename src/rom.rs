use std::cmp::max;
use std::fmt;
use std::io::{Error, ErrorKind, Read, Seek, SeekFrom};

pub const ROM_BANK_SIZE: usize = 16 * 1024;
pub const VROM_BANK_SIZE: usize = 8 * 1024;

pub struct Rom {
	pub mapper: u8,
	pub rom: Vec<u8>,
	pub video_rom: Vec<u8>,
	pub save_ram_length: u32,
	pub mirroring: Mirroring,
}

impl fmt::Debug for Rom {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		try!(write!(
			fmt,
			"<Rom mapper={}, rom={}k, video_rom={}k, ram={}k, mirroring={:?}>",
			self.mapper,
			self.rom.len() / 1024,
			self.video_rom.len() / 1024,
			self.save_ram_length / 1024,
			self.mirroring
		));

		Ok(())
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Mirroring {
	Horizontal,
	Vertical,
	FourScreen,
}

fn read_u8<T: Read + Seek>(fp: &mut T) -> u8 {
	let mut val = [0];
	fp.read_exact(&mut val);
	return val[0];
}

impl Rom {
	pub fn load<T: Read + Seek>(fp: &mut T) -> Self {
		let mut magic = [0; 4];
		fp.read_exact(&mut magic);

		if magic[0] != 'N' as u8 && magic[1] != 'E' as u8 && magic[2] != 'S' as u8 && magic[3] != 0x1a {
			println!("magic is not correct");
		}

		let rom_bank_count = read_u8(fp) as usize;
		let video_rom_bank_count = read_u8(fp) as usize;
		let flags_1 = read_u8(fp);
		let flags_2 = read_u8(fp);

		// For compatibility with older INES files we assume there must be always one RAM bank.
		let save_ram_length = max(1, read_u8(fp) as u32) * 8 * 1024;

		// Skip padding.
		fp.seek(SeekFrom::Current(7));

		let mirroring = {
			if flags_1 & 0b1000 != 0 {
				Mirroring::FourScreen
			} else if flags_1 & 0b1 == 0 {
				Mirroring::Horizontal
			} else {
				Mirroring::Vertical
			}
		};

		let has_trainer = flags_1 & 0b100 != 0;
		let mapper = (flags_2 & 0xF0) | ((flags_1 & 0xF0) >> 4);

		let mut rom = Vec::<u8>::with_capacity(rom_bank_count * ROM_BANK_SIZE);
		let mut video_rom = Vec::<u8>::with_capacity(video_rom_bank_count * VROM_BANK_SIZE);

		unsafe {
			rom.set_len(rom_bank_count * ROM_BANK_SIZE);
			video_rom.set_len(video_rom_bank_count * VROM_BANK_SIZE);
		}

		if has_trainer {
			fp.seek(SeekFrom::Current(512)); // Skip trainer.
		}

		fp.read_exact(&mut rom[..]);
		fp.read_exact(&mut video_rom[..]);

		Rom {
			mapper: mapper,
			rom: rom,
			video_rom: video_rom,
			save_ram_length: save_ram_length,
			mirroring: mirroring,
		}
	}

	pub fn rom_bank_count(&self) -> usize {
		self.rom.len() / ROM_BANK_SIZE
	}

	pub fn video_rom_bank_count(&self) -> usize {
		self.video_rom.len() / VROM_BANK_SIZE
	}
}
