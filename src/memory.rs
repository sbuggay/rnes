pub const ADDR_LO_BARE: u16 = 0x0000;
pub const ADDR_HI_BARE: u16 = 0xFFFF;

pub const MEMORY_SIZE: usize = (ADDR_HI_BARE - ADDR_LO_BARE) as usize + 1;

