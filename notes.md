```
CPU memory map:
    |                 |    MOST     |      |
    |      RANGE      | SIGNIFICANT | SIZE |              CONTENTS
    |                 |   NIBBLE    |      |
    ----------------------------------------------------------------------------
    | 0x0000...0x07FF | 0000...0000 |  2kb | RAM
    | 0x0800...0x1FFF | 0000...0001 |  6kb | mirrors of RAM
    | 0x2000...0x2007 | 0010...0010 |   8b | I/O registers (PPU, 8 registers)
    | 0x2008...0x3FFF | 0010...0011 |      | mirrors of I/O registers (PPU)
    | 0x4000...0x401F | 0100...0100 |  32b | I/O registers (APU, DMA, Joypads)
    | 0x4020...0x5FFF | 0100...0101 |< 8kb | expansion ROM
    | 0x6000...0x7FFF | 0110...0111 |  8kb | save RAM
    | 0x8000...0xBFFF | 1000...1011 | 16kb | PRG-ROM lower bank
    | 0xC000...0xFFFF | 1100...1111 | 16kb | PRG-ROM upper bank
Whole 0x4020...0xFFFF is mapped to the cartridge.
```