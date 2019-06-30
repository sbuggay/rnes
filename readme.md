# rnes

Currently, this fully passes the [nestest.nes](http://www.qmtpro.com/~nes/misc/nestest.txt) [emulator test](https://wiki.nesdev.com/w/index.php/Emulator_tests). This includes all the page crossing quirks as well as "invalid"/unofficial opcodes. As for right now I'm prioritizing correctness over everything else.

PPU and APU are currently a work in progress. Uses SDL2 for window handling, input, drawing, and audio.

build
```
cargo build
```

run
```
cargo run
```
