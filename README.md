# asmboy4GB
This gameboy program written in assembler enables you to write your own gameboy game in assembler **on the gameboy**.

> **_NOTE:_**  The code is experimental and is not meant for production use. Whatever that would mean for a software for writing gameboy games on a gameboy

## Compile
[Install RGBDS](https://rgbds.gbdev.io/install/) and put the binarys into your PATH. Then execute:
```
rgbasm -L -o asmboy4gb.o asmboy4gb.asm
rgblink -o asmboy4gb.gb asmboy4gb.o
rgblink asmboy4gb.o -n asmboy4gb.sym -m asmboy4gb.map
rgbfix -v -p 0xFF --ram-size 02 --mbc MBC1+RAM+BATTERY asmboy4gb.gb
```
The generated asmboy4gb.gb should be startable with any gameboy emulator

## Keys:

| Key      | Action |
| -------- | ----------- |
| SELECT      | Open the option menu to save a game, load the game or load one of the samples       |
| START   | Start the program       |
| RIGHT   | Add instruction or label above the currently selected instruction      |
| LEFT   | Delete currently selected instruction      |
| DOWN   | scroll down, in sub-menu: select next instruction, register, nibble (half-byte), condition etc.   |
| UP   | scroll up (when top of screen reached jumpt to start), In sub-menu: select previousinstruction, register, nibble (half-byte), condition etc.   |
| A   | in sub-menu: select the item and continue|

## Usage Video:

The provided savegame asmboy4gb.sav is the game programmed in the video.
To start the game load the game in an emulator like BGB.
If the sav file is placed in the same folder it will be automatically picked up.
In the emulator click SELECT and then `Load saved game`. After that press START and then press DOWN to select YES and confirm with A. Now the game should start

On the left top the points are printed. On the right top the number of misses left until game over is printed.


## Cheatsheet:
The cheatsheet can be helpful while programming on the go as it has most of the important addresses in it and fits good on a DIN A5.
## Helpful Ressources:

[Z80 opcodes encoding](http://www.z80.info/decoding.htm)

[Table with all gameboy instructions by instruction machine code](https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html)

[Memory Map/Hardware description of the Gameboy](https://gbdev.io/pandocs/)

[RGBDS Assembler instructions](https://rgbds.gbdev.io/docs/v0.6.0/gbz80.7/)

[Hello world in assembler](https://eldred.fr/gb-asm-tutorial/part1/hello_world.html)