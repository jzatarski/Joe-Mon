# Joe-Mon
A machine language monitor for the Beckman DU600 photospectrometer and other MC68K architecture platforms.

Written by Joseph Zatarski

Copyright 2014-2017

Version 2

Joe-Mon is a machine language monitor written entirely in MC68000/CPU32 assembly for the Beckman DU600 photospectrometer. Joe-mon incorporates the following features to aid in developing and running software on the host platform:
- s-record download
- machine language disassembly (WIP)
- boot simulation
- debugging capabilities (WIP)

Joe-Mon commands are designed to be added easily by using a common API for calling the function associated with a given command. Each command has an entry in a table with a function address and an address to the command string (for command recognition). Arguments are passed to the subroutine for the command as an argc (passed in d7) and \*\*argv (passed in a6). Argc (d7) must not be modified or must be restored before return since the command parser uses this to deallocate memory.

### usage

The monitor currently supports the following commands:

load - downloads a Motorola S-record into RAM.
abbreviated as l

ver - reports the version and some other info about the monitor.
abbreviated as v

boot \<addr\> - simulates the boot process on an image loaded into RAM
This mimics the reset process of the CPU32, but looks at RAM for the vector table instead of address 0. First, the trace bits are cleared. Second, the Vector Base Register (VBR) is set to point to the address of the new vector table in RAM. Then the stack is initialized to the address contained in the first entry of the new vector table. Lastly, the monitor jumps to the address in the second vector of the vector table. boot takes the address to boot as an argument in hexadecimal.
abbreviated as b

jump \<addr\> - jumps to the address specified as a hexadecimal argument.
abbreviated as j

regs - displays the values of registers
abbreviated as r

disassemble - disassembles code. WIP. Documentation to be updated upon completion of feature.
abbreviated as d

Additionally, sending a serial break to the console port is set up to reboot the monitor. This is the same as running the boot command, but without modifying the VBR from it's current value.

### Build instructions
Joe-Mon is written to be assembled by the vasm assembler. vasm supports multiple syntaxes and CPUs. The motorola syntax module along with the MC68K CPU module should be used to compile Joe-Mon. joemon.asm is the file to be assembled. Make sure to select the proper CPU type using the -m option with vasm. For example:

```
vasmm68k_mot -Fsrec -mcpu32 -o joemon.s -L joemon.lst joemon.asm
```

This will compile Joe-Mon to the s-record output file joemon.s with listing output in joemon.lst. Additionally, a version of Joe-Mon can be built to run in RAM by defining ram_version:

```
vasmm68k_mot -Fsrec -mcpu32 -o joemon-ram.s -L joemon-ram.lst -Dram_version joemon.asm
```

### Contributing

see CONTRIBUTING.md for information about contributing.

### Licensing

This software is licensed under the GPL. See license.txt for more details.

Note that copyright years in these files may be indicated as a range. For example, 2014-2017 indicates copyright for ALL of the years in the range: 2014,2015,2016,2017
