Joe-Mon
A machine language monitor for the Beckman DU600 photospectrometer and other
68K architecture platforms.
Written by Joseph Zatarski
Copyright 2014-2016
Version 2

The monitor currently supports 4 commands:

load - downloads a Motorola S-record into RAM.

ver - reports the version and some other info about the monitor.

boot - simulates the boot process on an image loaded into RAM
This mimics the reset process of the CPU32, but looks at RAM for the vector
table instead of address 0. First, the trace bits are cleared. Second, the
Vector Base Register (VBR) is set to point to the address of the new vector
table in RAM. Then the stack is initialized to the address contained in the
first entry in the new vector table. Lastly, the monitor jumps to the address
in the second vector in the vector table. boot takes the address to boot as an
argument in hexadecimal.

jump - jumps to the address specified as a hexadecimal argument.
