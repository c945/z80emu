CC      = gcc
CFLAGS  = -O2 -DLSB_FIRST -DDEBUG -I/usr/include/ncurses
#CFLAGS  = -g -DLSB_FIRST -DDEBUG

.SUFFIXES:	.asm .hex .bin

ALL:	z80emu test1.bin pcwpatb.bin dummy

Z80.o:	Z80.c Z80.h Codes.h CodesED.h CodesCB.h CodesXX.h Tables.h CodesXCB.h

Debug.o:	Debug.c Z80.h

dummy:	dummy.o Z80.o dasm.o
	$(CC) -o $@ dummy.o Z80.o dasm.o -lcurses

z80emu:	z80emu.o Z80.o dasm.o
	$(CC) -o $@ z80emu.o Z80.o dasm.o -lcurses

.asm.p:
	asl -L $<
.p.bin:
	p2bin -r '0-$$ffff' $*
