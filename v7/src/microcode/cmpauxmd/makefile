#
# Makefile for i386 PC compiled code interface files
# for the MIT CScheme microcode.
#
# $Id: makefile,v 1.3 1994/11/28 05:22:58 cph Exp $
#

all: i386-dos.asm i386-nt.asm i386-os2.asm

i386-dos.asm: i386.m4 ../s/dos.m4
	rm -f i386-dos.asm
	../s/dos.m4 <i386.m4 >i386-dos.asm

i386-nt.asm: i386.m4 ../s/nt.m4
	rm -f i386-nt.asm
	../s/nt.m4 <i386.m4 >i386-nt.asm

# This rule will run on an OS/2 machine using GNU make and GNU m4.
i386-os2.asm: i386.m4
	rm -f i386-os2.asm
	m4 -DOS2 < i386.m4 > i386-os2.asm
