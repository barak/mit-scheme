#
# Makefile for i386 PC compiled code interface files
# for the MIT CScheme microcode.
#
# $Id: makefile,v 1.4 1995/10/15 00:28:51 cph Exp $
#

all: i386-dos.asm i386-nt.asm

i386-dos.asm: i386.m4 ../s/dos.m4
	rm -f i386-dos.asm
	../s/dos.m4 <i386.m4 >i386-dos.asm

i386-nt.asm: i386.m4 ../s/nt.m4
	rm -f i386-nt.asm
	../s/nt.m4 <i386.m4 >i386-nt.asm
