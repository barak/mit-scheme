#
# Makefile for MS DOS and Windows NT compiled code interface files
# for the MIT CScheme microcode.
#
# $Id: makefile,v 1.1 1993/06/24 08:07:26 gjr Exp $
#

i386-dos.asm: i386.m4 ../s/dos.m4
	rm -f i386-dos.asm
	../s/dos.m4 <i386.m4 >i386-dos.asm


i386-nt.asm: i386.m4 ../s/nt.m4
	rm -f i386-nt.asm
	../s/nt.m4 <i386.m4 >i386-nt.asm


all: i386-dos.asm i386-nt.asm


