#
# Makefile for i386 PC compiled code interface files
# for the MIT Scheme microcode.
#
# $Id: makefile,v 1.6 1995/10/24 06:32:35 cph Exp $
#

all : i386-dos.asm i386-nt.asm i386-ntw.asm

# Expand for DOS.
i386-dos.asm : i386.m4 ../s/dos.m4
	rm -f $@
	../s/dos.m4 < $< > $@

# Expand for Win32 using Microsoft compiler.
i386-nt.asm : i386.m4 ../s/nt.m4
	rm -f $@
	../s/nt.m4 < $< > $@

# Expand for Win32 using Watcom compiler.
i386-ntw.asm : i386.m4 ../s/nt.m4
	rm -f $@
	../s/nt.m4 -P "define(WCC386R,1)" < $< > $@
