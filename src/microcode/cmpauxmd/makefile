# $Id: makefile,v 1.7 2000/12/05 21:23:50 cph Exp $
#
# Copyright (c) 2000 Massachusetts Institute of Technology
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# Makefile for MIT Scheme's i386 compiled-code interface files.

EXPANSIONS = i386-nt.asm i386-ntw.asm

all: $(EXPANSIONS)

# Expand for Win32 using Microsoft compiler.
i386-nt.asm: i386.m4
	rm -f $@
	./m4-dos -P "WIN32,1" < i386.m4 > i386-nt.asm

# Expand for Win32 using Watcom compiler.
i386-ntw.asm: i386.m4
	rm -f $@
	./m4-dos -P "WIN32,1" -P "WCC386R,1" < i386.m4 > i386-ntw.asm

maintainer-clean:
	rm -f $(EXPANSIONS)

.PHONY: all maintainer-clean
