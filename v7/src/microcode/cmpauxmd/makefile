# $Id: makefile,v 1.8 2002/11/20 19:46:16 cph Exp $
#
# Copyright (c) 2000 Massachusetts Institute of Technology
#
# This file is part of MIT Scheme.
#
# MIT Scheme is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# MIT Scheme is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MIT Scheme; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

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
