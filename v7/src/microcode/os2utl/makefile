### -*- Fundamental -*-
###
### $Id: makefile,v 1.16 2002/11/20 19:46:17 cph Exp $
###
### Copyright (c) 1994-2000 Massachusetts Institute of Technology
###
### This file is part of MIT Scheme.
###
### MIT Scheme is free software; you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published
### by the Free Software Foundation; either version 2 of the License,
### or (at your option) any later version.
###
### MIT Scheme is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with MIT Scheme; if not, write to the Free Software
### Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
### 02111-1307, USA.
###

#### Makefile for Scheme under OS/2

all: scheme.exe bchschem.exe

# Uncomment exactly one of the following two lines:
#debug_mode = debug
debug_mode = optimize

# Uncomment exactly one of the following include statements to
# customize this makefile for your compiler.  All of the
# configurations require both GNU make and GNU m4.

# IBM C Set++/2 or IBM Visual Age C++:
include os2utl\makefile.vac

# Watcom C/C++:
#include os2utl\makefile.wcc

# EMX/GCC:
#include os2utl\makefile.emx

# GCC/2 (this has not been tried in a long time):
#include os2utl\makefile.gcc
