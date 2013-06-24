# -*- Fundamental -*-
#
# $Id: scheme16.mak,v 1.5 2003/02/14 18:48:12 cph Exp $
#
# Copyright 1996 Massachusetts Institute of Technology
#
# This file is part of MIT/GNU Scheme.
#
# MIT/GNU Scheme is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# MIT/GNU Scheme is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MIT/GNU Scheme; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# Makefile for the 16-bit component of the MIT/GNU Scheme Win32 support

all: scheme16.dll

# These have to be compiled by a 16-bit compiler (e.g. C700)
# with the Win16 SDK!

scheme16.obj: scheme16.c ntscmlib.h
	cl /c /ASw /G2 /Gsw /Ow /W2 /Zp1 scheme16.c

scheme16.dll: scheme16.obj scheme16.def
	link scheme16.obj, scheme16.dll,scheme16.map /map, \
	     w32sut16.lib mdllcew.lib libw.lib/noe/nod,scheme16.def

