### -*- Fundamental -*-
###
###	$Id: scheme16.mak,v 1.4 1996/03/23 19:24:14 adams Exp $
###
###	Copyright (c) 1996 Massachusetts Institute of Technology
###
###	This material as developed by the Scheme project at the
###	Massachusetts Institute of Technology, Department of
###	Electrical Engineering and Computer Science.  Permission to
###	copy this software, to redistribute it, and to use it for any
###	purpose is granted, subject to the following restrictions and
###	understandings.
###
###	1. Any copy made of this software must include this copyright
###	notice in full.
###
###	2. Users of this software agree to make their best efforts (a)
###	to return to the MIT Scheme project any improvements or
###	extensions that they make, so that these may be included in
###	future releases; and (b) to inform MIT of noteworthy uses of
###	this software.
###
###	3. All materials developed as a consequence of the use of this
###	software shall duly acknowledge such use, in accordance with
###	the usual standards of acknowledging credit in academic
###	research.
###
###	4. MIT has made no warrantee or representation that the
###	operation of this software will be error-free, and MIT is
###	under no obligation to provide any services, by way of
###	maintenance, update, or otherwise.
###
###	5. In conjunction with products arising from the use of this
###	material, there shall be no use of the name of the
###	Massachusetts Institute of Technology nor of any adaptation
###	thereof in any advertising, promotional, or sales literature
###	without prior written consent from MIT in each case.
###

####	Makefile for the 16-bit component of the MIT Scheme Win32 support

all: scheme16.dll

# These have to be compiled by a 16-bit compiler (e.g. C700)
# with the Win16 SDK!

scheme16.obj: scheme16.c ntscmlib.h
	cl /c /ASw /G2 /Gsw /Ow /W2 /Zp1 scheme16.c

scheme16.dll: scheme16.obj scheme16.def
	link scheme16.obj, scheme16.dll,scheme16.map /map, \
	     w32sut16.lib mdllcew.lib libw.lib/noe/nod,scheme16.def

