/* -*-C-*-
   Machine file for MIPS computers.

$Id: mips.h,v 1.10 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1989-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef PROC_TYPE
#define PROC_TYPE PROC_TYPE_MIPS
#endif /* PROC_TYPE */

/* The MIPS compiler/linker doesn't use -lg.  */
#define LIB_DEBUG

#ifndef ALTERNATE_CC
/* The "-std" switch says to turn on ANSI features and compatible
   extensions.  The "-Olimit" switch is needed because some procedures
   are larger than the built-in optimization limit.  */
#ifdef _ULTRIX
/* Ultrix doesn't support -std */
#define C_SWITCH_MACHINE -Olimit 2000
#else
#define C_SWITCH_MACHINE -std -Olimit 2000
#endif
#else
#define C_SWITCH_MACHINE
#endif
