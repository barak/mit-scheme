/* -*-C-*-
   System file for Ultrix

$Id: ultrix.h,v 1.16 1999/01/02 06:11:34 cph Exp $

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

/* Why? -- ask JMiller */
#undef	ultrix

/* This is in mips.h for some reason. */
/* #define LIB_DEBUG */

#define _ULTRIX
#define C_SWITCH_SYSTEM -D_ULTRIX -YPOSIX
#define LD_SWITCH_SYSTEM -YPOSIX

#define LIBS_TERMCAP -ltermcap

/* Fix random bug in Ultrix "libX11.a"; I quote:

   "Linker library processing works this way.  If the library hasn't
   had ranlib(1) run on it, the Linker searches the library
   sequentially exactly once, loading every module that resolves a
   reference on the undefined symbol list.  If the library has been
   processed by ranlib(1), the Linker searches the index that ranlib
   built for unresolved external references and loads any modules that
   resolve those references.  It repeatedly searches the index until
   either it has no more unresolved references or until a search
   results in no more modules being loaded.  The Linker then processes
   the next item on the command line.

   This explains why -lc -lX11 -lc works but -lX11 -lc doesn't.  The
   emacs code itself references calloc or cfree somewhere.  If the
   Linker encounters libc.a before libX11.a, it will use libc.a's
   version of calloc/cfree.  By the time it encounters libX11.a,
   calloc and cfree are not on the undefined symbol list and thus
   there is no reason to load XvmsAlloc.o.  You need the second
   reference to libc.a on the command line to resolve any references
   from modules in libX11.a to modules in libc.a not already loaded.
   If you put libX11.a first, calloc and cfree references cause
   XvmsAlloc.o to be loaded.  This module also defines free, malloc,
   and realloc, causing the multiple symbol definitions message from
   ld(1)." */

#define LIBX11_SYSTEM -lc

#define ALTERNATE_M4 s/ultrix.m4
#ifndef INSTALL_PROGRAM
#define INSTALL_PROGRAM cp
#endif
