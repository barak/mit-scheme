/* -*-C-*-
   System file for Ultrix

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/s/Attic/ultrix.h,v 1.11 1990/10/16 20:57:25 cph Exp $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* Why? -- ask JMiller */
#undef	ultrix

/* This is in mips.h for some reason. */
/* #define LIB_DEBUG */

#define C_SWITCH_SYSTEM -D_ULTRIX -YPOSIX

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
