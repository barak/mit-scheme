/* -*-C-*-
   System file for Linux

$Id: linux.h,v 1.7 1996/03/04 20:38:54 cph Exp $

Copyright (c) 1995-96 Massachusetts Institute of Technology

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

#define LIBX11_MACHINE -L/usr/X11/lib

#define LIB_DEBUG

#define ALTERNATE_M4 s/ultrix.m4

/* The following change is necessary if ELF binaries are used.
   Unfortunately, it is insufficient, because the Linux ELF
   implementation also moves the text and data segments to 0x08000000,
   which means that all of the pointer manipulation code must be
   changed.  This is normal for some architectures, e.g. the MIPS and
   HPPA, but it make the binaries for Linux ELF different for the
   binaries for all other i386 machines.

   Since I don't currently know any way to adjust the mapping of the
   segments, if ELF is in use, I'll just force the switch that causes
   GCC to generate a.out format instead of ELF.  This is a temporary
   patch, because one of these days a.out won't be supported, but
   hopefully by then we'll know how to fix this correctly.  */

#ifdef __ELF__
#define M4_SWITCH_SYSTEM -P "define(LINUX_ELF,1)"
#define LD_SWITCH_SYSTEM -T s/linuxelf.lds
#define LIBS_TERMCAP -lncurses
#else
#define M4_SWITCH_SYSTEM
#define LD_SWITCH_SYSTEM
#define LIBS_TERMCAP -ltermcap
#endif
