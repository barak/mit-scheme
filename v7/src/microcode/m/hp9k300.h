/* -*-C-*-
   Machine file for HP9000 series 300 (or 200)

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/m/Attic/hp9k300.h,v 1.3 1989/07/27 06:02:41 cph Exp $

Copyright (c) 1989 Massachusetts Institute of Technology

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

/* Change this to PROC_TYPE_68000 if your machine is a series 200 or a
   model 310. */
#define PROC_TYPE PROC_TYPE_68020

#if (PROC_TYPE == PROC_TYPE_68020)
#define M4_SWITCH_MACHINE -DMC68881
#define AS_SWITCH_MACHINE +x -V 3
#else
#define M4_SWITCH_MACHINE
#define AS_SWITCH_MACHINE +X
#endif

#ifndef __GNUC__

/* For hp-ux version 6.2 and earlier, comment out this definition. */
#define C_OPTIMIZE_SWITCH +O1

#define C_SWITCH_MACHINE -Wp,-H60000 -Wc,-Nt30000

#endif

#if defined(HAVE_STARBASE_GRAPHICS) && !defined(STARBASE_DEVICE_DRIVERS)
/* Add additional Starbase device drivers here.
   If HAVE_X_WINDOWS is defined, -lddsox11 is
   automatically included, so don't add it here. */
#define STARBASE_DEVICE_DRIVERS -ldd300h
#endif

#if (defined (__GNUC__)) && (! (defined (__HPUX_ASM__)))
#define AS_RULE								\
.s.o: ; @ECHO "#** Generating" $@ because of $?				@@\
	$(AS) AS_SWITCH_SYSTEM AS_SWITCH_MACHINE -o $*.ohp $*.s		@@\
	hpxt $*.ohp $*.o						@@\
	rm -f $*.ohp
#endif
