/* -*-C-*-
   Machine file for MIPS computers.

$Id: mips.h,v 1.8 1993/06/28 02:29:40 cph Exp $

Copyright (c) 1989-1992 Massachusetts Institute of Technology

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

#ifndef PROC_TYPE
#define PROC_TYPE PROC_TYPE_MIPS
#endif /* PROC_TYPE */

/* The MIPS compiler/linker doesn't use -lg.  */
#define LIB_DEBUG

#ifndef ALTERNATE_CC
/* The "-std" switch says to turn on ANSI features and compatible
   extensions.  The "-Olimit" switch is needed because some procedures
   are larger than the built-in optimization limit.  */
#define C_SWITCH_MACHINE -std -Olimit 2000
#else
#define C_SWITCH_MACHINE
#endif
