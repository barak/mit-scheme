/* -*-C-*-
   Machine file for HP9000 series 600, 700, 800.

$Id: hp9k800.h,v 1.11 1993/02/06 05:36:04 gjr Exp $

Copyright (c) 1989-1993 Massachusetts Institute of Technology

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
#define PROC_TYPE PROC_TYPE_HPPA
#endif /* PROC_TYPE */

#if defined(hpux) || defined(__hpux)

#if defined(HAVE_STARBASE_GRAPHICS) && !defined(STARBASE_DEVICE_DRIVERS)
/* Add additional Starbase device drivers here. */
#  define STARBASE_DEVICE_DRIVERS -ldd98550
#endif

/* The following is also needed under HP-UX 8.01: +Obb999 */

#ifndef ALTERNATE_CC
   /* Assume HPC */
   /* Instead of "-Aa -D_HPUX_SOURCE" you can use "-Wp,-H512000" to
      get the traditional C compiler. */
#  define C_SWITCH_MACHINE -Aa -D_HPUX_SOURCE
#  define M4_SWITCH_MACHINE -DTYPE_CODE_LENGTH=6 -DHPC
#else
   /* Assume GCC */
#  define C_SWITCH_MACHINE
#  define M4_SWITCH_MACHINE -DTYPE_CODE_LENGTH=6 -DGCC
#endif

#else /* not hpux or __hpux */

/* Utah BSD */

#ifndef ALTERNATE_CC
#  define C_SWITCH_MACHINE -Dhp9000s800
#  define M4_SWITCH_MACHINE -P "define(TYPE_CODE_LENGTH,6)" -P "define(HPC,1)"
#else
#  define M4_SWITCH_MACHINE -P "define(TYPE_CODE_LENGTH,6)" -P "define(GCC,1)"
#endif

#endif /* hpux or __hpux */
