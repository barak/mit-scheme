/* -*-C-*-
   Machine file for HP9000 series 600, 700, 800.

$Id: hp9k800.h,v 1.14 1999/01/02 06:11:34 cph Exp $

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
/* C_SWITCH_MACHINE can take on several values:
   1. "-Ae" is for use on HP-UX 9.0 and later; it specifies ANSI C
      with HP-UX extensions.
   2. "-Aa -D_HPUX_SOURCE" is similar but for earlier HP-UX releases.
   3. "-Wp,-H512000" can be used in pre-9.0 releases to get
      traditional C (it might work in 9.0 also but hasn't been
      tested).  */
#  define C_SWITCH_MACHINE -Ae
#  define M4_SWITCH_MACHINE -DTYPE_CODE_LENGTH=6 -DHPC
/* "-Wl,+s" tells the linker to allow the environment variable
   SHLIB_PATH to be used to define directories to search for shared
   libraries when the microcode is executed. */
#  define LD_SWITCH_MACHINE -Wl,+s
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
