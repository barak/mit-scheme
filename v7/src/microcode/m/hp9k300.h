/* -*-C-*-
   Machine file for HP9000 series 300 (or 200)

$Id: hp9k300.h,v 1.13 1999/01/02 06:11:34 cph Exp $

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

/* In the following, it is assumed that the standard C compiler is the
   HP C compiler, and the "alternate" compiler is the GNU C compiler (GCC).
 */

/* Change this to PROC_TYPE_68000 if your machine is a series 200 or a
   model 310. 
 */
#ifndef PROC_TYPE
#define PROC_TYPE PROC_TYPE_68020
#endif /* PROC_TYPE */

/* The M4_SWITCH_MACHINE must contain -DHP if using HP C, -DGCC, if using
   GCC, and nothing special if using PCC.
 */

#if (PROC_TYPE == PROC_TYPE_68020)

#ifndef ALTERNATE_CC
#define M4_SWITCH_MACHINE -DMC68881 -DTYPE_CODE_LENGTH=6 -DHP
#else
#define M4_SWITCH_MACHINE -DMC68881 -DTYPE_CODE_LENGTH=6 -DGCC
#endif

#define AS_SWITCH_MACHINE +x -V 3

#else
#define M4_SWITCH_MACHINE
#define AS_SWITCH_MACHINE +X
#endif

#ifndef ALTERNATE_CC

/* For hp-ux version 6.5 or 7.0, uncomment this definition.  */
/* The full optimizer breaks some of the files in those versions.  */
/* #define C_OPTIMIZE_SWITCH +O1 */

/* C_SWITCH_MACHINE can take on several values:
   1. "-Aa -D_HPUX_SOURCE" is for use on HP-UX 9.0 and later; it
      specifies ANSI C with HP-UX extensions.
   2. "-Wp,-H512000" can be used on HP-UX 8.0 and later; it specifies
      traditional C.
   3. "-Wp,-H512000 -Wc,-Nt30000,-Ns3000" is for use in pre-8.0
      releases.  */
#define C_SWITCH_MACHINE -Aa -D_HPUX_SOURCE -DCOMPILER_PROCESSOR_TYPE=COMPILER_MC68020_TYPE -Dhp9000s300

#else

#define C_SWITCH_MACHINE -DCOMPILER_PROCESSOR_TYPE=COMPILER_MC68020_TYPE

#endif

#if defined(HAVE_STARBASE_GRAPHICS) && !defined(STARBASE_DEVICE_DRIVERS)
/* Add additional Starbase device drivers here. */
#define STARBASE_DEVICE_DRIVERS -ldd300h
#endif
