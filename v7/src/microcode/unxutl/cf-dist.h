/* -*-C-*-

$Id: cf-dist.h,v 1.25 2000/10/16 18:32:09 cph Exp $

Copyright (c) 1989-2000 Massachusetts Institute of Technology

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

/* Change the value of this macro to 1 to use the C back end instead of
   a native back end (if one exists). 
*/

#define BACK_END_TYPE 2

#define PROC_TYPE_UNKNOWN	0
#define PROC_TYPE_68000		1
#define PROC_TYPE_68020		2
#define PROC_TYPE_HPPA		3	/* HP Precision Architecture */
#define PROC_TYPE_VAX		4
#define PROC_TYPE_MIPS		5
#define PROC_TYPE_NS32K		6
#define PROC_TYPE_HCX		7	/* Harris HCX */
#define PROC_TYPE_IBM032	8	/* IBM RT */
#define PROC_TYPE_SPARC		9
#define PROC_TYPE_I386		10
#define PROC_TYPE_ALPHA		11
#define PROC_TYPE_POWER		12	/* IBM RS6000 and PowerPC */
#define PROC_TYPE_LIARC		13	/* Scheme compiled to C */

#if (BACK_END_TYPE == 0)
#define PROC_TYPE PROC_TYPE_UNKNOWN
#endif

#if (BACK_END_TYPE == 1)
#define PROC_TYPE PROC_TYPE_LIARC
#endif

/* Define this macro to use a non-standard compiler.
   It must be defined before including the m/ and s/ files because
   they may be conditionalized on it. */
/* #define ALTERNATE_CC gcc */

/* Define this macro to use a non-standard assembler. */
/* #define ALTERNATE_AS gashp */

/* Define this macro to use a non-standard install program. */
/* #define INSTALL_PROGRAM cp -p */

#include "s.h"
#include "m.h"

#ifndef PROC_TYPE
#define PROC_TYPE PROC_TYPE_UNKNOWN
#endif

/* The following two switches are mutually exclusive for most C compilers.
   An exception is the GNU C compiler. */

/* If defined, this prevents the C compiler from running its optimizer. */
/* #define SUPPRESS_C_OPTIMIZER */

#ifndef __GNUC__
/* If defined, this prevents the C compiler from
   generating debugging information. */
#define SUPPRESS_C_DEBUGGING
#endif

/* Some compilation options:
   -DDISABLE_HISTORY		turns off history recording mechanism
   -DCOMPILE_STEPPER		turns on support for the stepper    
 */
#define C_SWITCH_FEATURES -DCOMPILE_STEPPER

/* Define HAVE_X_WINDOWS if you want to use the X window system.  */
#define HAVE_X_WINDOWS

#if defined(__hpux) || defined(hpux)
/* If this is an HP-UX system, Starbase graphics support is available.
   If you don't want this support, comment out the following line.  */
#define HAVE_STARBASE_GRAPHICS
#endif

#ifdef HAVE_STARBASE_GRAPHICS
/* You may need to tweak one of the following device driver
   definitions to be correct for your machine.  */

#if defined(__hp9000s700) || defined(hp9000s700)

/* This default covers the standard grayscale and 8-bit color graphics
   for series 700 workstations.  If you have 24-bit graphics you will
   need to change this.  */
#define STARBASE_DEVICE_DRIVERS -lddgcrx

#else
#if defined(__hp9000s300) || defined(hp9000s300)

/* This default covers the standard monochrome and 8-bit (or less)
   color displays for 300 and 400 series workstations.  It also
   supports the 98556 graphics accelerator.  */
#define STARBASE_DEVICE_DRIVERS -ldd300h -ldd98556

#endif /* __hp9000s300 */
#endif /* __hp9000s700 */
#endif /* HAVE_STARBASE_GRAPHICS */
