/* -*-C-*-
   System file for HP-UX

$Id: hpux.h,v 1.10 1993/03/22 16:04:07 cph Exp $

Copyright (c) 1989-93 Massachusetts Institute of Technology

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

#define HAVE_TERMINFO

/* Define HAVE_STARBASE_GRAPHICS if you want Starbase graphics support. */
/* #define HAVE_STARBASE_GRAPHICS */

/* No special libraries are needed for debugging. */
#define LIB_DEBUG

#ifndef INSTALL_PROGRAM
#define INSTALL_PROGRAM cp
#endif

/* For releases of hp-UX prior to 9.0 change the following lines to
   read X11R4 instead of X11R5.  */
#define C_SWITCH_SYSTEM -D_HPUX -I/usr/include/X11R5
#define LD_SWITCH_SYSTEM -L /usr/lib/X11R5

#define LIB_DYNAMIC_LOAD /usr/lib/libdld.sl
