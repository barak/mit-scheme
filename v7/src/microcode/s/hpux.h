/* -*-C-*-
   System file for HP-UX

$Id: hpux.h,v 1.12 1999/01/02 06:11:34 cph Exp $

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
#define LD_SWITCH_SYSTEM -L /usr/lib/X11R5 -Wl,-E

/* These definitions, and the -Wl,-E in LD_SWITCH_SYSTEM, configure
   the microcode to support dynamic loading. */
#define SOURCES_SYSTEM pruxdld.c
#define OBJECTS_SYSTEM pruxdld.o
#define LIBS_SYSTEM -ldld
