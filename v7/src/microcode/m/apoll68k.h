/* -*-C-*-
   Machine file for Apollo 68k

$Id: apoll68k.h,v 1.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1989-1999 Massachusetts Institute of Technology
Copyright (c) 1991 Michael K. Gschwind

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
#define PROC_TYPE PROC_TYPE_UNKNOWN
#endif /* PROC_TYPE */

#ifndef ALTERNATE_CC

#define C_SWITCH_MACHINE -Dapollo -A nansi
#define LD_SWITCH_MACHINE -L/usr/lib/X11

#else /* ALTERNATE_CC */

#define C_SWITCH_MACHINE -Dapollo
#define LD_SWITCH_MACHINE -L/usr/lib/X11

#endif
