/* -*-C-*-
   System file for NeXT running Mach

$Id: nextos.h,v 1.7 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

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

#define C_SWITCH_SYSTEM -bsd -fwritable-strings -D_NEXTOS
#define LIBS_TERMCAP -ltermcap
/* -lg not needed if -g is given. */
#define LIB_DEBUG
