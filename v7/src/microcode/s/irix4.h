/* -*-C-*-
   System file for SGI's IRIS version 4.x

$Id: irix4.h,v 1.5 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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

#ifndef INSTALL_PROGRAM
#define INSTALL_PROGRAM cp
#endif

#define C_SWITCH_SYSTEM -D_IRIX4
#define ALTERNATE_M4 s/ultrix.m4
#define ALTERNATE_CC cc -cckr -float -Olimit 2000
#define LIBS_SYSTEM -lmalloc
