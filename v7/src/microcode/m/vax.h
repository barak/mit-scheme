/* -*-C-*-
   Machine file for DEC Vax computers

$Id: vax.h,v 1.6 1999/01/02 06:11:34 cph Exp $

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

/* This causes problems when generating xmakefile. */

#ifdef vax
#undef vax
#endif

#ifndef PROC_TYPE
#define PROC_TYPE PROC_TYPE_VAX
#endif /* PROC_TYPE */

/* The M4_SWITCH_MACHINE must contain -P "define(GCC,1)", if using GCC,
   -P "define(VMS,1)" if preparing the files for VMS Vax C,
    and nothing special if using PCC.
 */

#ifndef ALTERNATE_CC
#define M4_SWITCH_MACHINE -P "define(TYPE_CODE_LENGTH,6)"
#else
#define M4_SWITCH_MACHINE -P "define(TYPE_CODE_LENGTH,6)" -P "define(GCC,1)"
#endif

#define C_SWITCH_MACHINE
