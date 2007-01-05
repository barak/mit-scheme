/* -*-C-*-

$Id: locks.h,v 9.29 2007/01/05 15:33:06 cph Exp $

Copyright (c) 1987, 1988, 1989, 1999 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* Contains everything needed to lock and unlock parts of
   the heap, pure/constant space and the like.
   It also contains intercommunication stuff as well. */

typedef long *Lock_Handle;		/* Address of lock word */
#define CONTENTION_DELAY	10	/* For "slow" locks, back off */
#define Lock_Cell(Cell)		NULL	/* Start lock */
#define Unlock_Cell(Cell)		/* End lock */
#define Initialize_Heap_Locks()		/* Clear at start up */
#define Do_Store_No_Lock(To, F)	*(To) = F
#define Sleep(How_Long)		{ }	/* Delay for locks, etc. */

#define LOCK_FIRST(cell1, cell2)	(cell1 < cell2)
