/* -*-C-*-

$Id: nttrap.h,v 1.5 1999/01/02 06:11:34 cph Exp $

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

#ifndef SCM_NTTRAP_H
#define SCM_NTTRAP_H

enum trap_state
{
  trap_state_trapped,
  trap_state_exit,
  trap_state_suspend,
  trap_state_query,
  trap_state_recover,
  trap_state_exitting_soft,
  trap_state_exitting_hard
};

extern enum trap_state EXFUN (OS_set_trap_state, (enum trap_state state));

#endif /* SCM_NTTRAP_H */
