/* -*-C-*-

$Id: ospty.h,v 1.5 2002/11/20 19:46:12 cph Exp $

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

#ifndef SCM_OSPTY_H
#define SCM_OSPTY_H

#include "os.h"

extern CONST char * EXFUN
  (OS_open_pty_master, (Tchannel * master_fd, CONST char ** master_fname));
extern void EXFUN (OS_pty_master_send_signal, (Tchannel channel, int sig));
extern void EXFUN (OS_pty_master_kill, (Tchannel channel));
extern void EXFUN (OS_pty_master_stop, (Tchannel channel));
extern void EXFUN (OS_pty_master_continue, (Tchannel channel));
extern void EXFUN (OS_pty_master_interrupt, (Tchannel channel));
extern void EXFUN (OS_pty_master_quit, (Tchannel channel));
extern void EXFUN (OS_pty_master_hangup, (Tchannel channel));

#endif /* SCM_OSPTY_H */
