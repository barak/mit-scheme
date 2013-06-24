/* -*-C-*-

$Id: ostop.h,v 1.5 1999/01/02 06:11:34 cph Exp $

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

#ifndef SCM_OSTOP_H
#define SCM_OSTOP_H

#include "os.h"

extern int EXFUN (OS_under_emacs_p, (void));
extern void EXFUN (OS_initialize, (void));
extern void EXFUN (OS_reset, (void));
extern void EXFUN (OS_quit, (int code, int abnormal_p));
extern void EXFUN (OS_restartable_exit, (void));
extern void EXFUN (OS_save_external_state, (void));
extern void EXFUN (OS_save_internal_state, (void));
extern void EXFUN (OS_restore_internal_state, (void));
extern void EXFUN (OS_restore_external_state, (void));
extern CONST char * EXFUN (OS_error_code_to_message, (unsigned int code));

#endif /* SCM_OSTOP_H */
