/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/ostop.h,v 1.3 1991/01/24 04:34:30 cph Exp $

Copyright (c) 1990-1 Massachusetts Institute of Technology

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
