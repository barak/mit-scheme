/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/osproc.h,v 1.1 1990/06/20 19:36:30 cph Rel $

Copyright (c) 1990 Massachusetts Institute of Technology

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

#ifndef SCM_OSPROC_H
#define SCM_OSPROC_H

#include "os.h"

typedef unsigned int Tprocess;

enum process_status
{
  process_status_free,		/* unused process table entry */
  process_status_allocated,	/* being started */
  process_status_running,	/* running */
  process_status_stopped,	/* stopped but continuable */
  process_status_exited,	/* terminated by calling _exit() */
  process_status_signalled	/* terminated by being signalled */
};

enum process_ctty_type
{
  ctty_type_none,		/* no controlling terminal */
  ctty_type_inherited,		/* ctty is Scheme's ctty */
  ctty_type_pipe,		/* ctty is a pipe */
  ctty_type_pty			/* ctty is a PTY */
};

extern size_t OS_process_table_size;
#define NO_PROCESS OS_process_table_size
extern Tprocess EXFUN
  (OS_make_subprocess,
   (CONST char * filename,
    CONST char ** argv,
    char ** env,
    enum process_ctty_type ctty_type));
extern void EXFUN (OS_process_deallocate, (Tprocess process));
extern pid_t EXFUN (OS_process_id, (Tprocess process));
extern Tchannel EXFUN (OS_process_input, (Tprocess process));
extern Tchannel EXFUN (OS_process_output, (Tprocess process));
extern enum process_ctty_type EXFUN (OS_process_ctty_type, (Tprocess process));
extern enum process_status EXFUN (OS_process_status, (Tprocess process));
extern unsigned short EXFUN (OS_process_reason, (Tprocess process));
extern int EXFUN (OS_process_synchronous, (Tprocess process));
extern void EXFUN (OS_process_send_signal, (Tprocess process, int sig));
extern void EXFUN (OS_process_kill, (Tprocess process));
extern void EXFUN (OS_process_stop, (Tprocess process));
extern void EXFUN (OS_process_continue, (Tprocess process));
extern void EXFUN (OS_process_interrupt, (Tprocess process));
extern void EXFUN (OS_process_quit, (Tprocess process));

#endif /* SCM_OSPROC_H */
