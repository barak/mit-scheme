/* -*-C-*-

$Id: osproc.h,v 1.10 1999/01/02 06:11:34 cph Exp $

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

enum process_jc_status
{
  process_jc_status_no_ctty,	/* job has no control terminal */
  process_jc_status_unrelated,	/* job's ctty different from Scheme's */
  process_jc_status_no_jc,	/* job has same ctty, jc not available */
  process_jc_status_jc		/* job has same ctty, jc available */
};

enum process_ctty_type
{
  /* No controlling terminal.
     Used for batch jobs, similar to `nohup' program. */
  process_ctty_type_none,

  /* Use Scheme's controlling terminal, run in background. */
  process_ctty_type_inherit_bg,

  /* Use Scheme's controlling terminal, run in foreground. */
  process_ctty_type_inherit_fg,

  /* Use given controlling terminal, usually a PTY. */
  process_ctty_type_explicit
};

enum process_channel_type
{
  process_channel_type_none,
  process_channel_type_inherit,
  process_channel_type_ctty,
  process_channel_type_explicit
};

extern size_t OS_process_table_size;
#define NO_PROCESS OS_process_table_size
extern enum process_jc_status scheme_jc_status;

/* OS_make_subprocess is obsolete; use OS-specific procedure.  */
extern Tprocess EXFUN
  (OS_make_subprocess,
   (CONST char * filename,
    CONST char ** argv,
    CONST char ** env,
    CONST char * working_directory,
    enum process_ctty_type ctty_type,
    char * ctty_name,
    enum process_channel_type channel_in_type,
    Tchannel channel_in,
    enum process_channel_type channel_out_type,
    Tchannel channel_out,
    enum process_channel_type channel_err_type,
    Tchannel channel_err));
extern void EXFUN (OS_process_deallocate, (Tprocess process));

extern int EXFUN (OS_process_valid_p, (Tprocess process));
extern int EXFUN (OS_process_continuable_p, (Tprocess process));
extern int EXFUN (OS_process_foregroundable_p, (Tprocess process));

extern pid_t EXFUN (OS_process_id, (Tprocess process));
extern enum process_jc_status EXFUN (OS_process_jc_status, (Tprocess process));
extern int EXFUN (OS_process_status_sync, (Tprocess process));
extern int EXFUN (OS_process_status_sync_all, (void));
extern int EXFUN (OS_process_any_status_change, (void));
extern enum process_status EXFUN (OS_process_status, (Tprocess process));
extern unsigned short EXFUN (OS_process_reason, (Tprocess process));

extern void EXFUN (OS_process_send_signal, (Tprocess process, int sig));
extern void EXFUN (OS_process_kill, (Tprocess process));
extern void EXFUN (OS_process_stop, (Tprocess process));
extern void EXFUN (OS_process_interrupt, (Tprocess process));
extern void EXFUN (OS_process_quit, (Tprocess process));
extern void EXFUN (OS_process_hangup, (Tprocess process));

extern void EXFUN (OS_process_continue_background, (Tprocess process));
extern void EXFUN (OS_process_continue_foreground, (Tprocess process));
extern void EXFUN (OS_process_wait, (Tprocess process));

#endif /* SCM_OSPROC_H */
