/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

extern Tprocess OS_process_table_size;
#define NO_PROCESS ((Tprocess) -1)
extern enum process_jc_status scheme_jc_status;

/* OS_make_subprocess is obsolete; use OS-specific procedure.  */
extern Tprocess OS_make_subprocess
  (const char * filename,
   const char ** argv,
   const char ** env,
   const char * working_directory,
   enum process_ctty_type ctty_type,
   char * ctty_name,
   enum process_channel_type channel_in_type,
   Tchannel channel_in,
   enum process_channel_type channel_out_type,
   Tchannel channel_out,
   enum process_channel_type channel_err_type,
   Tchannel channel_err);
extern void OS_process_deallocate (Tprocess process);

extern int OS_process_valid_p (Tprocess process);
extern int OS_process_continuable_p (Tprocess process);
extern int OS_process_foregroundable_p (Tprocess process);

extern pid_t OS_process_id (Tprocess process);
extern enum process_jc_status OS_process_jc_status (Tprocess process);
extern int OS_process_status_sync (Tprocess process);
extern int OS_process_status_sync_all (void);
extern int OS_process_any_status_change (void);
extern enum process_status OS_process_status (Tprocess process);
extern unsigned short OS_process_reason (Tprocess process);

extern void OS_process_send_signal (Tprocess process, int sig);
extern void OS_process_kill (Tprocess process);
extern void OS_process_stop (Tprocess process);
extern void OS_process_interrupt (Tprocess process);
extern void OS_process_quit (Tprocess process);
extern void OS_process_hangup (Tprocess process);

extern void OS_process_continue_background (Tprocess process);
extern void OS_process_continue_foreground (Tprocess process);
extern void OS_process_wait (Tprocess process);

#endif /* SCM_OSPROC_H */
