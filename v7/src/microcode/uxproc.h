/* -*-C-*-

$Id: uxproc.h,v 1.6 1999/01/02 06:11:34 cph Exp $

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

#ifndef SCM_UXPROC_H
#define SCM_UXPROC_H

#include "osproc.h"

struct process
{
  pid_t id;
  long tick;
  long sync_tick;
  unsigned short raw_reason;
  unsigned short reason;
  enum process_status raw_status;
  enum process_status status;
  enum process_jc_status jc_status;
};

#define PROCESS_ID(process) ((process_table [(process)]) . id)
#define PROCESS_TICK(process) ((process_table [(process)]) . tick)
#define PROCESS_SYNC_TICK(process) ((process_table [(process)]) . sync_tick)
#define PROCESS_RAW_REASON(process) ((process_table [(process)]) . raw_reason)
#define PROCESS_REASON(process) ((process_table [(process)]) . reason)
#define PROCESS_RAW_STATUS(process) ((process_table [(process)]) . raw_status)
#define PROCESS_STATUS(process) ((process_table [(process)]) . status)
#define PROCESS_JC_STATUS(process) ((process_table [(process)]) . jc_status)

extern struct process * process_table;

/* OS_make_subprocess is obsolete, but it uses the same interface as
   UX_make_subprocess.  */
#define UX_make_subprocess OS_make_subprocess

#endif /* SCM_UXPROC_H */
