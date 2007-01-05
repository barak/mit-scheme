/* -*-C-*-

$Id: uxproc.h,v 1.10 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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
