/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

#ifndef SCM_OSTOP_H
#define SCM_OSTOP_H

#include "os.h"

extern int OS_under_emacs_p (void);
extern void OS_initialize (void);
extern void OS_reset (void);
extern void OS_quit (int code, int abnormal_p);
extern void OS_restartable_exit (void);
extern void OS_save_external_state (void);
extern void OS_save_internal_state (void);
extern void OS_restore_internal_state (void);
extern void OS_restore_external_state (void);
extern const char * OS_error_code_to_message (unsigned int code);
extern void OS_expect_sequential_access (void *start, void *end);
extern void OS_expect_normal_access (void *start, void *end);
extern void OS_free_pages (void *start, void *end);

#endif /* SCM_OSTOP_H */
