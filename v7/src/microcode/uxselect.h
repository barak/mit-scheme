/* -*-C-*-

$Id: uxselect.h,v 1.11 2007/04/22 16:31:23 cph Exp $

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

#ifndef SCM_UXSELECT_H
#define SCM_UXSELECT_H

enum select_input
{
  select_input_argument,
  select_input_other,
  select_input_none,
  select_input_process_status,
  select_input_interrupt
};

extern enum select_input UX_select_input (int fd, int blockp);
extern unsigned int UX_select_registry_size (void);
extern unsigned int UX_select_registry_lub (void);
extern void UX_select_registry_clear_all (void * fds);
extern void UX_select_registry_set (void * fds, unsigned int fd);
extern void UX_select_registry_clear (void * fds, unsigned int fd);
extern int UX_select_registry_is_set (void * fds, unsigned int fd);
extern enum select_input UX_select_registry_test
  (void * input_fds, int blockp,
    unsigned int * output_fds, unsigned int * output_nfds);
extern enum select_input UX_select_descriptor
  (unsigned int fd, int blockp);

#endif /* SCM_UXSELECT_H */
