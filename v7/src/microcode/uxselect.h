/* -*-C-*-

$Id: uxselect.h,v 1.8 2003/02/14 18:28:24 cph Exp $

Copyright (c) 1991-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
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

extern enum select_input EXFUN (UX_select_input, (int fd, int blockp));
extern unsigned int EXFUN (UX_select_registry_size, (void));
extern unsigned int EXFUN (UX_select_registry_lub, (void));
extern void EXFUN (UX_select_registry_clear_all, (PTR fds));
extern void EXFUN (UX_select_registry_set, (PTR fds, unsigned int fd));
extern void EXFUN (UX_select_registry_clear, (PTR fds, unsigned int fd));
extern int EXFUN (UX_select_registry_is_set, (PTR fds, unsigned int fd));
extern enum select_input EXFUN
  (UX_select_registry_test,
   (PTR input_fds, int blockp,
    unsigned int * output_fds, unsigned int * output_nfds));
extern enum select_input EXFUN
  (UX_select_descriptor, (unsigned int fd, int blockp));

#endif /* SCM_UXSELECT_H */
