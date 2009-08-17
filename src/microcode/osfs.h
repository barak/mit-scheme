/* -*-C-*-

$Id: 747fb00370504cd29c32ab18dc018bb21c6024be $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

#ifndef SCM_OSFS_H
#define SCM_OSFS_H

#include "os.h"

enum file_existence { file_does_exist, file_doesnt_exist, file_is_link };

enum file_type
{
  file_type_nonexistent = (-1),
  file_type_regular = 0,
  file_type_directory,
  file_type_unix_symbolic_link,
  file_type_unix_character_device,
  file_type_unix_block_device,
  file_type_unix_fifo,
  file_type_unix_stream_socket,
  file_type_os2_named_pipe,
  file_type_win32_named_pipe,
  file_type_unknown = 0xFFFF
};

extern enum file_existence OS_file_existence_test (const char *);
extern enum file_existence OS_file_existence_test_direct (const char *);
extern enum file_type OS_file_type_direct (const char *);
extern enum file_type OS_file_type_indirect (const char *);
extern int OS_file_access (const char *, unsigned int);
extern int OS_file_directory_p (const char *);
extern const char * OS_file_soft_link_p (const char *);
extern void OS_file_remove (const char *);
extern void OS_file_remove_link (const char *);
extern void OS_file_rename (const char *, const char *);
extern void OS_file_link_hard (const char *, const char *);
extern void OS_file_link_soft (const char *, const char *);
extern void OS_directory_make (const char *);
extern void OS_directory_delete (const char *);
extern int OS_file_touch (const char *);
extern unsigned int OS_directory_open (const char *);
extern int OS_directory_valid_p (unsigned int);
extern void OS_directory_close (unsigned int);
extern const char * OS_directory_read (unsigned int);
extern const char * OS_directory_read_matching (unsigned int, const char *);
extern int OS_channel_copy (off_t, Tchannel, Tchannel);
extern void OS_file_copy (const char *, const char *);

#endif /* SCM_OSFS_H */
