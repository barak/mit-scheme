/* -*-C-*-

$Id: osfs.h,v 1.11 2002/11/20 19:46:12 cph Exp $

Copyright (c) 1990-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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

extern enum file_existence EXFUN (OS_file_existence_test, (CONST char * name));
extern enum file_existence EXFUN
  (OS_file_existence_test_direct, (CONST char * name));
extern enum file_type EXFUN (OS_file_type_direct, (CONST char *));
extern enum file_type EXFUN (OS_file_type_indirect, (CONST char *));
extern int EXFUN (OS_file_access, (CONST char * name, unsigned int mode));
extern int EXFUN (OS_file_directory_p, (CONST char * name));
extern CONST char * EXFUN (OS_file_soft_link_p, (CONST char * name));
extern void EXFUN (OS_file_remove, (CONST char * name));
extern void EXFUN (OS_file_remove_link, (CONST char * name));
extern void EXFUN
  (OS_file_rename, (CONST char * from_name, CONST char * to_name));
extern void EXFUN
  (OS_file_link_hard, (CONST char * from_name, CONST char * to_name));
extern void EXFUN
  (OS_file_link_soft, (CONST char * from_name, CONST char * to_name));
extern void EXFUN (OS_directory_make, (CONST char * name));
extern void EXFUN (OS_directory_delete, (CONST char * name));
extern int EXFUN (OS_file_touch, (CONST char *));
extern unsigned int EXFUN (OS_directory_open, (CONST char * name));
extern int EXFUN (OS_directory_valid_p, (long index));
extern void EXFUN (OS_directory_close, (unsigned int index));
extern CONST char * EXFUN (OS_directory_read, (unsigned int index));
extern CONST char * EXFUN
  (OS_directory_read_matching, (unsigned int index, CONST char * prefix));

#endif /* SCM_OSFS_H */
