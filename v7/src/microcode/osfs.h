/* -*-C-*-

$Id: osfs.h,v 1.9 2000/12/05 21:23:47 cph Exp $

Copyright (c) 1990-2000 Massachusetts Institute of Technology

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

#ifndef SCM_OSFS_H
#define SCM_OSFS_H

#include "os.h"

enum file_existence { file_does_exist, file_doesnt_exist, file_is_link };

extern enum file_existence EXFUN (OS_file_existence_test, (CONST char * name));
extern enum file_existence EXFUN
  (OS_file_existence_test_direct, (CONST char * name));
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
