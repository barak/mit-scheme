/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/osfs.h,v 1.4 1992/02/08 14:54:10 cph Exp $

Copyright (c) 1990-92 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

#ifndef SCM_OSFS_H
#define SCM_OSFS_H

#include "os.h"

enum file_existence { file_does_exist, file_doesnt_exist, file_is_link };

extern enum file_existence EXFUN (OS_file_existence_test, (CONST char * name));
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
extern unsigned int EXFUN (OS_directory_open, (CONST char * name));
extern int EXFUN (OS_directory_valid_p, (long index));
extern void EXFUN (OS_directory_close, (unsigned int index));
extern CONST char * EXFUN (OS_directory_read, (unsigned int index));
extern CONST char * EXFUN
  (OS_directory_read_matching, (unsigned int index, CONST char * prefix));
extern int OS_directory_index;

#endif /* SCM_OSFS_H */
