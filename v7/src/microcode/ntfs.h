/* -*-C-*-

$Id: ntfs.h,v 1.3 1998/02/01 05:54:42 cph Exp $

Copyright (c) 1997-98 Massachusetts Institute of Technology

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

#include "nt.h"
#include "osfs.h"

enum get_file_info_result { gfi_ok, gfi_not_found, gfi_not_accessible };

extern enum get_file_info_result NT_get_file_info
  (const char *, BY_HANDLE_FILE_INFORMATION *);

#define STAT_NOT_FOUND_P(code)						\
  (((code) == ERROR_FILE_NOT_FOUND)					\
   || ((code) == ERROR_PATH_NOT_FOUND)					\
   || ((code) == ERROR_NOT_READY)					\
   || ((code) == ERROR_INVALID_DRIVE)					\
   || ((code) == ERROR_NO_MEDIA_IN_DRIVE))

#define STAT_NOT_ACCESSIBLE_P(code)					\
  (((code) == ERROR_ACCESS_DENIED)					\
   || ((code) == ERROR_SHARING_VIOLATION)				\
   || ((code) == ERROR_DRIVE_LOCKED))
