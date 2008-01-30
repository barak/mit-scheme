/* -*-C-*-

$Id: nt.h,v 1.15 2008/01/30 20:02:14 cph Exp $

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

/* NT system include file */

#ifndef SCM_NT_H
#define SCM_NT_H

#include <windows.h>
#include <sys/types.h>

#include <io.h>
#include <conio.h>
#include <sys/stat.h>
#include <direct.h>
#include <signal.h>
#include <errno.h>

#include <fcntl.h>

enum windows_type { wintype_unknown, wintype_31, wintype_95, wintype_nt };
extern enum windows_type NT_windows_type;

#ifndef ERRNO_NONBLOCK
#  define ERRNO_NONBLOCK 1998
#endif
#ifndef EINTR
#  define EINTR 1999
#endif

#include "config.h"
#include "intext.h"
#include "dstack.h"
#include "osscheme.h"
#include "ntsys.h"
#include "syscall.h"
#include "ntapi.h"
#include <time.h>

/* Crufty, but it will work here. */
#ifndef ENOSYS
#define ENOSYS 0
#endif

/* constants for access() */
#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 128
#endif

#define ALERT_CHAR '\a'
#define ALERT_STRING "\a"

#ifndef GUI
  extern HANDLE  STDIN_HANDLE,  STDOUT_HANDLE,  STDERR_HANDLE;
#endif

/* constants for open() and fcntl() */
#ifndef O_RDONLY
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#endif

/* mode bit definitions for open(), creat(), and chmod() */
#ifndef S_IRWXU
#define S_IRWXU 0700
#define S_IRWXG 0070
#define S_IRWXO 0007
#endif

#ifndef S_IRUSR
#define S_IRUSR 0400
#define S_IWUSR 0200
#define S_IXUSR 0100
#define S_IRGRP 0040
#define S_IWGRP 0020
#define S_IXGRP 0010
#define S_IROTH 0004
#define S_IWOTH 0002
#define S_IXOTH 0001
#endif

#define MODE_REG (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#define MODE_DIR (MODE_REG | S_IXUSR | S_IXGRP | S_IXOTH)

/* constants for lseek() */
#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#ifndef DECL_GETLOGIN
extern char * getlogin (void);
#endif

#ifdef _NFILE
#define NT_SC_OPEN_MAX() _NFILE
#else
#define NT_SC_OPEN_MAX() 16
#endif

#endif /* SCM_NT_H */
