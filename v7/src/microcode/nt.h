/* -*-C-*-

$Id: nt.h,v 1.7 1997/10/22 05:28:19 cph Exp $

Copyright (c) 1993-97 Massachusetts Institute of Technology

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

/* NT system include file */

#ifndef SCM_NT_H
#define SCM_NT_H

#define SYSTEM_NAME "NT"
#define SYSTEM_VARIANT "Windows-NT"

#include <windows.h>
#include <sys/types.h>

#include <io.h>
#include <conio.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <direct.h>
#include <signal.h>
#include <errno.h>

#include <fcntl.h>

enum windows_type { wintype_unknown, wintype_31, wintype_95, wintype_nt };
extern enum windows_type NT_windows_type;

#ifndef ERRNO_NONBLOCK
#define ERRNO_NONBLOCK	1998
#endif
#ifndef EINTR
#define EINTR		1999
#endif

#include "oscond.h"
#include "ansidecl.h"
#include "posixtyp.h"

#include "intext.h"
#include "dstack.h"
#include "osscheme.h"
#include "ntsys.h"
#include "syscall.h"
#include "ntapi.h"
#include <limits.h>
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

#ifdef __STDC__
#define ALERT_CHAR '\a'
#define ALERT_STRING "\a"
#else
#define ALERT_CHAR '\007'
#define ALERT_STRING "\007"
#endif

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
extern char * EXFUN (getlogin, (void));
#endif

#ifndef WINNT
extern PTR EXFUN (malloc, (unsigned int size));
extern PTR EXFUN (realloc, (PTR ptr, unsigned int size));
extern int EXFUN (gethostname, (char * name, unsigned int size));
#endif

#ifdef _NFILE
#define NT_SC_OPEN_MAX() _NFILE
#else
#define NT_SC_OPEN_MAX() 16
#endif

#endif /* SCM_NT_H */
