/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/pruxenv.c,v 1.5 1991/10/29 22:55:11 jinx Exp $

Copyright (c) 1990-91 Massachusetts Institute of Technology

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

/* Unix-specific process-environment primitives. */

#include "scheme.h"
#include "prims.h"
#include "ux.h"

#ifdef HAVE_SOCKETS
#include "uxsock.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#endif

extern char ** environ;

DEFINE_PRIMITIVE ("CURRENT-FILE-TIME", Prim_current_file_time, 0, 0,
  "Return the current file system time stamp.\n\
This is an integer whose units are in seconds.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (UX_time (0)));
}

DEFINE_PRIMITIVE ("FILE-TIME->STRING", Prim_file_time_to_string, 1, 1,
  "Convert a file system time stamp into a date/time string.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, INTEGER_P);
  {
    time_t clock = (arg_integer (1));
    char * time_string = (UX_ctime (&clock));
    (time_string[24]) = '\0';
    PRIMITIVE_RETURN (char_pointer_to_string ((unsigned char *) time_string));
  }
}

DEFINE_PRIMITIVE ("GET-USER-HOME-DIRECTORY", Prim_get_user_home_directory, 1, 1,
  "Return the file name of a given user's home directory.\n\
The user name argument must be a string.\n\
If no such user is known, #F is returned.")
{
  PRIMITIVE_HEADER (1);
  {
    struct passwd * entry = (UX_getpwnam (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((entry == 0) ? SHARP_F
       : (char_pointer_to_string ((unsigned char *) (entry -> pw_dir))));
  }
}

DEFINE_PRIMITIVE ("UID->STRING", Prim_uid_to_string, 1, 1,
  "Return the user name corresponding to UID.\n\
If the argument is not a known user ID, #F is returned.")
{
  PRIMITIVE_HEADER (1);
  {
    struct passwd * entry = (UX_getpwuid (arg_nonnegative_integer (1)));
    PRIMITIVE_RETURN
      ((entry == 0) ? SHARP_F
       : (char_pointer_to_string ((unsigned char *) (entry -> pw_name))));
  }
}

DEFINE_PRIMITIVE ("GID->STRING", Prim_gid_to_string, 1, 1,
  "Return the group name corresponding to GID.\n\
If the argument is not a known group ID, #F is returned.")
{
  PRIMITIVE_HEADER (1);
  {
    struct group * entry = (UX_getgrgid (arg_nonnegative_integer (1)));
    PRIMITIVE_RETURN
      ((entry == 0) ? SHARP_F
       : (char_pointer_to_string ((unsigned char *) (entry -> gr_name))));
  }
}

DEFINE_PRIMITIVE ("CURRENT-UID", Prim_current_uid, 0, 0,
  "Return Scheme's effective UID.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (UX_geteuid ()));
}

DEFINE_PRIMITIVE ("CURRENT-GID", Prim_current_gid, 0, 0,
  "Return Scheme's effective GID.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (UX_getegid ()));
}

DEFINE_PRIMITIVE ("SYSTEM", Prim_system, 1, 1,
  "Invoke sh (the Bourne shell) on the string argument.\n\
Wait until the shell terminates, returning its exit status as an integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (UX_system (STRING_ARG (1))));
}

DEFINE_PRIMITIVE ("UNIX-ENVIRONMENT", Prim_unix_environment_alist, 0, 0,
  "Copy the unix environment and return it as a vector of strings.")
{
  PRIMITIVE_HEADER (0);
  {
    char ** scan = environ;
    char ** end = scan;
    while ((*end++) != 0);
    end -= 1;
    {
      SCHEME_OBJECT result =
	(allocate_marked_vector (TC_VECTOR, (end - scan), 1));
      SCHEME_OBJECT * scan_result = (VECTOR_LOC (result, 0));
      while (scan < end)
	(*scan_result++) =
	  (char_pointer_to_string ((unsigned char *) (*scan++)));
      PRIMITIVE_RETURN (result);
    }
  }
}

#define HOSTNAMESIZE 1024

DEFINE_PRIMITIVE ("FULL-HOSTNAME", Prim_full_hostname, 0, 0,
  "Returns the full hostname (including domain if available) as a string.")
{
  PRIMITIVE_HEADER (0);
  {
    char this_host_name[HOSTNAMESIZE];
#ifdef HAVE_SOCKETS
    struct hostent * EXFUN (gethostbyname, (char *));
    struct hostent *this_host_entry;
#endif
    STD_VOID_SYSTEM_CALL (syscall_gethostname,
			  UX_gethostname (this_host_name, HOSTNAMESIZE));

#ifdef HAVE_SOCKETS
    this_host_entry = gethostbyname (this_host_name);
    PRIMITIVE_RETURN
      (char_pointer_to_string ((unsigned char *) (this_host_entry->h_name)));
#else
    PRIMITIVE_RETURN
      (char_pointer_to_string ((unsigned char *) this_host_name));
#endif
  }
}

DEFINE_PRIMITIVE ("HOSTNAME", Prim_hostname, 0, 0,
  "Returns the hostname of the machine as a string.")
{
  PRIMITIVE_HEADER (0);
  {
    char this_host_name[HOSTNAMESIZE];

    STD_VOID_SYSTEM_CALL (syscall_gethostname,
			  UX_gethostname (this_host_name, HOSTNAMESIZE));
    PRIMITIVE_RETURN
      (char_pointer_to_string ((unsigned char *) this_host_name));
  }
}
