/* -*-C-*-

$Id: 43538bfd124d827ec5ca009040c8ddcad7efcba2 $

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

/* Unix-specific process-environment primitives. */

#include "scheme.h"
#include "prims.h"
#include "ux.h"
#include "uxtrap.h"

extern const char * OS_current_user_name (void);
extern const char * OS_current_user_home_directory (void);

#ifdef HAVE_SOCKETS
#  include "uxsock.h"
#endif

DEFINE_PRIMITIVE ("FILE-TIME->STRING", Prim_file_time_to_string, 1, 1,
  "Convert a file system time stamp into a date/time string.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, INTEGER_P);
  {
    time_t clock = (arg_integer (1));
    char * time_string = (UX_ctime (&clock));
    (time_string[24]) = '\0';
    PRIMITIVE_RETURN (char_pointer_to_string (time_string));
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
    PRIMITIVE_RETURN ((entry == 0)
		      ? SHARP_F
		      : (char_pointer_to_string (entry -> pw_dir)));
  }
}

DEFINE_PRIMITIVE ("UID->STRING", Prim_uid_to_string, 1, 1,
  "Return the user name corresponding to UID.\n\
If the argument is not a known user ID, #F is returned.")
{
  PRIMITIVE_HEADER (1);
  {
    struct passwd * entry = (UX_getpwuid (arg_nonnegative_integer (1)));
    PRIMITIVE_RETURN ((entry == 0)
		      ? SHARP_F
		      : (char_pointer_to_string (entry -> pw_name)));
  }
}

DEFINE_PRIMITIVE ("GID->STRING", Prim_gid_to_string, 1, 1,
  "Return the group name corresponding to GID.\n\
If the argument is not a known group ID, #F is returned.")
{
  PRIMITIVE_HEADER (1);
  {
    struct group * entry = (UX_getgrgid (arg_nonnegative_integer (1)));
    PRIMITIVE_RETURN ((entry == 0)
		      ? SHARP_F
		      : (char_pointer_to_string (entry -> gr_name)));
  }
}

DEFINE_PRIMITIVE ("CURRENT-PID", Prim_current_pid, 0, 0,
  "Return Scheme's PID.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (UX_getpid ()));
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

DEFINE_PRIMITIVE ("REAL-UID", Prim_real_uid, 0, 0,
  "Return Scheme's real UID.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (UX_getuid ()));
}

DEFINE_PRIMITIVE ("REAL-GID", Prim_real_gid, 0, 0,
  "Return Scheme's real GID.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (UX_getgid ()));
}

DEFINE_PRIMITIVE ("CURRENT-USER-NAME", Prim_current_user_name, 0, 0,
  "Return (as a string) the user name of the user running Scheme.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (char_pointer_to_string (OS_current_user_name ()));
}

DEFINE_PRIMITIVE ("CURRENT-USER-HOME-DIRECTORY", Prim_current_user_home_directory, 0, 0,
  "Return the name of the current user's home directory.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN
    (char_pointer_to_string (OS_current_user_home_directory ()));
}

DEFINE_PRIMITIVE ("SYSTEM", Prim_system, 1, 1,
  "Invoke sh (the Bourne shell) on the string argument.\n\
Wait until the shell terminates, returning its exit status as an integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (UX_system (STRING_ARG (1))));
}

DEFINE_PRIMITIVE ("GET-ENVIRONMENT-VARIABLE", Prim_get_environment_variable, 1, 1,
  "Look up the value of a variable in the user's shell environment.\n\
The argument, a variable name, must be a string.\n\
The result is either a string (the variable's value),\n\
 or #F indicating that the variable does not exist.")
{
  PRIMITIVE_HEADER (1);
  {
    const char * variable_value = (UX_getenv (STRING_ARG (1)));
    PRIMITIVE_RETURN ((variable_value == 0)
		      ? SHARP_F
		      : (char_pointer_to_string (variable_value)));
  }
}

#define HOSTNAMESIZE 1024

DEFINE_PRIMITIVE ("FULL-HOSTNAME", Prim_full_hostname, 0, 0,
  "Returns the full hostname (including domain if available) as a string.")
{
  PRIMITIVE_HEADER (0);
  {
    char this_host_name [HOSTNAMESIZE];
#ifdef HAVE_SOCKETS
    struct hostent * this_host_entry;

    STD_VOID_SYSTEM_CALL
      (syscall_gethostname,
       (UX_gethostname (this_host_name, HOSTNAMESIZE)));
#else
    strcpy (this_host_name, "unknown-host.unknown.unknown");
#endif

#ifdef HAVE_SOCKETS
    this_host_entry = (gethostbyname (this_host_name));
    PRIMITIVE_RETURN ((this_host_entry == 0)
		      ? SHARP_F
		      : (char_pointer_to_string (this_host_entry -> h_name)));
#else
    PRIMITIVE_RETURN (char_pointer_to_string (this_host_name));
#endif
  }
}

DEFINE_PRIMITIVE ("HOSTNAME", Prim_hostname, 0, 0,
  "Returns the hostname of the machine as a string.")
{
  PRIMITIVE_HEADER (0);
  {
    char this_host_name[HOSTNAMESIZE];

#ifdef HAVE_SOCKETS
    STD_VOID_SYSTEM_CALL (syscall_gethostname,
			  UX_gethostname (this_host_name, HOSTNAMESIZE));
#else
    strcpy (this_host_name, "unknown-host");
#endif
    PRIMITIVE_RETURN (char_pointer_to_string (this_host_name));
  }
}

DEFINE_PRIMITIVE ("INSTRUCTION-ADDRESS->COMPILED-CODE-BLOCK",
		  Prim_instruction_address_to_compiled_code_block, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef CC_SUPPORT_P
  {
    SCHEME_OBJECT object = (ARG_REF (1));
    unsigned long pc;
    if (INTEGER_P (object))
      {
	if (! (integer_to_ulong_p (object)))
	  error_bad_range_arg (1);
	pc = (integer_to_ulong (object));
      }
    else
      {
	if (!CC_ENTRY_P (object))
	  error_bad_range_arg (1);
	pc = ((unsigned long) (CC_ENTRY_ADDRESS (object)));
      }
    PRIMITIVE_RETURN (find_ccblock (pc));
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}
