/* -*-C-*-

$Id: prdosenv.c,v 1.4 1992/09/30 19:32:25 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
/* DOS imitation */

#include "scheme.h"
#include "prims.h"
#include "msdos.h"

DEFINE_PRIMITIVE ("CURRENT-FILE-TIME", Prim_current_file_time, 0, 0,
  "Return the current file system time stamp.\n\
This is an integer whose units are in seconds.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (DOS_time (NULL)));
}

DEFINE_PRIMITIVE ("FILE-TIME->STRING", Prim_file_time_to_string, 1, 1,
  "Convert a file system time stamp into a date/time string.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, INTEGER_P);
  {
    time_t clock = (arg_integer (1));
    char * time_string = (DOS_ctime (&clock));
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
  PRIMITIVE_RETURN (char_pointer_to_string ((unsigned char *) "c:\\"));
}

DEFINE_PRIMITIVE ("UID->STRING", Prim_uid_to_string, 1, 1,
  "Return the user name corresponding to UID.\n\
If the argument is not a known user ID, #F is returned.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("GID->STRING", Prim_gid_to_string, 1, 1,
  "Return the group name corresponding to GID.\n\
If the argument is not a known group ID, #F is returned.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("CURRENT-UID", Prim_current_uid, 0, 0,
  "Return Scheme's effective UID.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (0));
}

DEFINE_PRIMITIVE ("CURRENT-GID", Prim_current_gid, 0, 0,
  "Return Scheme's effective GID.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (0));
}

DEFINE_PRIMITIVE ("SYSTEM", Prim_system, 1, 1,
  "Invoke COMMAND.COM on the string argument.\n\
Wait until the command terminates, returning its exit status as an integer.")
{
  static int state = 0;
  extern int EXFUN (under_X32_p, (void));
  extern int EXFUN (under_DPMI_p, (void));
  PRIMITIVE_HEADER (1);

  while (1)
    switch (state)
    {
      case 0:
        state = (((under_X32_p ()) && (! (under_DPMI_p ()))) ? 1 : -1);
	break;
	
      case 1:
	PRIMITIVE_RETURN (long_to_integer (DOS_system (STRING_ARG (1))));

      case -1:
        PRIMITIVE_RETURN (long_to_integer (-1));
    }
}

DEFINE_PRIMITIVE ("UNIX-ENVIRONMENT", Prim_unix_environment_alist, 0, 0,
  "Copy the unix environment and return it as a vector of strings.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("FULL-HOSTNAME", Prim_full_hostname, 0, 0,
  "Returns the full hostname (including domain if available) as a string.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN
    (char_pointer_to_string ((unsigned char *) "PC"));
}

DEFINE_PRIMITIVE ("HOSTNAME", Prim_hostname, 0, 0,
  "Returns the hostname of the machine as a string.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN
    (char_pointer_to_string ((unsigned char *) "IBMPC"));
}

DEFINE_PRIMITIVE ("DOS-SET-KEYBOARD-MODIFIER-MASK!", Prim_dos_set_kbd_mod_mask,
		  1, 1,
		  "Set the keyboard modifier mask")
{
  extern unsigned char EXFUN (dos_set_kbd_modifier_mask, (unsigned char));
  PRIMITIVE_HEADER (1);
  
  PRIMITIVE_RETURN (long_to_integer
		    ((long)
		     (dos_set_kbd_modifier_mask ((unsigned char)
						 (arg_integer (1))))));
}

DEFINE_PRIMITIVE ("DOS-SET-KEYBOARD-TRANSLATION!",
		  Prim_dos_set_keyboard_translation,
		  3, 3, 0)
{
  int result;
  extern int EXFUN (dos_set_kbd_translation,
		    (unsigned, unsigned, unsigned char));
  PRIMITIVE_HEADER (3);
  
  result = (dos_set_kbd_translation (((unsigned) (arg_integer (1))),
				     ((unsigned) (arg_integer (2))),
				     ((unsigned char) (arg_integer (3)))));
  if (result < 0)
    error_bad_range_arg (2);

  PRIMITIVE_RETURN (long_to_integer ((long) result));
}
