/* -*-C-*-

$Id: prdosenv.c,v 1.11 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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

/* Unix-specific process-environment primitives. */
/* DOS imitation */

#include "scheme.h"
#include "prims.h"
#include "msdos.h"
#include "dosio.h"

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

DEFINE_PRIMITIVE ("SYSTEM", Prim_system, 1, 1,
  "Invoke COMMAND.COM on the string argument.\n\
Wait until the command terminates, returning its exit status as an integer.")
{
  int result;
  extern int EXFUN (X32_system, (char *));
  PRIMITIVE_HEADER (1);
  
  result = (X32_system (STRING_ARG (1)));
  if (result < -1)
    error_external_return ();
  PRIMITIVE_RETURN (long_to_integer ((long) result));
}

/* values for io specs:
   -1   => default (console)
   >= 0 => channel number.
 */

static int
DEFUN (arg_io_spec, (arg_no), int arg_no)
{
  int arg = (arg_integer (arg_no));
  
  if ((arg < -1)
      || (arg >= ((int) OS_channel_table_size))
      || ((arg >= 0)
	  && (! (OS_channel_open_p ((Tchannel) arg)))))
    error_bad_range_arg (arg_no);
  return (arg);
}

DEFINE_PRIMITIVE ("RUN-SUBPROCESS", Prim_run_subprocess, 4, 4,
  "Invoke COMMAND.COM on STRING with I/O as specified by other arguments.\n\
Wait until the command terminates, returning its exit status as an integer.")
{
  int result;
  extern int EXFUN (X32_subprocess, (char *, int, int, int));
  PRIMITIVE_HEADER (3);
  
  result = (X32_subprocess ((STRING_ARG (1)),
			    (arg_io_spec (2)),
			    (arg_io_spec (3)),
			    (arg_io_spec (4))));
  if (result < -1)
    error_external_return ();
  PRIMITIVE_RETURN (long_to_integer ((long) result));
}

DEFINE_PRIMITIVE ("GET-ENVIRONMENT-VARIABLE", Prim_get_environment_variable, 1, 1,
  "Look up the value of a variable in the user's shell environment.\n\
The argument, a variable name, must be a string.\n\
The result is either a string (the variable's value),\n\
 or #F indicating that the variable does not exist.")
{
  PRIMITIVE_HEADER (1);
  {
    CONST char * variable_value = (DOS_getenv (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((variable_value == 0)
       ? SHARP_F
       : (char_pointer_to_string ((unsigned char *) variable_value)));
  }
}
