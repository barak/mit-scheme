/* -*-C-*-

$Id: prdosenv.c,v 1.7 1992/10/02 01:48:18 jinx Exp $

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
#include "dosio.h"

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
