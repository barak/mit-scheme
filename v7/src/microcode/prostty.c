/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prostty.c,v 1.1 1990/06/20 19:38:38 cph Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

/* Primitives to perform I/O to and from the console. */

#include "scheme.h"
#include "prims.h"
#include "ostty.h"
#include "osctty.h"
#include "ossig.h"
#include "osfile.h"
#include "osio.h"

static int transcript_file_open;
static Tchannel transcript_channel;

void
DEFUN_VOID (OS_initialize_transcript_file)
{
  transcript_file_open = 0;
  return;
}

DEFINE_PRIMITIVE ("TRANSCRIPT-ON", Prim_transcript_on, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  if (transcript_file_open)
    error_external_return ();
  transcript_channel = (OS_open_output_file (STRING_ARG (1)));
  transcript_file_open = 1;
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TRANSCRIPT-OFF", Prim_transcript_off, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  if (transcript_file_open)
    {
      OS_channel_close (transcript_channel);
      transcript_file_open = 0;
    }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TTY-INPUT-CHANNEL", Prim_tty_input_channel, 0, 0,
  "Return the standard input channel.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (OS_tty_input_channel ()));
}

DEFINE_PRIMITIVE ("TTY-OUTPUT-CHANNEL", Prim_tty_output_channel, 0, 0,
  "Return the standard output channel.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (OS_tty_output_channel ()));
}

DEFINE_PRIMITIVE ("TTY-X-SIZE", Prim_tty_x_size, 0, 0,
  "Return the display width in character columns.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (OS_tty_x_size ()));
}

DEFINE_PRIMITIVE ("TTY-Y-SIZE", Prim_tty_y_size, 0, 0,
  "Return the display height in character lines.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (OS_tty_y_size ()));
}

DEFINE_PRIMITIVE ("TTY-COMMAND-BEEP", Prim_tty_command_beep, 0, 0,
  "Return a string that, when written to the display, will make it beep.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (char_pointer_to_string (OS_tty_command_beep ()));
}

DEFINE_PRIMITIVE ("TTY-COMMAND-CLEAR", Prim_tty_command_clear, 0, 0,
  "Return a string that, when written to the display, will clear it.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (char_pointer_to_string (OS_tty_command_clear ()));
}

DEFINE_PRIMITIVE ("TTY-READ-CHAR-READY?", Prim_tty_read_char_ready_p, 1, 1,
  "This is an obsolete primitive.\n\
Return #T iff a character is ready to be read from the console.\n\
Argument DELAY says how many milliseconds to wait for a character.\n\
If a character is typed, #T is returned immediately,\n\
 otherwise #F is returned after DELAY has expired.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (OS_tty_char_ready_p (arg_nonnegative_integer (1))));
}

DEFINE_PRIMITIVE ("TTY-READ-CHAR", Prim_tty_read_char, 0, 0,
  "This is an obsolete primitive.\n\
Read a character from the console.\n\
The operating system's input editor is used to provide the character.")
{
  PRIMITIVE_HEADER (0);
  {
    char c = (OS_tty_read_char ());
    if (transcript_file_open)
      {
	if ((OS_channel_write (transcript_channel, (&c), 1)) != 1)
	  error_external_return ();
      }
    PRIMITIVE_RETURN (ASCII_TO_CHAR (c));
  }
}

DEFINE_PRIMITIVE ("TTY-READ-CHAR-IMMEDIATE", Prim_tty_read_char_immediate, 0, 0,
  "This is an obsolete primitive.\n\
Read a character from the console, without input editing.\n\
First, any pending input is discarded,\n\
 then the next character typed is returned immediately.")
{
  PRIMITIVE_HEADER (0);
  {
    char c = (OS_tty_read_char_immediate ());
    if (transcript_file_open)
      {
	if ((OS_channel_write (transcript_channel, (&c), 1)) != 1)
	  error_external_return ();
      }
    PRIMITIVE_RETURN (ASCII_TO_CHAR (c));
  }
}

DEFINE_PRIMITIVE ("TTY-WRITE-CHAR", Prim_tty_write_char, 1, 1,
  "This is an obsolete primitive.\n\
Write a character to the console.")
{
  PRIMITIVE_HEADER (1);
  {
    char c = (arg_ascii_char (1));
    OS_tty_write_char (c);
    if (transcript_file_open)
      {
	if ((OS_channel_write (transcript_channel, (&c), 1)) != 1)
	  error_external_return ();
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TTY-WRITE-STRING", Prim_tty_write_string, 1, 1,
  "This is an obsolete primitive.\n\
Write a string to the console.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  {
    fast SCHEME_OBJECT argument = (ARG_REF (1));
    fast CONST PTR string = (STRING_LOC (argument, 0));
    OS_tty_write_string (string);
    if (transcript_file_open)
      {
	long length = (STRING_LENGTH (argument));
	if ((OS_channel_write (transcript_channel, string, length)) != length)
	  error_external_return ();
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TTY-FLUSH-OUTPUT", Prim_tty_flush_output, 0, 0,
  "This is an obsolete primitive.\n\
Write the contents of the console output buffer to the console.\n\
Return after all of the contents has been written.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TTY-BEEP", Prim_tty_beep, 0, 0,
  "This is an obsolete primitive.\n\
Ring the console bell.")
{
  PRIMITIVE_HEADER (0);
  OS_tty_beep ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TTY-CLEAR", Prim_tty_clear, 0, 0,
  "This is an obsolete primitive.\n\
Clear the console screen.")
{
  PRIMITIVE_HEADER (0);
  OS_tty_write_string (OS_tty_command_clear ());
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TTY-NEXT-INTERRUPT-CHAR", Prim_tty_next_interrupt_char, 0, 0,
  "Return the next interrupt character in the console input buffer.\n\
The character is returned as an unsigned integer.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (OS_tty_next_interrupt_char ()));
}

DEFINE_PRIMITIVE ("TTY-CLEAN-INTERRUPTS", Prim_tty_clean_interrupts, 2, 2,
  "Clear the input buffer for a character interrupt.\n\
First arg MODE says how:\n\
 0 ==> discard input up to the most recent interrupt marker\n\
       that matches second arg CHAR.\n\
 1 ==> remove all interrupt markers that match second arg CHAR.\n\
CHAR should be the result of a call to `tty-next-interrupt-char'.")
{
  PRIMITIVE_HEADER (2);
  OS_tty_clean_interrupts ((arg_index_integer (1, 2)),
			   ((enum tty_clean_mode) (arg_ascii_integer (2))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TTY-GET-INTERRUPT-ENABLES", Prim_tty_get_interrupt_enables, 0, 0,
  "Return the current keyboard interrupt enables.")
{
  PRIMITIVE_HEADER (0);
  {
    Tinterrupt_enables mask;
    OS_ctty_get_interrupt_enables (&mask);
    PRIMITIVE_RETURN (long_to_integer (mask));
  }
}

DEFINE_PRIMITIVE ("TTY-SET-INTERRUPT-ENABLES", Prim_tty_set_interrupt_enables, 1, 1,
  "Change the keyboard interrupt enables to MASK.")
{
  PRIMITIVE_HEADER (1);
  {
    Tinterrupt_enables mask = (arg_integer (1));
    OS_ctty_set_interrupt_enables (&mask);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TTY-GET-INTERRUPT-CHARS", Prim_tty_get_interrupt_chars, 0, 0,
  "Return the current interrupt characters as a string.")
{
  PRIMITIVE_HEADER (0);
  {
    SCHEME_OBJECT result = (allocate_string (6));
    unsigned char * scan = (STRING_LOC (result, 0));
    (*scan++) = ((unsigned char) (OS_ctty_quit_char ()));
    (*scan++) = ((unsigned char) (OS_signal_quit_handler ()));
    (*scan++) = ((unsigned char) (OS_ctty_int_char ()));
    (*scan++) = ((unsigned char) (OS_signal_int_handler ()));
    (*scan++) = ((unsigned char) (OS_ctty_tstp_char ()));
    (*scan) = ((unsigned char) (OS_signal_tstp_handler ()));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("TTY-SET-INTERRUPT-CHARS", Prim_tty_set_interrupt_chars, 1, 1,
  "Change the current interrupt characters to STRING.\n\
STRING must be in the correct form for this operating system.")
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT argument = (ARG_REF (1));
    if (! ((STRING_P (argument)) && ((STRING_LENGTH (argument)) == 6)))
      error_wrong_type_arg (1);
    OS_signal_set_interrupt_handlers
      (((enum interrupt_handler) (STRING_REF (argument, 1))),
       ((enum interrupt_handler) (STRING_REF (argument, 3))),
       ((enum interrupt_handler) (STRING_REF (argument, 5))));
    OS_ctty_set_interrupt_chars
      ((STRING_REF (argument, 0)),
       (STRING_REF (argument, 2)),
       (STRING_REF (argument, 4)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SET-TTY-INTERRUPT-ENABLES!", Prim_set_tty_interrupt_enables, 1, 1,
  "This primitive is obsolete.")
{
  PRIMITIVE_HEADER (1);
  {
    Tinterrupt_enables old;
    Tinterrupt_enables new = (arg_integer (1));
    OS_ctty_get_interrupt_enables (&old);
    OS_ctty_set_interrupt_enables (&new);
    PRIMITIVE_RETURN (long_to_integer (old));
  }
}
