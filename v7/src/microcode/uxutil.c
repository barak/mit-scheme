/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxutil.c,v 1.3 1991/07/24 19:48:29 jinx Exp $

Copyright (c) 1990, 1991 Massachusetts Institute of Technology

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

#include "ux.h"
#include "uxutil.h"
#include <ctype.h>

static CONST char *
DEFUN (char_description_brief, (c), unsigned char c)
{
  static char buffer [5];
  switch (c)
    {
    case ' ': return ("SPC");
    case '\t': return ("TAB");
    case '\r': return ("RET");
    case '\n': return ("LFD");
    case '\033': return ("ESC");
    case '\177': return ("DEL");
    default:
      if (c < ' ')
	{
	  (buffer[0]) = '^';
	  (buffer[1]) = (c + '@');
	  (buffer[2]) = '\0';
	}
      else if (c < '\177')
	{
	  (buffer[0]) = c;
	  (buffer[1]) = '\0';
	}
      else
	{
	  (buffer[0]) = '\\';
	  (buffer[1]) = (c >> 6);
	  (buffer[2]) = ((c >> 3) & 7);
	  (buffer[3]) = (c & 7);
	  (buffer[4]) = '\0';
	}
      return (buffer);
    }
}

CONST char *
DEFUN (char_description, (c, long_p), unsigned char c AND int long_p)
{
  static char buffer [64];
  CONST char * description = (char_description_brief (c));
  if (long_p)
    {
      int meta = (c >= 0200);
      int cc = (c & 0177);
      int control = (cc < 0040);
      if (meta || control)
	{
	  sprintf (buffer, "`%s' (%s%s%c)",
		   description,
		   (meta ? "meta-" : ""),
		   (control ? "control-" : ""),
		   (control ? (cc + 0100) : cc));
	  return (buffer);
	}
    }
  sprintf (buffer, "`%s'", description);
  return (buffer);
}

static Ttty_state original_tty_state;

void
DEFUN_VOID (UX_initialize_userio)
{
  UX_terminal_get_state (STDIN_FILENO, (&original_tty_state));
}

static void
DEFUN (restore_input_state, (ap), PTR ap)
{
  UX_terminal_set_state (STDIN_FILENO, ap);
}

static Ttty_state *
DEFUN_VOID (save_input_state)
{
  Ttty_state * s = (dstack_alloc (sizeof (Ttty_state)));
  UX_terminal_get_state (STDIN_FILENO, s);
  transaction_record_action (tat_always, restore_input_state, s);
  return (s);
}

void
DEFUN_VOID (userio_buffered_input)
{
  save_input_state ();
  UX_terminal_set_state (STDIN_FILENO, (&original_tty_state));
}

char
DEFUN_VOID (userio_read_char)
{
  char c;
  while (1)
    {
      int nread;

      errno = 0;
      nread = (UX_read (STDIN_FILENO, (&c), 1));
      if (nread == 1)
	break;
      if (errno != EINTR)
	{
	  c = '\0';
	  break;
	}
    }
  return (c);
}

char
DEFUN_VOID (userio_read_char_raw)
{
  transaction_begin ();
  {
    /* Must split declaration and assignment because some compilers
       do not permit aggregate initializers. */
    Ttty_state state;
    state = (* (save_input_state ()));
    terminal_state_raw ((&state), STDIN_FILENO);
    UX_terminal_set_state (STDIN_FILENO, (&state));
  }
  {
    char c = (userio_read_char ());
    transaction_commit ();
    return (c);
  }
}

char
DEFUN (userio_choose_option, (herald, prompt, choices),
       CONST char * herald AND
       CONST char * prompt AND
       CONST char ** choices)
{
  while (1)
    {
      fputs (herald, stdout);
      putc ('\n', stdout);
      {
	CONST char ** scan = choices;
	while (1)
	  {
	    CONST char * choice = (*scan++);
	    if (choice == 0)
	      break;
	    fprintf (stdout, "  %s\n", choice);
	  }
      }
      fputs (prompt, stdout);
      fflush (stdout);
      {
	char command = (userio_read_char_raw ());
	if ((command == '\0') && (errno != 0))
	  return (command);
	putc ('\n', stdout);
	fflush (stdout);
	if (islower (command))
	  command = (toupper (command));
	{
	  CONST char ** scan = choices;
	  while (1)
	    {
	      CONST char * choice = (*scan++);
	      if (choice == 0)
		break;
	      {
		char option = (*choice);
		if (islower (option))
		  option = (toupper (option));
		if (command == option)
		  return (option);
	      }
	    }
	}
      }
    }
}

int
DEFUN (userio_confirm, (prompt), CONST char * prompt)
{
  while (1)
    {
      fputs (prompt, stdout);
      switch (userio_read_char_raw ())
	{
	case 'y':
	case 'Y':
	  return (1);
	case 'n':
	case 'N':
	  return (0);
	case '\0':
	  if (errno != 0)
	  {
	    /* IO problems, assume everything scrod. */
	    fprintf (stderr, "Problems reading keyboard input -- exiting.\n");
	    termination_eof ();
	  }
	}
    }
}
