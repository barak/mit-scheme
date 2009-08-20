/* -*-C-*-

$Id$

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

#include "scheme.h"
#include "ux.h"
#include "uxutil.h"
#include <ctype.h>

extern void terminal_state_raw (Ttty_state *, int);

static const char *
char_description_brief (unsigned char c)
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

const char *
char_description (unsigned char c, int long_p)
{
  static char buffer [64];
  const char * description = (char_description_brief (c));
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
UX_initialize_userio (void)
{
  UX_terminal_get_state (STDIN_FILENO, (&original_tty_state));
}

static void
restore_input_state (void * ap)
{
  UX_terminal_set_state (STDIN_FILENO, ap);
}

static Ttty_state *
save_input_state (void)
{
  Ttty_state * s = (dstack_alloc (sizeof (Ttty_state)));
  UX_terminal_get_state (STDIN_FILENO, s);
  transaction_record_action (tat_always, restore_input_state, s);
  return (s);
}

void
userio_buffered_input (void)
{
  save_input_state ();
  UX_terminal_set_state (STDIN_FILENO, (&original_tty_state));
}

char
userio_read_char (void)
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
userio_read_char_raw (void)
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
userio_choose_option (const char * herald,
		      const char * prompt,
		      const char ** choices)
{
  while (1)
    {
      fputs (herald, stdout);
      putc ('\n', stdout);
      {
	const char ** scan = choices;
	while (1)
	  {
	    const char * choice = (*scan++);
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
	  const char ** scan = choices;
	  while (1)
	    {
	      const char * choice = (*scan++);
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
userio_confirm (const char * prompt)
{
  while (1)
    {
      fputs (prompt, stdout);
      fflush (stdout);
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
