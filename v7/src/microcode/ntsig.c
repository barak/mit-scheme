/* -*-C-*-

$Id: ntsig.c,v 1.29 2008/01/30 20:02:15 cph Exp $

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


/* Hacks by SRA for NT:
    1. punt interactive debugging completely
*/

#include "scheme.h"
#include "critsec.h"
#include "ossig.h"
#include "osctty.h"
#include "ostty.h"
#include "nt.h"
#include "ntgui.h"
#include "ntio.h"
#include "ntscmlib.h"
#include "ntscreen.h"
#include "ntsys.h"

extern HANDLE master_tty_window;

/* Signal mask manipulation */

/* These could be implemented, at least under Win32s/DPMI
   by examining and setting the virtual interrupt state.
 */

void
preserve_signal_mask (void)
{
  return;
}

void
block_signals (void)
{
  return;
}

void
unblock_signals (void)
{
  return;
}

#define CONTROL_B_INTERRUPT_CHAR	'B'
#define CONTROL_G_INTERRUPT_CHAR	'G'
#define CONTROL_U_INTERRUPT_CHAR	'U'
#define CONTROL_X_INTERRUPT_CHAR	'X'
#define INTERACTIVE_INTERRUPT_CHAR	'!'
#define TERMINATE_INTERRUPT_CHAR	'@'
#define NO_INTERRUPT_CHAR		'0'

/* Keyboard interrupt */

#define KB_INT_TABLE_SIZE		((256) + 1)

#define CONTROL_BREAK			'\0'		/* A lie. */
#define CONTROL_B			'\002'
#define CONTROL_C			'\003'
#define CONTROL_G			'\007'
#define CONTROL_U			'\025'
#define CONTROL_X			'\030'

#define CONTROL_B_ENABLE		(0x1)
#define CONTROL_G_ENABLE		(0x2)
#define CONTROL_U_ENABLE		(0x4)
#define CONTROL_X_ENABLE		(0x8)
#define INTERACTIVE_INTERRUPT_ENABLE	(0x10)
#define TERMINATE_INTERRUPT_ENABLE	(0x20)

/* This is a table and also a null terminated string. */
unsigned char keyboard_interrupt_table[KB_INT_TABLE_SIZE];
static unsigned char keyboard_interrupt_enables;

void
OS_ctty_get_interrupt_enables (Tinterrupt_enables * mask)
{
  *mask = ((Tinterrupt_enables) keyboard_interrupt_enables);
  return;
}

void
OS_ctty_set_interrupt_enables (Tinterrupt_enables * mask)
{
  /* Kludge: ctl-break always enabled. */
  keyboard_interrupt_enables = (((unsigned char) (* mask))
				| TERMINATE_INTERRUPT_ENABLE);
  return;
}

/* This is a temporary kludge. */

#define NUM_INT_CHANNELS 6

static cc_t int_chars[NUM_INT_CHANNELS];
static cc_t int_handlers[NUM_INT_CHANNELS];

#define SCREEN_COMMAND_INTERRUPT_FIRST (SCREEN_COMMAND_CLOSE+10)

int signal_keyboard_character_interrupt (int);

LRESULT
master_tty_interrupt (HWND tty, WORD command)
{
  int ch = int_chars[command - SCREEN_COMMAND_INTERRUPT_FIRST];
  return (signal_keyboard_character_interrupt (ch));
}

static void
update_interrupt_characters (void)
{
  int i;

  for (i = 0; i < KB_INT_TABLE_SIZE; i++)
  {
    keyboard_interrupt_table[i] = NO_INTERRUPT_CHAR;
    SendMessage (master_tty_window, SCREEN_SETBINDING, i, 0);
  }

  for (i = 0; i < NUM_INT_CHANNELS; i++)
  {
    unsigned char handler;

    switch (int_handlers[i])
    {
      case interrupt_handler_control_b:
        handler = CONTROL_B_INTERRUPT_CHAR;
	break;

      case interrupt_handler_control_g:
        handler = CONTROL_G_INTERRUPT_CHAR;
	break;

      case interrupt_handler_control_u:
        handler = CONTROL_U_INTERRUPT_CHAR;
	break;

      case interrupt_handler_control_x:
        handler = CONTROL_X_INTERRUPT_CHAR;
	break;

      case interrupt_handler_interactive:
        handler = INTERACTIVE_INTERRUPT_CHAR;
	break;

      case interrupt_handler_terminate:
	handler = TERMINATE_INTERRUPT_CHAR;
	break;

      default:
        handler = NO_INTERRUPT_CHAR;
	break;
    }
    keyboard_interrupt_table[(int) (int_chars[i])] = handler;

    SendMessage (master_tty_window,
		 SCREEN_SETCOMMAND,
                 (SCREEN_COMMAND_INTERRUPT_FIRST + i),
		 (LPARAM) master_tty_interrupt);

    SendMessage (master_tty_window,
		 SCREEN_SETBINDING,
                 int_chars[i],
		 (SCREEN_COMMAND_INTERRUPT_FIRST + i));
  }
  return;
}

unsigned int
OS_ctty_num_int_chars (void)
{
  return (NUM_INT_CHANNELS);
}

cc_t *
OS_ctty_get_int_chars (void)
{
  return (&int_chars[0]);
}

void
OS_ctty_set_int_chars (cc_t * new_int_chars)
{
  int i;

  for (i = 0; i < NUM_INT_CHANNELS; i++)
    int_chars[i] = new_int_chars[i];
  update_interrupt_characters ();
  return;
}

cc_t *
OS_ctty_get_int_char_handlers (void)
{
  return (&int_handlers[0]);
}

void
OS_ctty_set_int_char_handlers (cc_t * new_int_handlers)
{
  int i;

  for (i = 0; i < NUM_INT_CHANNELS; i++)
    int_handlers[i] = new_int_handlers[i];
  update_interrupt_characters ();
  return;
}

static void
console_write_string (unsigned char * string)
{
  outf_console ("%s", string);
  outf_flush_console ();
  return;
}

static void
initialize_keyboard_interrupt_table (void)
{
  /* Set up default interrupt characters */
  int_chars[0] = CONTROL_B;
  int_handlers[0] = ((unsigned char) interrupt_handler_control_b);
  int_chars[1] = CONTROL_G;
  int_handlers[1] = ((unsigned char) interrupt_handler_control_g);
  int_chars[2] = CONTROL_U;
  int_handlers[2] = ((unsigned char) interrupt_handler_control_u);
  int_chars[3] = CONTROL_X;
  int_handlers[3] = ((unsigned char) interrupt_handler_control_x);
  int_chars[4] = CONTROL_C;
  int_handlers[4] = ((unsigned char) interrupt_handler_interactive);
  int_chars[5] = CONTROL_BREAK;
  int_handlers[5] = ((unsigned char) interrupt_handler_terminate);
  keyboard_interrupt_enables =
    (CONTROL_B_ENABLE | CONTROL_G_ENABLE
     | CONTROL_U_ENABLE | CONTROL_X_ENABLE
     | INTERACTIVE_INTERRUPT_ENABLE
     | TERMINATE_INTERRUPT_ENABLE);
  update_interrupt_characters ();
  return;
}

static int hard_attn_limit = 2;
static int hard_attn_counter = 0;

cc_t
OS_tty_map_interrupt_char (cc_t int_char)
{
  /* Scheme got a keyboard interrupt, reset the hard attention counter. */
  hard_attn_counter = 0;
  return (int_char);
}

static void
print_interrupt_help (void)
{
  console_write_string ("\r\nInterrupt choices are:\r\n"
    "C-G interrupt:   ^G (abort to top level)\r\n"
    "C-X interrupt:   ^x (abort)\r\n"
    "C-B interrupt:   ^B (break)\r\n"
    "C-U interrupt:   ^U (up)\r\n"
    "(exit) to exit Scheme\r\n"
    );

  return;
}

extern void tty_set_next_interrupt_char (cc_t);

#define REQUEST_INTERRUPT_IF_ENABLED(mask) do				\
{									\
  if (keyboard_interrupt_enables & (mask))				\
  {									\
    tty_set_next_interrupt_char ((cc_t) interrupt_char);		\
    interrupt_p = 1;							\
  }									\
  else									\
    interrupt_p = 0;							\
} while (0)

int
signal_keyboard_character_interrupt (int c)
{
  if (c == -1)
  {
    if (keyboard_interrupt_enables & TERMINATE_INTERRUPT_ENABLE)
      goto interactive_interrupt;
    else
      return (0);
  }
  if (c == -2)
  {
    /* Special kludge for hard attn. */
    if (keyboard_interrupt_enables & TERMINATE_INTERRUPT_ENABLE)
    {
      hard_attn_counter += 1;
      if (hard_attn_counter >= hard_attn_limit)
      {
	console_write_string ("\nTerminating scheme!");
	termination_normal (0);
      }
      tty_set_next_interrupt_char (CONTROL_G_INTERRUPT_CHAR);
    }
    return (0);
  }

  else if ((c >= 0) && (c < KB_INT_TABLE_SIZE))
  {
    int interrupt_p, interrupt_char;

    interrupt_char = keyboard_interrupt_table[c];

    switch (interrupt_char)
    {
      case CONTROL_B_INTERRUPT_CHAR:
	REQUEST_INTERRUPT_IF_ENABLED (CONTROL_B_ENABLE);
	break;

      case CONTROL_G_INTERRUPT_CHAR:
	REQUEST_INTERRUPT_IF_ENABLED (CONTROL_G_ENABLE);
	break;

      case CONTROL_U_INTERRUPT_CHAR:
	REQUEST_INTERRUPT_IF_ENABLED (CONTROL_U_ENABLE);
	break;

      case CONTROL_X_INTERRUPT_CHAR:
	REQUEST_INTERRUPT_IF_ENABLED (CONTROL_X_ENABLE);
	break;

      case INTERACTIVE_INTERRUPT_CHAR:
	if (! (keyboard_interrupt_enables & INTERACTIVE_INTERRUPT_ENABLE))
	{
	  interrupt_p = 0;
	  break;
	}
interactive_interrupt:
	print_interrupt_help ();
	interrupt_p = 1;
	break;

      default:
	interrupt_p = 0;
    }
    return (interrupt_p);
  }
  return (0);
}

void
OS_restartable_exit (void)
{
  return;
}

/* System-level timer interrupt */

/* INT_Global_GC: High-priority Windows polling interrupt.
   INT_Global_1:  Windows polling interrupt.
 */

#define CATATONIA_PERIOD       120000	/* msec */
#define ASYNC_TIMER_PERIOD	50	/* msec */

static void * timer_state = ((void *) NULL);
extern unsigned long * win32_catatonia_block;

static char *
install_timer (void)
{
  /* This presumes that the catatonia block is allocated near
     the register block and locked in physical memory with it.
   */

  long catatonia_offset
    = (((SCHEME_OBJECT *) &win32_catatonia_block[0]) - (&Registers[0]));

  win32_catatonia_block[CATATONIA_BLOCK_COUNTER] = 0;
  win32_catatonia_block[CATATONIA_BLOCK_LIMIT]
    = (CATATONIA_PERIOD / ASYNC_TIMER_PERIOD);
  win32_catatonia_block[CATATONIA_BLOCK_FLAG] = 0;
  switch (win32_system_utilities.install_async_timer
	  (&timer_state,
	   &Registers[0],
	   REGBLOCK_MEMTOP,
	   REGBLOCK_INT_CODE,
	   REGBLOCK_INT_MASK,
	   (INT_Global_GC | INT_Global_1),
	   catatonia_offset,
	   WM_CATATONIC,
	   WM_SCHEME_INTERRUPT,
	   master_tty_window,
	   OS_grab_interrupt_registers,
	   OS_release_interrupt_registers))
  {
    case WIN32_ASYNC_TIMER_OK:
      return (NULL);

    case WIN32_ASYNC_TIMER_NONE:
      return ("No asynchronous timer facilities available");

    case WIN32_ASYNC_TIMER_EXHAUSTED:
      return ("No asynchronous timers available");

    case WIN32_ASYNC_TIMER_RESOLUTION:
      return ("Wrong asynchronous timer resolution");

    case WIN32_ASYNC_TIMER_NOLOCK:
      return ("Unable to lock the system timer interrupt handler");

    case WIN32_ASYNC_TIMER_NOMEM:
      return ("Not enough memory to install the timer interrupt handler");

    case WIN32_ASYNC_TIMER_NOLDT:
      return ("Not enough selectors to fix the timer interrupt handler");

    default:
      return ("Unknown asynchronous timer return code");
  }
}

static void
flush_timer (void)
{
  win32_system_utilities.flush_async_timer (timer_state);
  return;
}

/* This sets up the interrupt handlers for both DOS and NT,
   so that bands can be shared.
 */

void
NT_initialize_fov (SCHEME_OBJECT fov)
{
  int ctr, in;
  SCHEME_OBJECT iv, imv, prim;
  static int interrupt_numbers[2] =
  {
    Global_GC_Level,
    Global_1_Level,
  };
  static long interrupt_masks[2] =
  {
    0,				/* No interrupts allowed */
    (INT_Stack_Overflow | INT_Global_GC | INT_GC),
  };

  iv = (VECTOR_REF (fov, SYSTEM_INTERRUPT_VECTOR));
  imv = (VECTOR_REF (fov, FIXOBJ_INTERRUPT_MASK_VECTOR));
  prim = (make_primitive ("MICROCODE-POLL-INTERRUPT-HANDLER", 2));

  for (ctr = 0; ctr < ((sizeof (interrupt_numbers)) / (sizeof (int))); ctr++)
  {
    in = interrupt_numbers[ctr];
    VECTOR_SET (iv, in, prim);
    VECTOR_SET (imv, in, (long_to_integer (interrupt_masks[ctr])));
  }
  return;
}

void
NT_initialize_signals (void)
{
  char * timer_error;

  initialize_keyboard_interrupt_table ();
  timer_error = (install_timer ());
  if (timer_error)
  {
    outf_fatal ("install_timer:  %s", timer_error);
    outf_flush_fatal ();
    abort ();
  }
  return;
}

extern void NT_restore_signals (void);

void
NT_restore_signals (void)
{
  flush_timer ();
  return;
}
