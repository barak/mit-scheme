/* -*-C-*-

$Id: ntsig.c,v 1.6 1993/07/27 21:00:54 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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


/* Hacks by SRA for NT:
    1. punt interactive debugging completely
*/

#include "scheme.h"
#include "nt.h"
#include <signal.h>
/*#include <int.h> SRA*/
#include "ossig.h"
#include "osctty.h"
#include "ostty.h"
#include "critsec.h"
/*#include <bios.h> SRA*/
#include "ntsys.h"
#include "ntio.h"
#include "ntexcp.h"
#include "ntkbd.h"
#ifdef USE_ZORTECH_CERROR
#include <cerror.h>
#endif
#include "extern.h"
#include "ntutil.h"
#include "ntscreen.h"
#include "ntscmlib.h"

#ifndef fileno
#define fileno(fp)	((fp)->_file)
#endif

cc_t EXFUN (DOS_interactive_interrupt_handler, (void));

/* Signal Manipulation */

#ifdef UNUSED

static Tsignal_handler
DEFUN (current_handler, (signo), int signo)
{
  Tsignal_handler result = (DOS_signal (signo, SIG_IGN));
  if (result != SIG_IGN)
    DOS_signal (signo, result);
  return (result);
}
#endif /* UNUSED */

#define INSTALL_HANDLER DOS_signal
#define NEED_HANDLER_TRANSACTION

#define ENTER_HANDLER(signo)
#define ABORT_HANDLER DOS_signal
#define EXIT_HANDLER DOS_signal


/* These could be implemented, at least under DPMI by examining
   and setting the virtual interrupt state.
 */

void
DEFUN_VOID (preserve_signal_mask)
{
  return;
}

void
DEFUN_VOID (block_signals)
{
  return;
}

void
DEFUN_VOID (unblock_signals)
{
  return;
}

#ifdef UNUSED
/* Signal Descriptors */

enum dfl_action { dfl_terminate, dfl_ignore, dfl_stop };

struct signal_descriptor
{
  int signo;
  CONST char * name;
  enum dfl_action action;
  int flags;
};

/* `flags' bits */
#define NOIGNORE 1
#define NOBLOCK 2
#define NOCATCH 4
#define CORE_DUMP 8

static struct signal_descriptor * signal_descriptors;
static unsigned int signal_descriptors_length;
static unsigned int signal_descriptors_limit;

static void
DEFUN (defsignal, (signo, name, action, flags),
       int signo AND
       CONST char * name AND
       enum dfl_action action AND
       int flags)
{
  if (signo == 0)
    return;
  if (signal_descriptors_length == signal_descriptors_limit)
    {
      signal_descriptors_limit += 8;
      signal_descriptors =
	(DOS_realloc (signal_descriptors,
		     (signal_descriptors_limit *
		      (sizeof (struct signal_descriptor)))));
      if (signal_descriptors == 0)
	{
	  outf_fatal ("\nUnable to grow signal definitions table.\n");
	  termination_init_error ();
	}
    }
  {
    struct signal_descriptor * sd =
      (signal_descriptors + (signal_descriptors_length++));
    (sd -> signo) = signo;
    (sd -> name) = name;
    (sd -> action) = action;
    (sd -> flags) = flags;
  }
}

static struct signal_descriptor *
DEFUN (find_signal_descriptor, (signo), int signo)
{
  struct signal_descriptor * scan = signal_descriptors;
  struct signal_descriptor * end = (scan + signal_descriptors_length);
  for (; (scan < end); scan += 1)
    if ((scan -> signo) == signo)
      return (scan);
  return (0);
}

CONST char *
DEFUN (find_signal_name, (signo), int signo)
{
  static char buffer [32];
  struct signal_descriptor * descriptor = (find_signal_descriptor (signo));
  if (descriptor != 0)
    return (descriptor -> name);
  sprintf (buffer, "unknown signal %d", signo);
  return ((CONST char *) buffer);
}

#define OS_SPECIFIC_SIGNALS()

#if (SIGABRT == SIGIOT)
#undef SIGABRT
#define SIGABRT 0
#endif

static void
DEFUN_VOID (initialize_signal_descriptors)
{
  signal_descriptors_length = 0;
  signal_descriptors_limit = 32;
  signal_descriptors =
    (DOS_malloc (signal_descriptors_limit *
		 (sizeof (struct signal_descriptor))));
  if (signal_descriptors == 0)
    {
      outf_error ("\nUnable to allocate signal definitions table.\n");
      termination_init_error ();
    }

  defsignal (SIGINT, "SIGINT",		dfl_terminate,	0);
  defsignal (SIGILL, "SIGILL",		dfl_terminate,	CORE_DUMP);
  defsignal (SIGFPE, "SIGFPE",		dfl_terminate,	CORE_DUMP);
  defsignal (SIGSEGV, "SIGSEGV",	dfl_terminate,	CORE_DUMP);
  defsignal (SIGTERM, "SIGTERM",	dfl_terminate,	0);
  defsignal (SIGABRT, "SIGABRT",	dfl_terminate,	CORE_DUMP);

  OS_SPECIFIC_SIGNALS ();
}
#endif

/* Signal Handlers */

struct handler_record
{
  int signo;
  Tsignal_handler handler;
};

#define DEFUN_STD_HANDLER(name, statement)				\
static Tsignal_handler_result						\
DEFUN (name, (signo), int signo)					\
{									\
  int STD_HANDLER_abortp;						\
  ENTER_HANDLER (signo);						\
  STD_HANDLER_abortp = (enter_interruption_extent ());			\
  transaction_begin ();							\
  {									\
    struct handler_record * record =					\
      (dstack_alloc (sizeof (struct handler_record)));			\
    (record -> signo) = signo;						\
    (record -> handler) = 0;						\
    transaction_record_action (tat_abort, ta_abort_handler, record);	\
  }									\
  statement;								\
  if (STD_HANDLER_abortp)						\
    {									\
      transaction_abort ();						\
      exit_interruption_extent ();					\
    }									\
  transaction_commit ();						\
  EXIT_HANDLER (signo, name);						\
  SIGNAL_HANDLER_RETURN ();						\
}


static void
DEFUN (ta_abort_handler, (ap), PTR ap)
{
  ABORT_HANDLER ((((struct handler_record *) ap) -> signo),
		 (((struct handler_record *) ap) -> handler));
}
#ifdef UNUSED
#endif /* UNUSED */

#define CONTROL_B_INTERRUPT_CHAR	'B'
#define CONTROL_G_INTERRUPT_CHAR	'G'
#define CONTROL_U_INTERRUPT_CHAR	'U'
#define CONTROL_X_INTERRUPT_CHAR	'X'
#define INTERACTIVE_INTERRUPT_CHAR	'!'
#define TERMINATE_INTERRUPT_CHAR	'@'
#define NO_INTERRUPT_CHAR		'0'


static void
DEFUN (echo_keyboard_interrupt, (c, dc), cc_t c AND cc_t dc)
{
  c &= 0177;
  if (c == ALERT_CHAR)
    outf_console ("%c", c);
  else if (c < '\040')
    outf_console ("^%c", (c+'@'));
  else if (c == '\177')
    outf_console ("^?");
  else
    outf_console ("%c", c);
  outf_flush_console();
}

DEFUN_STD_HANDLER (sighnd_control_g,
  {
    tty_set_next_interrupt_char (CONTROL_G_INTERRUPT_CHAR);
  })

DEFUN_STD_HANDLER (sighnd_control_c,
  {
    cc_t int_char;

    int_char = (DOS_interactive_interrupt_handler ());
    if (int_char != ((cc_t) 0))
      tty_set_next_interrupt_char (int_char);
  })


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
DEFUN (OS_ctty_get_interrupt_enables, (mask), Tinterrupt_enables * mask)
{
  *mask = ((Tinterrupt_enables) keyboard_interrupt_enables);
  return;
}

void
DEFUN (OS_ctty_set_interrupt_enables, (mask), Tinterrupt_enables * mask)
{
  /* Kludge: ctl-break always enabled. */
  keyboard_interrupt_enables = (((unsigned char) (*mask))
				| TERMINATE_INTERRUPT_ENABLE);
  return;
}

/* This is a temporary kludge. */

#define NUM_INT_CHANNELS 6
static cc_t int_chars[NUM_INT_CHANNELS];
static cc_t int_handlers[NUM_INT_CHANNELS];

#define SCREEN_COMMAND_INTERRUPT_FIRST (SCREEN_COMMAND_CLOSE+10)

LRESULT   master_tty_interrupt (HWND tty, WORD command)
{
    int  ch = int_chars[command - SCREEN_COMMAND_INTERRUPT_FIRST];
    return (signal_keyboard_character_interrupt (ch));
}

static void
DEFUN_VOID (update_interrupt_characters)
{
  extern HANDLE master_tty_window;
  int i;

  for (i = 0; i < KB_INT_TABLE_SIZE; i++) {
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
    SendMessage (master_tty_window, SCREEN_SETCOMMAND,
                 SCREEN_COMMAND_INTERRUPT_FIRST+i,
		 (LPARAM) master_tty_interrupt);
    SendMessage (master_tty_window, SCREEN_SETBINDING,
                 int_chars[i], SCREEN_COMMAND_INTERRUPT_FIRST+i);
  }
  return;
}

unsigned int
DEFUN_VOID (OS_ctty_num_int_chars)
{
  return (NUM_INT_CHANNELS);
}

cc_t *
DEFUN_VOID (OS_ctty_get_int_chars)
{
  return (&int_chars[0]);
}

void
DEFUN (OS_ctty_set_int_chars, (new_int_chars), cc_t * new_int_chars)
{
  int i;

  for (i = 0; i < NUM_INT_CHANNELS; i++)
    int_chars[i] = new_int_chars[i];
  update_interrupt_characters ();
  return;
}

cc_t *
DEFUN_VOID (OS_ctty_get_int_char_handlers)
{
  return (&int_handlers[0]);
}

void
DEFUN (OS_ctty_set_int_char_handlers, (new_int_handlers),
       cc_t * new_int_handlers)
{
  int i;

  for (i = 0; i < NUM_INT_CHANNELS; i++)
    int_handlers[i] = new_int_handlers[i];
  update_interrupt_characters ();
  return;
}

extern long EXFUN (text_write, (int, CONST unsigned char *, size_t));

static void
DEFUN (console_write_string, (string), unsigned char * string)
{
  outf_console ("%s", string);
  outf_flush_console();
  return;
}

static void
DEFUN (console_write_character, (c), unsigned char c)
{
  outf_console ("%c", c);
  outf_flush_console();
  return;
}

static unsigned char
DEFUN_VOID (console_read_character)
{
  return  userio_read_char();
}

void
DEFUN_VOID (initialize_keyboard_interrupt_table)
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
  update_interrupt_characters ();
  keyboard_interrupt_enables =
    (CONTROL_B_ENABLE | CONTROL_G_ENABLE
     | CONTROL_U_ENABLE | CONTROL_X_ENABLE
     | INTERACTIVE_INTERRUPT_ENABLE
     | TERMINATE_INTERRUPT_ENABLE);
  return;
}

static int hard_attn_limit = 2;
static int hard_attn_counter = 0;

cc_t
DEFUN (OS_tty_map_interrupt_char, (int_char), cc_t int_char)
{
  /* Scheme got a keyboard interrupt, reset the hard attention counter. */
  hard_attn_counter = 0;
  return (int_char);
}

static void
DEFUN_VOID (print_interrupt_help)
{
  console_write_string (
    "\r\nInterrupt choices are:\r\n"
    "C-G interrupt:   ^G (abort to top level)\r\n"
    "C-X interrupt:   ^x (abort)\r\n"
    "C-B interrupt:   ^B (break)\r\n"
    "C-U interrupt:   ^U (up)\r\n"
    "(exit) to exit Scheme\r\n"
    );

/*
  console_write_string ("\nInterrupt Choices are:\n");
  console_write_string ("C-G interrupt:    G, g, ^G (abort to top level)\n");
  console_write_string ("C-X interrupt:    X, x, ^x (abort)\n");
  console_write_string ("C-B interrupt:    B, b, ^B (break)\n");
  console_write_string ("C-U interrupt:    U, u, ^U (up)\n");
  console_write_string ("Ignore interrupt: I, i     (dismiss)\n");
  console_write_string ("Reset scheme:     R, r     (hard reset)\n");
  console_write_string ("Quit scheme:      Q, q     (exit)\n");
  console_write_string ("Print help:       ?");
*/
  return;
}

#define REQUEST_INTERRUPT_IF_ENABLED(mask) do				\
{									\
  if (keyboard_interrupt_enables & (mask))				\
  {									\
    tty_set_next_interrupt_char (interrupt_char);			\
    interrupt_p = 1;							\
  }									\
  else									\
    interrupt_p = 0;							\
} while (0)

int EXFUN (signal_keyboard_character_interrupt, (int));

int
DEFUN (signal_keyboard_character_interrupt, (c), int c)
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
      goto interactive_interrupt;
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
	{
	  cc_t int_char;

	  /*int_char = (DOS_interactive_interrupt_handler ());*/
	  print_interrupt_help();
	  int_char = 0;
	  
	  if (int_char == ((cc_t) 0))
	    hard_attn_counter = 0;
	  else
	  {
	    tty_set_next_interrupt_char ((int) int_char);
	    interrupt_p = 1;
	  }
	}
	break;

      default:
	interrupt_p = 0;
    }
    return (interrupt_p);
  }
  return (0);
}

cc_t
DEFUN_VOID (DOS_interactive_interrupt_handler)
{
  while (1)
  {
    unsigned char response;

    console_write_string
      ("\nKeyboard interrupt, type character (? for help): ");

    response = (console_read_character ());
    console_write_character (response);

    switch (response)
    {
      case 'b':
      case 'B':
      case CONTROL_B:
	return CONTROL_B_INTERRUPT_CHAR;

      case 'g':
      case 'G':
      case CONTROL_G:
	return CONTROL_G_INTERRUPT_CHAR;

      case 'i':
      case 'I':
	return ((cc_t) 0);

      case 'R':
      case 'r':
      {
	extern void EXFUN (soft_reset, (void));
	soft_reset ();
	/*NOTREACHED*/
      }

      case 'q':
      case 'Q':
      {
	console_write_string ("\nTerminate scheme (y or n)? ");
	response = (console_read_character ());
	console_write_character (response);
	if ((response == 'y') || (response == 'Y'))
	{
	  console_write_string ("\n");
	  termination_normal (0);
	}
	print_interrupt_help ();
	break;
      }

      case 'u':
      case 'U':
      case CONTROL_U:
	return CONTROL_U_INTERRUPT_CHAR;

      case 'x':
      case 'X':
      case CONTROL_X:
	return CONTROL_X_INTERRUPT_CHAR;

      case '?':
	print_interrupt_help ();
	break;

      default:
      {
	unsigned char temp[128];

	sprintf (temp, "\nIllegal interrupt character: [%c]", response);
	console_write_string (temp);
	print_interrupt_help ();
	break;
      }
    }
 }
}

void
DEFUN_VOID (OS_restartable_exit)
{
  return;
}

/* Timer interrupt */

/* Why does this raise INT_Timer as well?
   We could request an synchronous Windows timer that would trigger
   the timer interrupt bit.

   INT_Global_1: Windows polling interrupt
   INT_Timer:    Scheme timer interrupt
 */

static void * timer_state = ((void *) NULL);

DEFUN_VOID (install_timer)
{
  switch (win32_install_async_timer (&Registers[REGBLOCK_INT_CODE],
				     &Registers[REGBLOCK_INT_MASK],
				     &Registers[REGBLOCK_MEMTOP],
				     (INT_Global_1 | INT_Timer),
				     &timer_state))
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

    default:
      return ("Unknown asynchronous timer return code");
  }
}

static void
DEFUN_VOID (flush_timer)
{
  win32_flush_async_timer (timer_state);
  return;
}

/* This sets up the interrupt handlers for both DOS and NT,
   so that bands can be shared.
 */

void
DEFUN (NT_initialize_fov, (fov), SCHEME_OBJECT fov)
{
  int ctr, in;
  SCHEME_OBJECT iv, imv, prim, mask;
  extern SCHEME_OBJECT EXFUN (make_primitive, (char *));
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

  iv = (FAST_VECTOR_REF (fov, System_Interrupt_Vector));
  imv = (FAST_VECTOR_REF (fov, FIXOBJ_INTERRUPT_MASK_VECTOR));
  prim = (make_primitive ("MICROCODE-POLL-INTERRUPT-HANDLER"));

  for (ctr = 0; ctr < ((sizeof (interrupt_numbers)) / (sizeof (int))); ctr++)
  {
    in = interrupt_numbers[ctr];
    VECTOR_SET (iv, in, prim);
    VECTOR_SET (imv, in, (long_to_integer (interrupt_masks[ctr])));
  }
  return;
}

void
DEFUN_VOID (NT_initialize_signals)
{
  char * timer_error = (install_timer ());

  if (timer_error)
  {
    outf_fatal ("install_timer:  %s", timer_error);
    outf_flush_fatal ();
    abort ();
  }	
  return;
}

extern void EXFUN (NT_restore_signals, (void));

void
DEFUN_VOID (NT_restore_signals)
{
  flush_timer ();
  return;
}
