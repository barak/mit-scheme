/* -*-C-*-

$Id: dossig.c,v 1.12 1992/10/07 06:23:32 jinx Exp $

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

#include "scheme.h"
#include "msdos.h"
#include <signal.h>
#include <int.h>
#include "ossig.h"
#include "osctty.h"
#include "ostty.h"
#include "critsec.h"
#include <bios.h>
#include "dossys.h"
#include "dosexcp.h"
#include "doskbd.h"
#ifdef USE_ZORTECH_CERROR
#include <cerror.h>
#endif

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

#define INSTALL_HANDLER DOS_signal
#define NEED_HANDLER_TRANSACTION

#define ENTER_HANDLER(signo)
#define ABORT_HANDLER DOS_signal
#define EXIT_HANDLER DOS_signal

#endif /* UNUSED */

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
	  fprintf (stderr, "\nUnable to grow signal definitions table.\n");
	  fflush (stderr);
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
      fprintf (stderr, "\nUnable to allocate signal definitions table.\n");
      fflush (stderr);
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
#endif /* UNUSED */

#define CONTROL_B_INTERRUPT_CHAR	'B'
#define CONTROL_G_INTERRUPT_CHAR	'G'
#define CONTROL_U_INTERRUPT_CHAR	'U'
#define CONTROL_X_INTERRUPT_CHAR	'X'
#define INTERACTIVE_INTERRUPT_CHAR	'!'
#define TERMINATE_INTERRUPT_CHAR	'@'
#define NO_INTERRUPT_CHAR		'0'

#ifdef UNUSED
static void
DEFUN (echo_keyboard_interrupt, (c, dc), cc_t c AND cc_t dc)
{
  c &= 0177;
  if (c == ALERT_CHAR)
    putc (c, stdout);
  else if (c < '\040')
    {
      putc ('^', stdout);
      putc ((c + '@'), stdout);
    }
  else if (c == '\177')
    fputs ("^?", stdout);
  else
    putc (c, stdout);
  fflush (stdout);
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

#endif /* UNUSED */

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

static void
DEFUN_VOID (update_interrupt_characters)
{
  int i;

  for (i = 0; i < KB_INT_TABLE_SIZE; i++)
    keyboard_interrupt_table[i] = NO_INTERRUPT_CHAR;

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
  (void) text_write ((fileno (stdout)), string, (strlen (string)));
  return;
}

static void
DEFUN (console_write_character, (c), unsigned char c)
{
  (void) text_write ((fileno (stdout)), &c, 1);
  return;
}

static unsigned char
DEFUN_VOID (console_read_character)
{
  unsigned char c;
  extern int EXFUN (dos_read, (int, PTR, size_t, int, int));

  /* non-buffered, non-blocking read. */
  (void) dos_read ((fileno (stdin)), &c, 1, 0, 0);
  return (c);
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
  console_write_string ("\nInterrupt Choices are:\n");
  console_write_string ("C-G interrupt:    G, g, ^G (abort to top level)\n");
  console_write_string ("C-X interrupt:    X, x, ^x (abort)\n");
  console_write_string ("C-B interrupt:    B, b, ^B (break)\n");
  console_write_string ("C-U interrupt:    U, u, ^U (up)\n");
  console_write_string ("Ignore interrupt: I, i     (dismiss)\n");
  console_write_string ("Reset scheme:     R, r     (hard reset)\n");
  console_write_string ("Quit scheme:      Q, q     (exit)\n");
  console_write_string ("Print help:       ?");
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

	  int_char = (DOS_interactive_interrupt_handler ());
	  if (int_char != ((cc_t) 0))
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

#ifdef UNUSED

#define IF_POSIX_SIGNALS(code) do {} while (0)

DEFUN_STD_HANDLER (sighnd_stop, {})

#ifdef HAVE_ITIMER

DEFUN_STD_HANDLER (sighnd_timer,
  {
    request_timer_interrupt ();
  })

#else /* not HAVE_ITIMER */

extern void EXFUN (reschedule_alarm, (void));

DEFUN_STD_HANDLER (sighnd_timer,
  {
    /* reschedule_alarm ();
       request_timer_interrupt ();
     */
  })

#endif /* HAVE_ITIMER */

DEFUN_STD_HANDLER (sighnd_save_then_terminate,
  (request_suspend_interrupt ()))

#ifndef SIGNUP
#define SIGHUP 999
#endif

DEFUN_STD_HANDLER (sighnd_terminate,
  (termination_signal
   ((! (option_emacs_subprocess && (signo == SIGHUP)))
    ? (find_signal_name (signo))
    : 0)))

#define VOID ((struct sigcontext *) 0)

DEFUN_STD_HANDLER (sighnd_fpe,
  {
    if (executing_scheme_primitive_p ())
      error_floating_point_exception ();
    trap_handler ("floating-point exception signal", signo, VOID, VOID);
  })

DEFUN_STD_HANDLER (sighnd_hardware_trap,
  (trap_handler ("hardware fault signal", signo, VOID, VOID)))

DEFUN_STD_HANDLER (sighnd_software_trap,
  (trap_handler ("system software fault signal", signo, VOID, VOID)))


/* When a child process terminates, it becomes a zombie until its
   parent process calls one of the wait() routines to obtain the
   child's termination status.  The SIGCHLD handler must always call
   wait() or waitpid() to permit the child process's resources to be
   freed. */

/* On systems with waitpid() (i.e. those that support WNOHANG) we must
   loop until there are no more processes, because some of those
   systems may deliver only one SIGCHLD when more than one child
   terminates.  Systems without waitpid() (e.g. _SYSV) typically
   provide queuing of SIGCHLD such that one SIGCHLD is delivered for
   every child that terminates.  Systems that provide neither
   waitpid() nor queuing are so losing that we can't win, in which
   case we just hope that child terminations don't happen too close to
   one another to cause problems. */

DEFUN_STD_HANDLER (sighnd_dead_subprocess,
  {
  })
#endif /* UNUSED */

/* PC specific low-level interrupt hooks. */
/* Control-Break Interrupt. */

int
DEFUN (control_break_handler, (pd), struct INT_DATA * pd)
{
  tty_set_next_interrupt_char (CONTROL_G_INTERRUPT_CHAR);
  return (INTERRUPT_RETURN);
}

/* Critical-Error (abort, retry, ignore, fail) handler */

#define CE_CAN_ERROR_BIT	0x1000
#define CE_CAN_RETRY_BIT	0x0800
#define CE_CAN_IGNORE_BIT	0x0400

#define CE_IGNORE		0
#define CE_RETRY		1
#define CE_KILL			2
#define CE_ERROR		3

int
ce_handler (int * ax, int * di)
{  
  if (((* ax) & CE_CAN_ERROR_BIT) != 0)
    * ax = (((* ax) & 0xff00) | CE_ERROR);

  else if (((* ax) & CE_CAN_IGNORE_BIT) != 0)
    * ax = (((* ax) & 0xff00) | CE_IGNORE);

  else if (((* ax) & CE_CAN_RETRY_BIT) != 0)
    * ax = (((* ax) & 0xff00) | CE_RETRY);

  else
    /* We should really kill Scheme,
       but there may be no way to do this from here.
     */
    * ax = (((* ax) & 0xff00) | CE_KILL);

  return (1);
}

#ifdef USE_ZORTECH_CERROR

int _far _cdecl 
critical_error_handler (int * ax, int * di)
{
  return (ce_handler (ax, di));
}

#else /* not USE_ZORTECH_CERROR */

int
DEFUN (critical_error_handler, (pd), struct INT_DATA * pd)
{
  int value = (ce_handler (&pd->regs.e.eax, &pd->regs.e.edi));
  return ((value == 1) ? INTERRUPT_RETURN : INTERRUPT_CHAIN_NEXT);
}

#endif /* USE_ZORTECH_CERROR */

/* Interval timer */

/* Scheme timer emulation; DOS does not have an ITIMER like unix. */
/* Zero means timer is not set or has expired. */

extern unsigned long scm_itimer_counter;
extern unsigned long scm_itimer_reload;

unsigned long scm_itimer_counter = 0;
unsigned long scm_itimer_reload = 0;

extern void EXFUN (dos_process_timer_interrupt, (void));

void
DEFUN_VOID (dos_process_timer_interrupt)
{
  if (scm_itimer_counter != 0)
  {
    if (--scm_itimer_counter == 0)
    { 
      scm_itimer_counter = scm_itimer_reload;
      request_timer_interrupt ();
    }
  }
  return;
}

extern int EXFUN (bios_timer_handler, (struct INT_DATA *));

int 
DEFUN (bios_timer_handler, (pd), struct INT_DATA *pd)
{
#if 0
  dos_process_timer_interrupt ();
#else
  /* This is a kludge for DOS.
     Reuse INT_Global_GC as a high-priority interrupt from
     which the keyboard interrupt and real timer interrupt are
     derived.
   */
  REQUEST_INTERRUPT (INT_Global_GC);
#endif
  return (INTERRUPT_CHAIN_NEXT);
}

static Boolean
  dos_interrupts_initialized_p = false,
  ctrl_c_check_flag = true;

dos_boolean DOS_keyboard_intercepted_p = false;

#define NUM_DOS_INTVECT		(MAX_DOS_INTVECT + 1)
#define NUM_DOS_HANDLERS	(NUM_DOS_INTVECT + NUM_DOS_EXCP)
static int EXFUN ((* (dos_interrupt_restoration[NUM_DOS_HANDLERS])), 
                  (unsigned));

static void
DEFUN (dos_record_interrupt_interception, (intno, restorer),
       unsigned intno AND int ((*restorer) (unsigned)))
{
  dos_interrupt_restoration[intno] = restorer;
  return;
}

static int
DEFUN (scm_int_restore, (iv), unsigned iv)
{
  int_restore (iv);
  return (DOS_SUCCESS);		/* A big lie. */
}

static int 
DEFUN (scm_int_intercept, (iv, proc, stack),
       unsigned iv AND int (*proc)(struct INT_DATA *) AND unsigned stack)
{
  if ((int_intercept (iv, proc, stack)) != 0)
    return (DOS_FAILURE);
    
  dos_record_interrupt_interception (iv, scm_int_restore);
  return (DOS_SUCCESS);
}

static void
DEFUN_VOID (DOS_initialize_interrupts)
{
  int iv;
  
#ifdef USE_ZORTECH_CERROR
  _cerror_handler = ((int _far _cdecl (*) (int *, int *)) NULL);
#endif

  ctrl_c_check_flag = (dos_set_ctrl_c_check_flag (0));
  
  for (iv = (NUM_DOS_HANDLERS - 1); iv >= 0; iv--)
    dos_interrupt_restoration[iv] = ((int (*) (unsigned)) NULL);

  dos_interrupts_initialized_p = true;
  return;
} 

extern int EXFUN (DPMI_free_scheme_stack, (unsigned short));
extern int EXFUN (DPMI_alloc_scheme_stack,
		  (unsigned short *, unsigned short *, unsigned long));

extern unsigned short Scheme_Stack_Segment_Selector;
extern unsigned short scheme_ss, scheme_ds;
unsigned short scheme_ds = 0;
unsigned short scheme_ss = 0;

static char i386_exceptions_to_handle[] =
{
  DOS_EXCP_Stack_exception,	/* Must be first */
  DOS_EXCP_Integer_divide_by_zero,
  DOS_EXCP_Debug_exception,
  DOS_EXCP_Breakpoint,
  DOS_EXCP_Integer_overflow,
  DOS_EXCP_Bounds_check,
  DOS_EXCP_Invalid_opcode,
  DOS_EXCP_Numeric_co_processor_not_available,
  DOS_EXCP_Numeric_co_processor_segment_overrun,
  DOS_EXCP_Invalid_TSS,
  DOS_EXCP_Segment_not_present,
  DOS_EXCP_General_protection,
  DOS_EXCP_Page_Fault,
  DOS_EXCP_Floating_point_exception,
  DOS_EXCP_Alignment_check,
  DOS_INVALID_TRAP
};

static short old_excp_handler_cs[NUM_DOS_EXCP];
static unsigned old_excp_handler_eip[NUM_DOS_EXCP];
static void * stack_exception_fault_stack = ((void *) NULL);

#define STACK_EXCEPTION_STACK_SIZE	2048

static int
DEFUN (restore_exception_handler, (iv, restore),
       unsigned iv
       AND int EXFUN ((* restore), (unsigned, unsigned short, unsigned)))
{
  unsigned excp = (iv - NUM_DOS_INTVECT);

  if (((* restore) (excp,
		    old_excp_handler_cs[excp],
		    old_excp_handler_eip[excp]))
      != DOS_SUCCESS)
    return (DOS_FAILURE);
  if (excp == DOS_EXCP_Stack_exception)
  {
    if (scheme_ss != 0)
    {
      Scheme_Stack_Segment_Selector = scheme_ds;
      DPMI_free_scheme_stack (scheme_ss);
    }
    free (stack_exception_fault_stack);
    stack_exception_fault_stack = ((void *) NULL);
  }
  return (DOS_SUCCESS);
}

/* The following two procedures would not be here if C had lambda */

static int
DEFUN (DPMI_restore_handler, (iv), unsigned iv)
{
  return (restore_exception_handler (iv, DPMI_restore_exception_handler));
}

static int
DEFUN (X32_restore_handler, (iv), unsigned iv)
{
  return (restore_exception_handler (iv, X32_restore_exception_handler));
}

static void
DEFUN (exception_handler, (trapno, trapcode, scp),
       unsigned trapno AND unsigned trapcode AND struct sigcontext * scp)
{
  trap_handler ("hardware exception", ((int) trapno), trapcode, scp);
  /*NOTREACHED*/
}

static void
DEFUN (DPMI_stack_fault_handler, (trapno, trapcode, scp),
       unsigned trapno AND unsigned trapcode AND struct sigcontext * scp)
{
  Scheme_Stack_Segment_Selector = scheme_ds;
  if (((scp->sc_ss & 0xffff) == scheme_ss)
      && (scp->sc_esp < (((unsigned long) Stack_Guard) + 0x1000)))
  {
    scp->sc_ss = scheme_ds;
    REQUEST_INTERRUPT (INT_Stack_Overflow);
    return;
  }
  trap_handler ("hardware exception", ((int) trapno), trapcode, scp);
  /*NOTREACHED*/
}

extern void EXFUN (dos386_stack_reset, (void));

void
DEFUN_VOID (dos386_stack_reset)
{
  if (scheme_ss != 0)
    Scheme_Stack_Segment_Selector = scheme_ss;
  return;
}

static void
DEFUN (install_exception_handlers, (get_vector, set_handler, restore),
       int EXFUN ((* get_vector),
		  (unsigned, unsigned short *, unsigned *))
       AND int EXFUN ((* set_handler),
		      (unsigned,
		       void EXFUN ((*),
				   (unsigned,
				    unsigned,
				    struct sigcontext *)),
		       void *))
       AND int EXFUN ((* restore), (unsigned)))
{
  int i;
  char * normal_stack = ((char *) NULL);

  for (i = 0; dos_true ; i++)
  {
    int excp = ((int) i386_exceptions_to_handle[i]);

    if (excp == DOS_INVALID_TRAP)
      break;
    if (((* get_vector) (((unsigned) excp),
			 & old_excp_handler_cs[excp],
			 & old_excp_handler_eip[excp]))
	!= DOS_SUCCESS)
      continue;
    if (excp == DOS_EXCP_Stack_exception)
    {
      void EXFUN ((* handler), (unsigned, unsigned, struct sigcontext *));
      char * stack;

      stack = ((char *) (malloc (2 * STACK_EXCEPTION_STACK_SIZE)));
      if (stack == ((char *) NULL))
	continue;
      handler = exception_handler;
      if ((under_DPMI_p ())
	  && (enable_DPMI_exceptions_p ())
	  && ((DPMI_alloc_scheme_stack (&scheme_ds, &scheme_ss,
					((unsigned long) Stack_Guard)))
	      == DOS_SUCCESS))
      {
	Scheme_Stack_Segment_Selector = scheme_ss;
	handler = DPMI_stack_fault_handler;
	normal_stack = (stack + STACK_EXCEPTION_STACK_SIZE);
      }
      if (((* set_handler) (((unsigned) excp),
			    handler,
			    ((void *) (stack + STACK_EXCEPTION_STACK_SIZE))))
	  != DOS_SUCCESS)
      {
	normal_stack = ((char *) NULL);
	free (stack);
	if (handler != exception_handler)
	{
	  Scheme_Stack_Segment_Selector = scheme_ds;
	  DPMI_free_scheme_stack (scheme_ss);
	  scheme_ss = 0;
	}
	continue;
      }
      stack_exception_fault_stack = ((void *) stack);
    }
    else if (((* set_handler) (((unsigned) excp),
			       exception_handler,
			       ((void *) normal_stack)))
	     != DOS_SUCCESS)
      continue;
    dos_record_interrupt_interception ((excp + NUM_DOS_INTVECT), restore);
  }
  return;
}

/* No lambda! foo. */

static int
DEFUN (DOS_restore_keyboard, (intno), unsigned intno)
{
  if ((dos_restore_kbd_hook ()) != DOS_SUCCESS)
    return (DOS_FAILURE);
  DOS_keyboard_intercepted_p = false;
  return (DOS_SUCCESS);
}     

static dos_boolean
DEFUN_VOID (enable_DPMI_exceptions_p)
{
  extern int strcmp_ci (char *, char *);
  char * envvar = (DOS_getenv ("MITSCHEME_DPMI_EXCEPTIONS"));

  if ((envvar == NULL) || ((strcmp_ci (envvar, "true")) == 0))
    return (dos_true);
  else
    return (dos_false);
}

static void
DEFUN_VOID (DOS_install_interrupts)
{
  extern dos_boolean EXFUN (under_X32_p, (void));
  dos_boolean x32_p = (under_X32_p ());
  dos_boolean dpmi_p = (under_DPMI_p ());
  
  if (x32_p)
  {
    extern void EXFUN (X32_asm_initialize, (void));
    extern int EXFUN (X32_lock_scheme_microcode, (void));
    extern int EXFUN (X32_interrupt_restore, (unsigned));
    extern int EXFUN (X32_int_intercept, (unsigned, void (*) (), PTR));
    extern void EXFUN (X32_timer_interrupt, (void));
    extern void EXFUN (X32_critical_error, (void));
    extern int X32_timer_interrupt_previous;
    extern int X32_critical_error_previous;

    X32_asm_initialize ();

    if ((X32_lock_scheme_microcode ()) != 0)
    {
      fprintf (stderr,
	       "\n;; DOS_install_interrupts (X32): Unable to lock memory.");
      fprintf (stderr,
	       "\n;; Interrupt and exceptions handlers not available!\n");
      fflush (stderr);
      return;
    }

    if ((X32_int_intercept (DOS_INTVECT_USER_TIMER_TICK,
			    X32_timer_interrupt,
			    ((PTR) &X32_timer_interrupt_previous)))
	!= 0)
    {
      fprintf (stderr,
	       "\n;; DOS_install_interrupts (X32): Unable to intercept.");
      fprintf (stderr,
	       "\n;; Timer interrupt not available!\n");
      fflush (stderr);
    }
    else
      dos_record_interrupt_interception (DOS_INTVECT_USER_TIMER_TICK,
					 X32_interrupt_restore);

    if (!dpmi_p)
    {
#ifdef USE_ZORTECH_CERROR
      _cerror_handler = critical_error_handler;
      cerror_open ();
#else /* not USE_ZORTECH_CERROR */
      if ((X32_int_intercept (DOS_INTVECT_CRITICAL_ERROR,
			      X32_critical_error,
			      ((PTR) &X32_critical_error_previous)))
	  == 0)
	dos_record_interrupt_interception (DOS_INTVECT_CRITICAL_ERROR,
					   X32_interrupt_restore);
      
#endif /* USE_ZORTECH_CERROR */
    }
  }

  else
  {
    scm_int_intercept (DOS_INTVECT_USER_TIMER_TICK, 
		       bios_timer_handler, 
		       256);

    if (!dpmi_p)
    {
      scm_int_intercept (DOS_INTVECT_KB_CTRL_BREAK,
			 control_break_handler,
			 256);

#ifdef USE_ZORTECH_CERROR
      _cerror_handler = critical_error_handler;
      cerror_open ();
#else /* not USE_ZORTECH_CERROR */
      scm_int_intercept (DOS_INTVECT_CRITICAL_ERROR,
			 critical_error_handler,
			 256);
#endif /* USE_ZORTECH_CERROR */
    }
  }

  if ((dos_install_kbd_hook ()) == DOS_SUCCESS)
  {
    dos_record_interrupt_interception (DOS_INTVECT_SYSTEM_SERVICES,
				       DOS_restore_keyboard);
    DOS_keyboard_intercepted_p = true;    
  }

  if (dpmi_p && (enable_DPMI_exceptions_p ()))
    install_exception_handlers (DPMI_get_exception_vector,
				DPMI_set_exception_handler,
				DPMI_restore_handler);
  else if (x32_p)
    install_exception_handlers (X32_get_exception_vector,
				X32_set_exception_handler,
				X32_restore_handler);

  return;
}

void
DEFUN_VOID (DOS_restore_interrupts)
{
  int iv;

  if (dos_interrupts_initialized_p)
  {
    for (iv = (NUM_DOS_HANDLERS - 1); iv >= 0; iv--)
      if ((dos_interrupt_restoration[iv]) != ((int (*) (unsigned)) NULL))
      {
	(void) ((dos_interrupt_restoration[iv]) (iv));
	dos_interrupt_restoration[iv] = ((int (*) (unsigned)) NULL);
      }

#ifdef USE_ZORTECH_CERROR
    if (_cerror_handler == critical_error_handler)
    {
      cerror_close ();
      _cerror_handler = ((int _far _cdecl (*) (int *, int *)) NULL);
    }
#endif /* USE_ZORTECH_CERROR */

    dos_interrupts_initialized_p = false;
  }
  dos_set_ctrl_c_check_flag (ctrl_c_check_flag);
  return;
}

/* Signal Bindings */

#ifdef UNUSED

static void
DEFUN (bind_handler, (signo, handler),
       int signo AND
       Tsignal_handler handler)
{
  if ((signo != 0)
      && ((handler != ((Tsignal_handler) sighnd_stop)))
      && ((current_handler (signo)) == SIG_DFL))
    INSTALL_HANDLER (signo, handler);
  return;
}

#endif /* UNUSED */

void
DEFUN_VOID (DOS_initialize_signals)
{
#ifdef UNUSED
  initialize_signal_descriptors ();
  bind_handler (SIGINT,		sighnd_control_c);
  bind_handler (SIGTERM,	sighnd_control_g);
  bind_handler (SIGFPE,		sighnd_fpe);
  if ((isatty (STDIN_FILENO)) || option_emacs_subprocess)
    {
      bind_handler (SIGILL,	sighnd_hardware_trap);
      bind_handler (SIGSEGV,	sighnd_hardware_trap);
      bind_handler (SIGABRT,	sighnd_software_trap);
    }
  {
    struct signal_descriptor * scan = signal_descriptors;
    struct signal_descriptor * end = (scan + signal_descriptors_length);
    while (scan < end)
      {
	if (((scan -> flags) & NOCATCH) == 0)
	  switch (scan -> action)
	    {
	    case dfl_terminate:
	      bind_handler ((scan -> signo), sighnd_terminate);
	      break;
	    case dfl_stop:
	      bind_handler ((scan -> signo), sighnd_stop);
	      break;
	    }
	scan += 1;
      }
  }
#else /* UNUSED */
  DOS_initialize_interrupts ();
  DOS_install_interrupts ();
#endif /* UNUSED */
  return;
}
