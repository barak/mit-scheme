/* -*-C-*-

$Id: dosconio.c,v 1.7 1992/09/24 01:34:59 cph Exp $

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

/* Console I/O supplement */

#include "scheme.h"
#include "prims.h"
#include "msdos.h"
#include "dosio.h"
#include "dosscan.h"
#include "dossys.h"
#include "intrpt.h"

#ifdef __STDC__
#define fileno(fp)	((fp)->_file)
#endif

#define CONIO_BUFFER_SIZE	(1024)
#define TYPEAHEAD_BUFFER_SIZE	(1024)

#define System_Error_Reset()		\
  (errno = 0)

#define System_Error_Return(err)	\
{					\
  errno = err;				\
  return -1;				\
}

/* Characters are kept in the typeahead_buffer before read is called,
   in the key_buffer before return is pressed, and in the line_buffer
   before the line is read.
 */
typedef	struct conin_buffer_struct
{
  unsigned char buffer[CONIO_BUFFER_SIZE];
  size_t length;
} conio_buffer_t;

typedef struct typeahead_buffer_struct
{
  unsigned char buffer[TYPEAHEAD_BUFFER_SIZE];
  size_t length;
} typeahead_buffer_t;

static conio_buffer_t line_buffer, key_buffer;
static typeahead_buffer_t typeahead_buffer;

static int max_scancode_conversion_length = 1;
static unsigned char * keyboard_scancode_table[] = DEFAULT_SCANCODE_CONVERSIONS;

#define IMAX(a, b) (((a) > (b)) ? (a) : (b))

#define TYPEAHEAD_BUFFER_REMAINING()	\
  (TYPEAHEAD_BUFFER_SIZE - typeahead_buffer.length)

#define TYPEAHEAD_BUFFER_AVAILABLE_P()	\
  ((TYPEAHEAD_BUFFER_REMAINING ()) >= max_scancode_conversion_length)

static void
DEFUN (map_keyboard_scancode, (scancode), unsigned char scancode)
{
  extern int signal_keyboard_character_interrupt (int);

  if (scancode < KEYBOARD_SCANCODE_TABLE_SIZE)
  {
    int len;
    unsigned char * conversion = keyboard_scancode_table[scancode];
    if (conversion == NO_CONVERSION)
      return;
    else if (conversion == SOFT_ATTN)
    {
      signal_keyboard_character_interrupt (-1);
      return;
    }
    else if (conversion == HARD_ATTN)
    {
      signal_keyboard_character_interrupt (-2);
      return;
    }

    len = ((conversion == CTRL_AT) ? 1 : (strlen (conversion)));
    
    if (len <= (TYPEAHEAD_BUFFER_REMAINING ()))
    {
      /* Copy conversion string into typeahead buffer, worrying about
	 interrupt characters along the way.
       */
      while (--len >= 0)
      {
	if ((signal_keyboard_character_interrupt ((int) (*conversion))) == 0)
	  typeahead_buffer.buffer[typeahead_buffer.length++] = *conversion++;
      }
    }
  }
  return;
}

static void
DEFUN_VOID (recompute_max_scancode_conversion_length)
{
  int i, length;
  max_scancode_conversion_length = 1;

  for (i = 0; i < KEYBOARD_SCANCODE_TABLE_SIZE; i++)
  { 
    unsigned char * conversion = keyboard_scancode_table[i];
    if (conversion == NO_CONVERSION)
      length = 0;
    else if (conversion == CTRL_AT)
      length = 1;
    else
      length = (strlen (conversion));
    max_scancode_conversion_length = (IMAX (length,
					    max_scancode_conversion_length));
  }
  return;
}

static void
DEFUN_VOID (consume_typeahead)
{
  extern int signal_keyboard_character_interrupt (int);
  unsigned char character;

  while ((TYPEAHEAD_BUFFER_AVAILABLE_P ()) &&
	 (dos_poll_keyboard_character (&character)))
  { 
    if (character == '\0') /* Extended scancode */
    { 
      dos_poll_keyboard_character (&character);
      map_keyboard_scancode (character);
    }
    else if ((signal_keyboard_character_interrupt ((int) character)) == 0)
      typeahead_buffer.buffer[typeahead_buffer.length++] = character;
    else
      break;
  }
  return;
}

static int
DEFUN_VOID (typeahead_available_p)
{
  consume_typeahead ();
  return (!(typeahead_buffer.length == 0));
}

static unsigned char
DEFUN_VOID (get_typeahead_character)
{
  if (typeahead_buffer.length == 0)
    return '\0';
  else
  {
    int i;
    unsigned char result;

    result = typeahead_buffer.buffer[0];
    for (i = 1; i < typeahead_buffer.length; i++)
      typeahead_buffer.buffer[i - 1] = typeahead_buffer.buffer[i];
    typeahead_buffer.length--;
    return (result);
  }
}

static void
DEFUN (key_buffer_insert_self, (c), unsigned char c)
{
  static unsigned char crlf[] = {CARRIAGE_RETURN, LINEFEED};

  if (key_buffer.length != CONIO_BUFFER_SIZE)
  {
    key_buffer.buffer[key_buffer.length++] = c;
    if (c == LINEFEED)
      dos_console_write (crlf, (sizeof (crlf)));
    else
      dos_console_write (&c, 1);
  }
  return;
}

static void
DEFUN_VOID (key_buffer_erase_character)
{
  static char erase[] = {BACKSPACE, SPACE, BACKSPACE};

  if (key_buffer.length != 0)
  {
    key_buffer.length -= 1;
    dos_console_write (erase, (sizeof (erase)));
  }
  return;
}

static void
DEFUN_VOID (key_buffer_to_line_buffer)
{
  register size_t i = 0;
  register size_t j = 0;

  while ((i < key_buffer.length)
	 && (line_buffer.length != CONIO_BUFFER_SIZE))
    line_buffer.buffer[line_buffer.length++] = key_buffer.buffer[i++];
  while (i < key_buffer.length)
    key_buffer.buffer[j++] = key_buffer.buffer[i++];
  key_buffer.length = j;
  return;
}

void
DEFUN_VOID (flush_conio_buffers)
{
  line_buffer.length = 0;
  key_buffer.length = 0;
  typeahead_buffer.length = 0;
  return;
}

void
DEFUN_VOID (DOS_initialize_conio)
{
  void initialize_keyboard_interrupt_table (void);
  void initialize_scancode_table (void);

  flush_conio_buffers ();
  initialize_keyboard_interrupt_table ();
  initialize_scancode_table ();
  return;
}

extern void EXFUN (DOS_initialize_fov, (SCHEME_OBJECT));

void
DEFUN (DOS_initialize_fov, (fov), SCHEME_OBJECT fov)
{
  extern SCHEME_OBJECT EXFUN (make_primitive, (char *));
  SCHEME_OBJECT iv, prim;

  prim = (make_primitive ("DOS-HIGH-PRIORITY-TIMER-INTERRUPT"));
  iv = (FAST_VECTOR_REF (fov, System_Interrupt_Vector));
  VECTOR_SET (iv, Global_GC_Level, prim);
  return;
}

static void
DEFUN (non_buffered_key_command, (c), unsigned char c)
{
  if (line_buffer.length == CONIO_BUFFER_SIZE) return;
 
  if ((!DOS_keyboard_intercepted_p)
      && (c == BACKSPACE))
    c = DELETE;
  line_buffer.buffer[line_buffer.length++] = c;
  return;
}

static int
DEFUN (empty_line_buffer, (buffer, nbytes), char * buffer AND size_t nbytes)
{
  register size_t i, j;
  
  for (i = 0; ((i < line_buffer.length)&&(i < nbytes)); i++)
    *buffer++ = line_buffer.buffer[i];
  nbytes = i;
  for (j = 0; i < line_buffer.length; i++, j++)
    line_buffer.buffer[j] = line_buffer.buffer[i];
  line_buffer.length -= nbytes;
  return (nbytes);
}

static void
DEFUN (buffered_key_command, (c), unsigned char c)
{
  switch(c)
  { 
    case CARRIAGE_RETURN:
    case LINEFEED:
      key_buffer_insert_self(LINEFEED);
      key_buffer_to_line_buffer();
      break;
    case DELETE:
    case BACKSPACE: /* Backspace */
      if (key_buffer.length != 0)
	key_buffer_erase_character();
      break;
    default:
      key_buffer_insert_self(c);
      break;
  }
  return;
}

long
DEFUN (console_read, (buffer, nbytes, buffered_p, blocking_p),
       char * buffer AND unsigned nbytes AND int buffered_p AND int blocking_p)
{ 
  System_Error_Reset();
  do
  { /* Get all pending characters into the buffer */
    while (typeahead_available_p ())
    { 
      if (buffered_p)
	buffered_key_command (get_typeahead_character ());
      else
	/* Non buffered channel, in CScheme, also no echo. */
      	non_buffered_key_command (get_typeahead_character ());
    } /* End WHILE */
    /* Test for pending interrupts here: */
    if (pending_interrupts_p ())
    {
      if (INTERRUPT_QUEUED_P (INT_Character))
	flush_conio_buffers ();
      System_Error_Return (EINTR);
    }
    /* Return if we buffered up a line, or channel is not buffered */
    if (line_buffer.length != 0)
      return (empty_line_buffer (buffer, nbytes));
  } while (blocking_p);	/* Keep reading for blocking channel. */
  /* This means there is nothing available, don't block */
  System_Error_Return (ERRNO_NONBLOCK);
}

extern int EXFUN
 (text_write, (int fd AND CONST unsigned char * buffer AND size_t nbytes));

void
DEFUN (console_write_string, (string), void * string)
{
  text_write ((fileno(stdout)), string, strlen((char *) string));
  return;
}

DEFINE_PRIMITIVE ("DOS-HIGH-PRIORITY-TIMER-INTERRUPT", Prim_dos_high_priority_timer, 2, 2,
		  "DOS High-priority timer interrupt handler.")
{
  extern void EXFUN (dos_process_timer_interrupt, (void));
  PRIMITIVE_HEADER (2);

  consume_typeahead ();
  dos_process_timer_interrupt ();
  CLEAR_INTERRUPT (INT_Global_GC);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("KEYBOARD-GET-CONVERSION", Prim_keyboard_get_conversion, 1, 1,
		  "Translate a keyboard scan code into a string.")
{
  PRIMITIVE_HEADER (1);
  {
    long scancode = arg_integer(1);

    if ((scancode < 0) || (scancode >= KEYBOARD_SCANCODE_TABLE_SIZE))
      error_bad_range_arg(1);
    else
    {
      unsigned char * conversion = keyboard_scancode_table[scancode];
      if (conversion == NO_CONVERSION)
	PRIMITIVE_RETURN (SHARP_F);
      else if (conversion == CTRL_AT)
	PRIMITIVE_RETURN (memory_to_string (1, "\0"));
      else
	PRIMITIVE_RETURN (char_pointer_to_string (conversion));
    }
  }
}

#ifndef ULONG_BIT
# define ULONG_BIT		((sizeof (unsigned long)) * CHAR_BIT)
#endif

#define MALLOCED_TABLE_SIZE	\
  ((KEYBOARD_SCANCODE_TABLE_SIZE + (ULONG_BIT - 1)) / ULONG_BIT)

static unsigned long scancode_malloced_table[MALLOCED_TABLE_SIZE] = {0,};

#define SCANCODE_TO_MALLOCED_TABLE_WORD(s)	((s) / ULONG_BIT)
#define SCANCODE_TO_MALLOCED_TABLE_BIT(s)	((s) % ULONG_BIT)

#define SCANCODE_MALLOCED_P(s)						\
  ((scancode_malloced_table[SCANCODE_TO_MALLOCED_TABLE_WORD (s)] &	\
    (1 << SCANCODE_TO_MALLOCED_TABLE_BIT (s)))				\
   != 0)

#define SCANCODE_MALLOCED(s)						\
  (scancode_malloced_table[SCANCODE_TO_MALLOCED_TABLE_WORD (s)] |=	\
   (1 << SCANCODE_TO_MALLOCED_TABLE_BIT (s)))

#define SCANCODE_MALLOCED_NOT(s)					\
  (scancode_malloced_table[SCANCODE_TO_MALLOCED_TABLE_WORD (s)] &=	\
   (~(1 << SCANCODE_TO_MALLOCED_TABLE_BIT (s))))

static void
DEFUN_VOID (initialize_scancode_table)
{
  int i;

  for (i = 0; i < MALLOCED_TABLE_SIZE; i++)
    scancode_malloced_table[i] = ((unsigned long) 0);
  recompute_max_scancode_conversion_length ();
  return;
}

DEFINE_PRIMITIVE ("KEYBOARD-SET-CONVERSION!", Prim_keyboard_set_conversion, 2, 2,
		  "Set the translation for a keyboard scan code.")
{
  PRIMITIVE_HEADER (2);
  {
    int scancode = (arg_integer (1));
    SCHEME_OBJECT scheme_conversion = (ARG_REF (2));

    if ((scancode < 0) || (scancode >= KEYBOARD_SCANCODE_TABLE_SIZE))
      error_bad_range_arg(1);
    else
    { 
      int len;
      unsigned char * old_conversion = keyboard_scancode_table[scancode];
      int old_malloced_p = (SCANCODE_MALLOCED_P (scancode));

      if ((scheme_conversion != SHARP_F) && (!STRING_P (scheme_conversion)))
	error_wrong_type_arg (2);

      len = ((scheme_conversion == SHARP_F)
	     ? 0
	     : (STRING_LENGTH (scheme_conversion)));
      if (len == 0)
      {
	keyboard_scancode_table[scancode] = NO_CONVERSION;
	SCANCODE_MALLOCED_NOT (scancode);
	if (old_malloced_p)
	  DOS_free (old_conversion);
      }
      else if ((len == 1)
	       && ((STRING_REF (scheme_conversion, 0)) == '\0'))
      {
	keyboard_scancode_table[scancode] = CTRL_AT;
	SCANCODE_MALLOCED_NOT (scancode);
	if (old_malloced_p)
	  DOS_free (old_conversion);
      }
      else
      {
	int i;
	unsigned char * conversion, * ptr, * scheme;

	conversion = (DOS_malloc (len + 1));
	if (conversion == 0)
	  error_system_call (ENOMEM, syscall_malloc);
	ptr = conversion;
	scheme = (STRING_LOC (scheme_conversion, 0));
	for (i = 0; i < len; i ++)
	  *ptr++ = *scheme++;
	*ptr = '\0';
	keyboard_scancode_table[scancode] = conversion;
	SCANCODE_MALLOCED (scancode);
	if (old_malloced_p)
	  DOS_free (old_conversion);
      }
      recompute_max_scancode_conversion_length ();
      PRIMITIVE_RETURN (UNSPECIFIC);
    }
  }
}
