/* -*-C-*-

$Id: os2ctty.c,v 1.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1994-1999 Massachusetts Institute of Technology

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

#include "os2.h"
#include "osctty.h"
#include "ossig.h"

#define CONTROL_B_ENABLE		(0x1)
#define CONTROL_G_ENABLE		(0x2)
#define CONTROL_U_ENABLE		(0x4)
#define CONTROL_X_ENABLE		(0x8)
#define INTERACTIVE_INTERRUPT_ENABLE	(0x10)
#define TERMINATE_INTERRUPT_ENABLE	(0x20)

#define ALL_ENABLES							\
  (CONTROL_B_ENABLE | CONTROL_G_ENABLE | CONTROL_U_ENABLE | CONTROL_X_ENABLE)

#define CONTROL_B			'\002'
#define CONTROL_G			'\007'
#define CONTROL_U			'\025'
#define CONTROL_X			'\030'
#define CONTROL_C			'\003'

#define KB_INT_CHARS_SIZE 5
#define KB_INT_TABLE_SIZE 256

static char keyboard_interrupt_characters [KB_INT_CHARS_SIZE];
static enum interrupt_handler keyboard_interrupt_handlers [KB_INT_CHARS_SIZE];
static enum interrupt_handler keyboard_interrupt_table [KB_INT_TABLE_SIZE];
static enum interrupt_handler keyboard_break_interrupt;
static Tinterrupt_enables keyboard_interrupt_enables;

static void
update_keyboard_interrupt_characters (void)
{
  unsigned int i;
  for (i = 0; (i < KB_INT_TABLE_SIZE); i += 1)
    (keyboard_interrupt_table[i]) = interrupt_handler_ignore;
  for (i = 0; (i < KB_INT_CHARS_SIZE); i += 1)
    (keyboard_interrupt_table [keyboard_interrupt_characters [i]]) =
      (keyboard_interrupt_handlers[i]);
}

void
OS2_initialize_keyboard_interrupts (void)
{
  (keyboard_interrupt_characters[0]) = CONTROL_B;
  (keyboard_interrupt_handlers[0]) = interrupt_handler_control_b;
  (keyboard_interrupt_characters[1]) = CONTROL_G;
  (keyboard_interrupt_handlers[1]) = interrupt_handler_control_g;
  (keyboard_interrupt_characters[2]) = CONTROL_U;
  (keyboard_interrupt_handlers[2]) = interrupt_handler_control_u;
  (keyboard_interrupt_characters[3]) = CONTROL_X;
  (keyboard_interrupt_handlers[3]) = interrupt_handler_control_x;
  (keyboard_interrupt_characters[4]) = CONTROL_C;
  (keyboard_interrupt_handlers[4]) = interrupt_handler_interactive;
  keyboard_break_interrupt = interrupt_handler_terminate;
  update_keyboard_interrupt_characters ();
  keyboard_interrupt_enables = ALL_ENABLES;
}

void
OS_ctty_get_interrupt_enables (Tinterrupt_enables * mask)
{
  (*mask) = keyboard_interrupt_enables;
}

void
OS_ctty_set_interrupt_enables (Tinterrupt_enables * mask)
{
  keyboard_interrupt_enables = ((*mask) & ALL_ENABLES);
}

unsigned int
OS_ctty_num_int_chars (void)
{
  return (KB_INT_CHARS_SIZE + 1);
}

cc_t
OS_tty_map_interrupt_char (cc_t int_char)
{
  return (int_char);
}

cc_t *
OS_ctty_get_int_chars (void)
{
  static cc_t characters [KB_INT_CHARS_SIZE + 1];
  unsigned int i;
  for (i = 0; (i < KB_INT_CHARS_SIZE); i += 1)
    (characters[i]) = (keyboard_interrupt_characters[i]);
  (characters[i]) = '\0';	/* dummy for control-break */
  return (characters);
}

void
OS_ctty_set_int_chars (cc_t * characters)
{
  unsigned int i;
  for (i = 0; (i < KB_INT_CHARS_SIZE); i += 1)
    (keyboard_interrupt_characters[i]) = (characters[i]);
  update_keyboard_interrupt_characters ();
}

cc_t *
OS_ctty_get_int_char_handlers (void)
{
  static cc_t handlers [KB_INT_CHARS_SIZE + 1];
  unsigned int i;
  for (i = 0; (i < KB_INT_CHARS_SIZE); i += 1)
    (handlers[i]) = ((cc_t) (keyboard_interrupt_handlers[i]));
  (handlers[i]) = ((cc_t) keyboard_break_interrupt);
  return (handlers);
}

void
OS_ctty_set_int_char_handlers (cc_t * handlers)
{
  unsigned int i;
  for (i = 0; (i < KB_INT_CHARS_SIZE); i += 1)
    (keyboard_interrupt_handlers[i]) =
      ((enum interrupt_handler) (handlers[i]));
  keyboard_break_interrupt = ((enum interrupt_handler) (handlers[i]));
  update_keyboard_interrupt_characters ();
}

static char
check_if_enabled (enum interrupt_handler handler)
{
  unsigned int bitmask;
  char result;
  switch (handler)
    {
    case interrupt_handler_control_b:
      bitmask = CONTROL_B_ENABLE;
      result = 'B';
      break;
    case interrupt_handler_control_g:
      bitmask = CONTROL_G_ENABLE;
      result = 'G';
      break;
    case interrupt_handler_control_u:
      bitmask = CONTROL_U_ENABLE;
      result = 'U';
      break;
    case interrupt_handler_control_x:
      bitmask = CONTROL_X_ENABLE;
      result = 'X';
      break;
    case interrupt_handler_interactive:
      bitmask = INTERACTIVE_INTERRUPT_ENABLE;
      result = '!';
      break;
    case interrupt_handler_terminate:
      bitmask = TERMINATE_INTERRUPT_ENABLE;
      result = '@';
      break;
    default:
      bitmask = 0;
      result = '\0';
      break;
    }
  return (((keyboard_interrupt_enables & bitmask) == 0) ? '\0' : result);
}

char
OS2_keyboard_interrupt_handler (char c)
{
  return (check_if_enabled (keyboard_interrupt_table[c]));
}

char
OS2_keyboard_break_interrupt_handler (void)
{
  return (check_if_enabled (keyboard_break_interrupt));
}
