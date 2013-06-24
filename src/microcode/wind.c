/* -*-C-*-

$Id: wind.c,v 1.6 1999/01/03 05:33:51 cph Exp $

Copyright (C) 1990-1999 Massachusetts Institute of Technology

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

#include <stdio.h>
#include "obstack.h"
#include "dstack.h"
#include "outf.h"
extern void EXFUN (free, (PTR ptr));
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern void EXFUN (block_signals, (void));
extern void EXFUN (unblock_signals, (void));

static void
DEFUN (error, (procedure_name, message),
       CONST char * procedure_name AND
       CONST char * message)
{
  outf_fatal ("%s: %s\n", procedure_name, message);
  outf_flush_fatal ();
  abort ();
}

static PTR
DEFUN (xmalloc, (length), unsigned int length)
{
  extern PTR EXFUN (malloc, (unsigned int length));
  PTR result = (malloc (length));
  if (result == 0)
    error ("malloc", "memory allocation failed");
  return (result);
}

struct winding_record
{
  struct winding_record * next;
  void EXFUN ((*protector), (PTR environment));
  PTR environment;
};

static struct obstack dstack;
static struct winding_record * current_winding_record;
PTR dstack_position;

void
DEFUN_VOID (dstack_initialize)
{
  obstack_init (&dstack);
  dstack_position = 0;
  current_winding_record = 0;
}

void
DEFUN_VOID (dstack_reset)
{
  block_signals ();
  obstack_free ((&dstack), 0);
  dstack_initialize ();
  unblock_signals ();
}

#define EXPORT(sp) ((PTR) (((char *) (sp)) + (sizeof (PTR))))

PTR
DEFUN (dstack_alloc, (length), unsigned int length)
{
  PTR chunk;
  block_signals ();
  chunk = (obstack_alloc ((&dstack), ((sizeof (PTR)) + length)));
  (* ((PTR *) chunk)) = dstack_position;
  dstack_position = chunk;
  unblock_signals ();
  return (EXPORT (chunk));
}

void
DEFUN (dstack_protect, (protector, environment),
       void EXFUN ((*protector), (PTR environment)) AND
       PTR environment)
{
  struct winding_record * record =
    (dstack_alloc (sizeof (struct winding_record)));
  (record -> next) = current_winding_record;
  (record -> protector) = protector;
  (record -> environment) = environment;
  current_winding_record = record;
}

void
DEFUN (dstack_alloc_and_protect, (length, initializer, protector),
       unsigned int length AND
       void EXFUN ((*initializer), (PTR environment)) AND
       void EXFUN ((*protector), (PTR environment)))
{
  struct winding_record * record =
    (dstack_alloc ((sizeof (struct winding_record)) + length));
  PTR environment = (((char *) record) + (sizeof (struct winding_record)));
  (*initializer) (environment);
  (record -> next) = current_winding_record;
  (record -> protector) = protector;
  (record -> environment) = environment;
  current_winding_record = record;
}

void
DEFUN (dstack_set_position, (position), PTR position)
{
  block_signals ();
#define DEBUG_DSTACK
#ifdef DEBUG_DSTACK
  {
    PTR * sp = dstack_position;
    while (sp != position)
      {
	if (sp == 0)
	  error ("dstack_set_position", "position argument not found");
	sp = (*sp);
      }
  }
#endif /* DEBUG_DSTACK */
  while (dstack_position != position)
    {
      if (dstack_position == 0)
	error ("dstack_set_position", "no more stack");
      if ((EXPORT (dstack_position)) == current_winding_record)
	{
	  PTR sp = dstack_position;
	  struct winding_record * record = current_winding_record;
	  /* Must unblock signals while the protector runs, and
	     re-block afterwards, in case the protector does something
	     to change the signal mask.  Otherwise, the change to the
	     signal mask will be undone when the final call to
	     unblock_signals is performed.  */
	  unblock_signals ();
	  (* (record -> protector)) (record -> environment);
	  block_signals ();
	  if (sp != dstack_position)
	    error ("dstack_set_position", "stack slipped during unwind");
	  current_winding_record = (record -> next);
	}
      {
	PTR * sp = dstack_position;
	dstack_position = (*sp);
	obstack_free ((&dstack), sp);
      }
    }
  unblock_signals ();
}

struct binding_record
{
  PTR * location;
  PTR value;
};

static void
DEFUN (undo_binding, (environment), PTR environment)
{
  (* (((struct binding_record *) environment) -> location)) =
    (((struct binding_record *) environment) -> value);
}

static PTR * save_binding_location;

static void
DEFUN (save_binding, (environment), PTR environment)
{
  (((struct binding_record *) environment) -> location) =
    save_binding_location;
  (((struct binding_record *) environment) -> value) =
    (*save_binding_location);
}

void
DEFUN (dstack_bind, (location, value), PTR location AND PTR value)
{
  save_binding_location = location;
  dstack_alloc_and_protect
    ((sizeof (struct binding_record)), save_binding, undo_binding);
  (* ((PTR *) location)) = value;
}
