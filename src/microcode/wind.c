/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

#include "config.h"
#include "obstack.h"
#include "dstack.h"
#include "outf.h"
#include "os.h"
#define obstack_chunk_alloc OS_malloc
#define obstack_chunk_free free

extern void block_signals (void);
extern void unblock_signals (void);

static void error (const char *, const char *) NORETURN;

static void
error (const char * procedure_name, const char * message)
{
  outf_fatal ("%s: %s\n", procedure_name, message);
  outf_flush_fatal ();
  abort ();
}

struct winding_record
{
  struct winding_record * next;
  void (*protector) (void * environment);
  void * environment;
};

static struct obstack dstack;
static struct winding_record * current_winding_record;
void * dstack_position;

void
dstack_initialize (void)
{
  obstack_init (&dstack);
  dstack_position = 0;
  current_winding_record = 0;
}

void
dstack_reset (void)
{
  block_signals ();
  obstack_free ((&dstack), 0);
  dstack_initialize ();
  unblock_signals ();
}

#define EXPORT(sp) ((void *) (((char *) (sp)) + (sizeof (void *))))

void *
dstack_alloc (unsigned int length)
{
  void * chunk;
  block_signals ();
  chunk = (obstack_alloc ((&dstack), ((sizeof (void *)) + length)));
  (* ((void **) chunk)) = dstack_position;
  dstack_position = chunk;
  unblock_signals ();
  return (EXPORT (chunk));
}

void
dstack_protect (void (*protector) (void * environment),
       void * environment)
{
  struct winding_record * record =
    (dstack_alloc (sizeof (struct winding_record)));
  (record -> next) = current_winding_record;
  (record -> protector) = protector;
  (record -> environment) = environment;
  current_winding_record = record;
}

void
dstack_alloc_and_protect (unsigned int length,
			  void (*initializer) (void * environment),
			  void (*protector) (void * environment))
{
  struct winding_record * record =
    (dstack_alloc ((sizeof (struct winding_record)) + length));
  void * environment = (((char *) record) + (sizeof (struct winding_record)));
  (*initializer) (environment);
  (record -> next) = current_winding_record;
  (record -> protector) = protector;
  (record -> environment) = environment;
  current_winding_record = record;
}

void
dstack_set_position (void * position)
{
  block_signals ();
#define DEBUG_DSTACK
#ifdef DEBUG_DSTACK
  {
    void ** sp = dstack_position;
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
	  void * sp = dstack_position;
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
	void ** sp = dstack_position;
	dstack_position = (*sp);
	obstack_free ((&dstack), sp);
      }
    }
  unblock_signals ();
}

struct binding_record
{
  void ** location;
  void * value;
};

static void
undo_binding (void * environment)
{
  (* (((struct binding_record *) environment) -> location)) =
    (((struct binding_record *) environment) -> value);
}

static void ** save_binding_location;

static void
save_binding (void * environment)
{
  (((struct binding_record *) environment) -> location) =
    save_binding_location;
  (((struct binding_record *) environment) -> value) =
    (*save_binding_location);
}

void
dstack_bind (void * location, void * value)
{
  save_binding_location = location;
  dstack_alloc_and_protect
    ((sizeof (struct binding_record)), save_binding, undo_binding);
  (* ((void **) location)) = value;
}
