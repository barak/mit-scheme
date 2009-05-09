/* -*-C-*-

$Id: transact.c,v 1.11 2008/01/30 20:02:21 cph Exp $

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

#include "config.h"
#include "outf.h"
#include "dstack.h"

static void
error (const char * procedure_name, const char * message)
{
  outf_fatal ("%s: %s\n", procedure_name, message);
  outf_flush_fatal ();
  abort ();
}

enum transaction_state { active, aborting, committing };

struct transaction
{
  void * checkpoint;
  enum transaction_state state;
};

static struct transaction * current_transaction;

static void
guarantee_current_transaction (const char * proc)
{
  if (current_transaction == 0)
    error (proc, "no transaction");
  switch (current_transaction -> state)
    {
    case committing: error (proc, "commit in progress"); break;
    case aborting: error (proc, "abort in progress"); break;
    case active: break;
    }
}

void
transaction_initialize (void)
{
  current_transaction = 0;
}

void
transaction_begin (void)
{
  void * checkpoint = dstack_position;
  struct transaction * transaction =
    (dstack_alloc (sizeof (struct transaction)));
  (transaction -> checkpoint) = checkpoint;
  (transaction -> state) = active;
  dstack_bind ((&current_transaction), transaction);
}

void
transaction_abort (void)
{
  guarantee_current_transaction ("transaction_abort");
  (current_transaction -> state) = aborting;
  dstack_set_position (current_transaction -> checkpoint);
}

void
transaction_commit (void)
{
  guarantee_current_transaction ("transaction_commit");
  (current_transaction -> state) = committing;
  dstack_set_position (current_transaction -> checkpoint);
}

struct action
{
  enum transaction_action_type type;
  void (*procedure) (void * environment);
  void * environment;
};

static void
execute_action (void * action)
{
  if ((((struct action *) action) -> type) !=
      (((current_transaction -> state) == committing)
       ? tat_abort : tat_commit))
    (* (((struct action *) action) -> procedure))
      (((struct action *) action) -> environment);
}

void
transaction_record_action (enum transaction_action_type type,
			   void (*procedure) (void * environment),
			   void * environment)
{
  guarantee_current_transaction ("transaction_record_action");
  {
    struct action * action = (dstack_alloc (sizeof (struct action)));
    (action -> type) = type;
    (action -> procedure) = procedure;
    (action -> environment) = environment;
    dstack_protect (execute_action, action);
  }
}
