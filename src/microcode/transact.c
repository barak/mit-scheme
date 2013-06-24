/* -*-C-*-

$Id: transact.c,v 1.5 2000/12/05 21:23:48 cph Exp $

Copyright (C) 1990-2000 Massachusetts Institute of Technology

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
#include "config.h"
#include "outf.h"
#include "dstack.h"

static void
DEFUN (error, (procedure_name, message),
       CONST char * procedure_name AND
       CONST char * message)
{
  outf_fatal ("%s: %s\n", procedure_name, message);
  outf_flush_fatal ();
  abort ();
}

enum transaction_state { active, aborting, committing };

struct transaction
{
  PTR checkpoint;
  enum transaction_state state;
};

static struct transaction * current_transaction;

static void
DEFUN (guarantee_current_transaction, (proc), CONST char * proc)
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
DEFUN_VOID (transaction_initialize)
{
  current_transaction = 0;
}

void
DEFUN_VOID (transaction_begin)
{
  PTR checkpoint = dstack_position;
  struct transaction * transaction =
    (dstack_alloc (sizeof (struct transaction)));
  (transaction -> checkpoint) = checkpoint;
  (transaction -> state) = active;
  dstack_bind ((&current_transaction), transaction);
}

void
DEFUN_VOID (transaction_abort)
{
  guarantee_current_transaction ("transaction_abort");
  (current_transaction -> state) = aborting;
  dstack_set_position (current_transaction -> checkpoint);
}

void
DEFUN_VOID (transaction_commit)
{
  guarantee_current_transaction ("transaction_commit");
  (current_transaction -> state) = committing;
  dstack_set_position (current_transaction -> checkpoint);
}

struct action
{
  enum transaction_action_type type;
  void EXFUN ((*procedure), (PTR environment));
  PTR environment;
};

static void
DEFUN (execute_action, (action), PTR action)
{
  if ((((struct action *) action) -> type) !=
      (((current_transaction -> state) == committing)
       ? tat_abort : tat_commit))
    (* (((struct action *) action) -> procedure))
      (((struct action *) action) -> environment);
}

void
DEFUN (transaction_record_action, (type, procedure, environment),
       enum transaction_action_type type AND
       void EXFUN ((*procedure), (PTR environment)) AND
       PTR environment)
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
