/* Copyright (C) 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/transact.c,v 1.1 1990/06/20 19:38:56 cph Rel $ */

#include <stdio.h>
#include "dstack.h"

static void
DEFUN (error, (procedure_name, message),
       CONST char * procedure_name AND
       CONST char * message)
{
  fprintf (stderr, "%s: %s\n", procedure_name, message);
  fflush (stderr);
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
    case committing: error (proc, "commit in progress");
    case aborting: error (proc, "abort in progress");
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
