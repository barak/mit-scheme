/* -*-C-*-

$Id: dstack.h,v 1.14 2007/01/05 15:33:06 cph Exp $

Copyright 1990,1991,1992,1993,2004 Massachusetts Institute of Technology

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

#ifndef __DSTACK_H__
#define __DSTACK_H__

#include "config.h"
#include "ansidecl.h"
#include <setjmp.h>

#ifdef STDC_HEADERS
#  include <stdlib.h>
#endif

extern void EXFUN (dstack_initialize, (void));
/* Call this once to initialize the stack. */

extern void EXFUN (dstack_reset, (void));
/* Call this once to reset the stack. */

extern PTR EXFUN (dstack_alloc, (unsigned int length));
/* Allocate a chunk of `length' bytes of space on the stack and return
   a pointer to it. */

extern void EXFUN
  (dstack_protect,
   (void EXFUN ((*protector), (PTR environment)), PTR environment));
/* Create an unwind protection frame that invokes `protector' when
   the stack is unwound.  `environment' is passed to `protector' as
   its sole argument when it is invoked. */

extern void EXFUN
  (dstack_alloc_and_protect,
   (unsigned int length,
    void EXFUN ((*initializer), (PTR environment)),
    void EXFUN ((*protector), (PTR environment))));
/* Allocate a chunk of `length' bytes of space, call `initializer' to
   initialize that space, and create an unwind protection frame that
   invokes `protector' when the stack is unwound.  */

extern PTR dstack_position;
/* The current stack pointer. */

extern void EXFUN (dstack_set_position, (PTR position));
/* Unwind the stack to `position', which must be a previous value of
   `dstack_position'. */

extern void EXFUN (dstack_bind, (PTR location, PTR value));
/* Dynamically bind `location' to `value'.  `location' is treated as
   `PTR*' -- it is declared `PTR' for programming convenience. */

enum transaction_action_type { tat_abort, tat_commit, tat_always };

extern void EXFUN (transaction_initialize, (void));
extern void EXFUN (transaction_begin, (void));
extern void EXFUN (transaction_abort, (void));
extern void EXFUN (transaction_commit, (void));
extern void EXFUN
  (transaction_record_action,
   (enum transaction_action_type type,
    void EXFUN ((*procedure), (PTR environment)),
    PTR environment));

typedef unsigned long Tptrvec_index;
typedef unsigned long Tptrvec_length;

struct struct_ptrvec
{
  Tptrvec_length length;
  PTR * elements;
};
typedef struct struct_ptrvec * Tptrvec;

#define PTRVEC_LENGTH(ptrvec) ((ptrvec) -> length)
#define PTRVEC_REF(ptrvec, index) (((ptrvec) -> elements) [(index)])
#define PTRVEC_LOC(ptrvec, index) (& (PTRVEC_REF ((ptrvec), (index))))
#define PTRVEC_START(ptrvec) (PTRVEC_LOC ((ptrvec), 0))
#define PTRVEC_END(ptrvec) (PTRVEC_LOC ((ptrvec), (PTRVEC_LENGTH (ptrvec))))

extern Tptrvec EXFUN (ptrvec_allocate, (Tptrvec_length length));
extern void EXFUN (ptrvec_deallocate, (Tptrvec ptrvec));
extern void EXFUN (ptrvec_set_length, (Tptrvec ptrvec, Tptrvec_length length));
extern Tptrvec EXFUN (ptrvec_copy, (Tptrvec ptrvec));
extern void EXFUN (ptrvec_adjoin, (Tptrvec ptrvec, PTR element));
extern int EXFUN (ptrvec_memq, (Tptrvec ptrvec, PTR element));
extern void EXFUN
  (ptrvec_move_left,
   (Tptrvec source, Tptrvec_index source_start, Tptrvec_index source_end,
    Tptrvec target, Tptrvec_index target_start));
extern void EXFUN
  (ptrvec_move_right,
   (Tptrvec source, Tptrvec_index source_start, Tptrvec_index source_end,
    Tptrvec target, Tptrvec_index target_start));

typedef struct condition_type * Tcondition_type;
typedef struct condition * Tcondition;
typedef struct condition_restart * Tcondition_restart;

struct condition_type
{
  unsigned long index;
  PTR name;
  Tptrvec generalizations;
  void EXFUN ((*reporter), (Tcondition));
};
#define CONDITION_TYPE_INDEX(type) ((type) -> index)
#define CONDITION_TYPE_NAME(type) ((type) -> name)
#define CONDITION_TYPE_GENERALIZATIONS(type) ((type) -> generalizations)
#define CONDITION_TYPE_REPORTER(type) ((type) -> reporter)

struct condition
{
  Tcondition_type type;
  Tptrvec irritants;
};
#define CONDITION_TYPE(condition) ((condition) -> type)
#define CONDITION_IRRITANTS(condition) ((condition) -> irritants)

struct condition_restart
{
  PTR name;
  Tcondition_type type;
  void EXFUN ((*procedure), (PTR));
};
#define CONDITION_RESTART_NAME(restart) ((restart) -> name)
#define CONDITION_RESTART_TYPE(restart) ((restart) -> type)
#define CONDITION_RESTART_PROCEDURE(restart) ((restart) -> procedure)

/* Allocate and return a new condition type object. */
extern Tcondition_type EXFUN
  (condition_type_allocate,
   (PTR name,
    Tptrvec generalizations,
    void EXFUN ((*reporter), (Tcondition condition))));

/* Deallocate the condition type object `type'. */
extern void EXFUN (condition_type_deallocate, (Tcondition_type type));

/* Allocate and return a new condition object. */
extern Tcondition EXFUN
  (condition_allocate, (Tcondition_type type, Tptrvec irritants));

/* Deallocate the condition object `condition'. */
extern void EXFUN (condition_deallocate, (Tcondition condition));

/* Bind a handler for the condition type object `type'. */
extern void EXFUN
  (condition_handler_bind,
   (Tcondition_type type, void EXFUN ((*handler), (Tcondition condition))));

/* Signal `condition'. */
extern void EXFUN (condition_signal, (Tcondition condition));

/* Bind a restart called `name' for the condition type object `type'.
   Invoking the restart causes `restart_procedure' to be executed. */
extern void EXFUN
  (condition_restart_bind,
   (PTR name,
    Tcondition_type type,
    void EXFUN ((*procedure), (PTR argument))));

/* Find a restart called `name' that matches `condition'.
   If `condition' is 0, any restart called `name' will do.
   If no such restart exists, 0 is returned. */
extern Tcondition_restart EXFUN
  (condition_restart_find, (PTR name, Tcondition condition));

/* Return a ptrvec of the restarts that match `condition'.
   If `condition' is 0, all restarts are returned. */
extern Tptrvec EXFUN (condition_restarts, (Tcondition condition));

#endif /* __DSTACK_H__ */
