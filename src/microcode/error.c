/* -*-C-*-

$Id: 3d6475e68a747f03bf7c75aad7ccf76595d63707 $

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
#include "os.h"

static Tptrvec generalizations_union (Tptrvec);

struct handler_record
{
  struct handler_record * next;
  Tcondition_type type;
  void (*handler) (Tcondition);
};

struct restart_record
{
  struct restart_record * next;
  struct condition_restart contents;
};

static unsigned long next_condition_type_index;
static struct handler_record * current_handler_record;
static struct restart_record * current_restart_record;

void
initialize_condition_system (void)
{
  next_condition_type_index = 0;
  current_handler_record = 0;
  current_restart_record = 0;
}

Tcondition_type
condition_type_allocate (void * name,
       Tptrvec generalizations,
       void (*reporter) (Tcondition condition))
{
  Tcondition_type type = (OS_malloc (sizeof (struct condition_type)));
  Tptrvec g = (generalizations_union (generalizations));
  ptrvec_adjoin (g, type);
  (CONDITION_TYPE_INDEX (type)) = (next_condition_type_index++);
  (CONDITION_TYPE_NAME (type)) = name;
  (CONDITION_TYPE_GENERALIZATIONS (type)) = g;
  (CONDITION_TYPE_REPORTER (type)) = reporter;
  return (type);
}

void
condition_type_deallocate (Tcondition_type type)
{
  ptrvec_deallocate (CONDITION_TYPE_GENERALIZATIONS (type));
  free (type);
}

Tcondition
condition_allocate (Tcondition_type type,
       Tptrvec irritants)
{
  Tcondition condition = (OS_malloc (sizeof (struct condition)));
  (CONDITION_TYPE (condition)) = type;
  (CONDITION_IRRITANTS (condition)) = irritants;
  return (condition);
}

void
condition_deallocate (Tcondition condition)
{
  ptrvec_deallocate (CONDITION_IRRITANTS (condition));
  free (condition);
}

static Tptrvec
generalizations_union_2 (Tptrvec x, Tptrvec y)
{
  void ** scan_x = (PTRVEC_START (x));
  void ** end_x = (scan_x + (PTRVEC_LENGTH (x)));
  void ** scan_y = (PTRVEC_START (y));
  void ** end_y = (scan_y + (PTRVEC_LENGTH (y)));
  Tptrvec_length length = 0;
  unsigned long ix;
  unsigned long iy;
  Tptrvec result;
  void ** scan_result;
  while (1)
    {
      if (scan_x == end_x)
	{
	  length += (end_y - scan_y);
	  break;
	}
      if (scan_y == end_y)
	{
	  length += (end_x - scan_x);
	  break;
	}
      length += 1;
      ix = (CONDITION_TYPE_INDEX ((Tcondition_type) (*scan_x)));
      iy = (CONDITION_TYPE_INDEX ((Tcondition_type) (*scan_y)));
      if (ix <= iy) scan_x += 1;
      if (iy <= ix) scan_y += 1;
    }
  result = (ptrvec_allocate (length));
  scan_result = (PTRVEC_START (result));
  while (1)
    {
      if (scan_x == end_x)
	{
	  while (scan_y < end_y) (*scan_result++) = (*scan_y++);
	  break;
	}
      if (scan_y == end_y)
	{
	  while (scan_x < end_x) (*scan_result++) = (*scan_x++);
	  break;
	}
      ix = (CONDITION_TYPE_INDEX ((Tcondition_type) (*scan_x)));
      iy = (CONDITION_TYPE_INDEX ((Tcondition_type) (*scan_y)));
      if (ix == iy)
	{
	  (*scan_result++) = (*scan_x++);
	  scan_y += 1;
	}
      else
	(*scan_result++) = ((ix < iy) ? (*scan_x++) : (*scan_y++));
    }
  return (result);
}

Tptrvec
generalizations_union (Tptrvec generalizations)
{
  Tptrvec_length length = (PTRVEC_LENGTH (generalizations));
  if (length == 0)
    return (ptrvec_allocate (0));
  if (length == 1)
    return (ptrvec_copy (PTRVEC_REF (generalizations, 0)));
  {
    void ** scan = (PTRVEC_START (generalizations));
    void ** end = (scan + length);
    Tptrvec result = ((Tptrvec) (*scan++));
    result = (generalizations_union_2 (result, ((Tptrvec) (*scan++))));
    while (scan < end)
      {
	Tptrvec v = (generalizations_union_2 (result, ((Tptrvec) (*scan++))));
	ptrvec_deallocate (result);
	result = v;
      }
    return (result);
  }
}

void
condition_handler_bind (Tcondition_type type,
       void (*handler) (Tcondition condition))
{
  struct handler_record * record =
    (dstack_alloc (sizeof (struct handler_record)));
  (record -> next) = current_handler_record;
  (record -> type) = type;
  (record -> handler) = handler;
  dstack_bind ((&current_handler_record), record);
}

#define GENERALIZATIONS(condition)					\
  (CONDITION_TYPE_GENERALIZATIONS (CONDITION_TYPE (condition)))

void
condition_signal (Tcondition condition)
{
  Tptrvec generalizations = (GENERALIZATIONS (condition));
  struct handler_record * record = current_handler_record;
  while (record != 0)
    {
      Tcondition_type type = (record -> type);
      if ((type == 0) || (ptrvec_memq (generalizations, type)))
	{
	  void * position = dstack_position;
	  dstack_bind ((&current_handler_record), (record -> next));
	  (* (record -> handler)) (condition);
	  dstack_set_position (position);
	}
      record = (record -> next);
    }
}

void
condition_restart_bind (void * name,
       Tcondition_type type,
       void (*procedure) (void * argument))
{
  struct restart_record * record =
    (dstack_alloc (sizeof (struct restart_record)));
  (record -> next) = current_restart_record;
  (record -> contents . name) = name;
  (record -> contents . type) = type;
  (record -> contents . procedure) = procedure;
  dstack_bind ((&current_restart_record), record);
}

Tcondition_restart
condition_restart_find (void * name,
       Tcondition condition)
{
  struct restart_record * record = current_restart_record;
  if (condition == 0)
    while (record != 0)
      {
	if ((record -> contents . name) == name)
	  return (& (record -> contents));
	record = (record -> next);
      }
  else
    {
      Tptrvec generalizations = (GENERALIZATIONS (condition));
      while (record != 0)
	{
	  if (((record -> contents . name) == name) &&
	      (ptrvec_memq (generalizations, (record -> contents . type))))
	    return (& (record -> contents));
	  record = (record -> next);
	}
    }
  return (0);
}

Tptrvec
condition_restarts (Tcondition condition)
{
  struct restart_record * record = current_restart_record;
  Tptrvec_length length = 0;
  Tptrvec generalizations = 0;
  Tptrvec result;
  void ** scan_result;
  if (condition == 0)
    while (record != 0)
      {
	length += 1;
	record = (record -> next);
      }
  else
    {
      generalizations = (GENERALIZATIONS (condition));
      while (record != 0)
	{
	  if (ptrvec_memq (generalizations, (record -> contents . type)))
	    length += 1;
	  record = (record -> next);
	}
    }
  result = (ptrvec_allocate (length));
  scan_result = (PTRVEC_START (result));
  record = current_restart_record;
  if (condition == 0)
    while (record != 0)
      {
	(*scan_result++) = (& (record -> contents));
	record = (record -> next);
      }
  else
    while (record != 0)
      {
	if (ptrvec_memq (generalizations, (record -> contents . type)))
	  (*scan_result++) = (& (record -> contents));
	record = (record -> next);
      }
  return (result);
}
