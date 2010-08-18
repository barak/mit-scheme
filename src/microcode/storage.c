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

/* Global-variable storage */

#include "scheme.h"

#ifndef CC_SUPPORT_P
   SCHEME_OBJECT Registers [REGBLOCK_MINIMUM_LENGTH];
#endif

/* next free word in heap */
SCHEME_OBJECT * Free;

/* strict limit for Free */
SCHEME_OBJECT * heap_alloc_limit;

/* limits of active heap */
SCHEME_OBJECT * heap_start;
SCHEME_OBJECT * heap_end;

/* pointer to most-recently pushed item */
SCHEME_OBJECT * stack_pointer;

/*-strict limit for stack_pointer */
SCHEME_OBJECT * stack_guard;

/* limits of stack */
SCHEME_OBJECT * stack_start;
SCHEME_OBJECT * stack_end;

/* next free word in constant space */
SCHEME_OBJECT * constant_alloc_next;

/* limits of constant space */
SCHEME_OBJECT * constant_start;
SCHEME_OBJECT * constant_end;

/* Address of the most recent return code in the stack.
   This is only meaningful while in compiled code.  */
SCHEME_OBJECT * last_return_code;

SCHEME_OBJECT fixed_objects;

/* Array of contiguous auxiliary storage, one entry per ephemeron, for
   the sake of the garbage collector, which can use the array however
   it pleases -- as a hash table, binary tree, &c.  */

SCHEME_OBJECT ephemeron_array = SHARP_F;
unsigned long ephemeron_count = 0;

bool trapping;

unsigned long n_heap_blocks;
unsigned long n_constant_blocks;
unsigned long n_stack_blocks;
SCHEME_OBJECT * memory_block_start;
SCHEME_OBJECT * memory_block_end;

unsigned long heap_reserved;

/* Amount of space needed when GC requested */
unsigned long gc_space_needed;

#ifndef HEAP_IN_LOW_MEMORY
   SCHEME_OBJECT * memory_base;
#endif

#ifdef ENABLE_DEBUGGING_TOOLS
   bool Eval_Debug = false;
   bool Hex_Input_Debug = false;
   bool File_Load_Debug = false;
   bool Reloc_Debug = false;
   bool Intern_Debug = false;
   bool Cont_Debug = false;
   bool Primitive_Debug = false;
   bool Lookup_Debug = false;
   bool Define_Debug = false;
   bool GC_Debug = false;
   bool Upgrade_Debug = false;
   bool Dump_Debug = false;
   bool Trace_On_Error = false;
   bool Bignum_Debug = false;
   bool Per_File = false;
   unsigned int debug_slotno = 0;
   unsigned int debug_nslots = 0;
   unsigned int local_slotno = 0;
   unsigned int local_nslots = 0;
   unsigned int debug_circle [100];
   unsigned int local_circle [100];
#endif

const char * CONT_PRINT_RETURN_MESSAGE =   "SAVE_CONT, return code";
const char * CONT_PRINT_EXPR_MESSAGE   =   "SAVE_CONT, expression";
const char * RESTORE_CONT_RETURN_MESSAGE = "RESTORE_CONT, return code";
const char * RESTORE_CONT_EXPR_MESSAGE =   "RESTORE_CONT, expression";

/* Interpreter code name and message tables */

unsigned long MAX_RETURN = MAX_RETURN_CODE;

const char * Return_Names [] = RETURN_NAME_TABLE;	/* in returns.h */
const char * type_names [] = TYPE_NAME_TABLE;		/* in types.h */
const char * Abort_Names [] = ABORT_NAME_TABLE;		/* in const.h */
const char * Error_Names [] = ERROR_NAME_TABLE;		/* in errors.h */
const char * Term_Names [] = TERM_NAME_TABLE;		/* in errors.h */
const char * term_messages [] = TERM_MESSAGE_TABLE;	/* in errors.h */
const char * fixed_objects_names [] = FIXED_OBJECTS_NAMES; /* in fixobj.h */
