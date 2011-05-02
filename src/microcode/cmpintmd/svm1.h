/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* Compiled code interface macros for SVM v1. */

#ifndef SCM_CMPINTMD_H_INCLUDED
#define SCM_CMPINTMD_H_INCLUDED 1

#define ASM_RESET_HOOK initialize_svm1

#define COMPILER_REGBLOCK_N_FIXED 512

typedef byte_t insn_t;

/* Number of insn_t units preceding entry address in which header
   (type and offset info) is stored.  */
#define CC_ENTRY_HEADER_SIZE (CC_ENTRY_TYPE_SIZE + CC_ENTRY_OFFSET_SIZE)
#define CC_ENTRY_TYPE_SIZE 2
#define CC_ENTRY_OFFSET_SIZE 2

/* Number of insn_t units preceding entry header in which GC trap
   instructions are stored.  */
#define CC_ENTRY_GC_TRAP_SIZE 0

/* Size of closure count in insn_t units. */
#define CLOSURE_COUNT_SIZE 2

/* Offset of first (canonical) closure entry point in insn_t units. */
#define CLOSURE_ENTRY_OFFSET SIZEOF_SCHEME_OBJECT

/* Offset to start of first closure entry -- the entry type (format) word. */
#define CLOSURE_ENTRY_START (CLOSURE_ENTRY_OFFSET - CC_ENTRY_TYPE_SIZE)

/* Size of closure entry in insn_t units.  */
#define CLOSURE_ENTRY_SIZE 5

/* Size of execution cache in SCHEME_OBJECTS.  */
#define UUO_LINK_SIZE 2
#define READ_UUO_TARGET(a, r) read_uuo_target (a)

#define UTILITY_RESULT_DEFINED 1

typedef struct
{
  bool scheme_p;
  union { long interpreter_code; insn_t * new_pc; } arg;
} utility_result_t;

#define RETURN_TO_C(code) do						\
{									\
  (DSU_result->scheme_p) = false;					\
  ((DSU_result->arg) . interpreter_code) = (code);			\
  return;								\
} while (false)

#define RETURN_TO_SCHEME(ep) do						\
{									\
  (DSU_result->scheme_p) = true;					\
  ((DSU_result->arg) . new_pc) = (ep);					\
  return;								\
} while (false)

#define ENTER_SCHEME(ep) return (C_to_interface (ep))

extern long C_to_interface (void *);
extern void initialize_svm1 (void);
extern insn_t * read_uuo_target (SCHEME_OBJECT *);
extern unsigned int read_u16 (insn_t *);

#endif /* !SCM_CMPINTMD_H_INCLUDED */
