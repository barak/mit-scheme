/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

/* Compiled code interface macros for C "native" code. */

#ifndef SCM_CMPINTMD_H_INCLUDED
#define SCM_CMPINTMD_H_INCLUDED 1

/*

Structure of code block (prior to link):

+-----+-------------------------+
| MV  |		length		|
+-----+-------------------------+
| NMV |		  2N		|
+-----+-------------------------+
|	 format/offset1		|
+-------------------------------+
|	     index1		|
+-------------------------------+
.				.
.				.
+-------------------------------+
|	 format/offsetN		|
+-------------------------------+
|	     indexN		|
+-----+---------+---------------+
| FIX |   UUO	|      2M	|  (not TC_LINKAGE_SECTION)
+-----+---------+---------------+
|	      name1		|
+-------------------------------+
|	     nargs1		|
+-------------------------------+
.				.
.				.
+-------------------------------+
|	      nameM		|
+-------------------------------+
|	     nargsM		|
+-----+---------+---------------+
| FIX |  REFS	|	L	|  (not TC_LINKAGE_SECTION)
+-----+---------+---------------+
|	      name1		|
+-------------------------------+
.				.
.				.
+-------------------------------+
|	      nameL		|
+-----+---------+---------------+
| FIX |  ASNS	|	K	|  (not TC_LINKAGE_SECTION)
+-----+---------+---------------+
|	      name1		|
+-------------------------------+
.				.
.				.
+-------------------------------+
|	      nameK		|
+-----+---------+---------------+
| FIX |  GUUO	|      2J	|  (not TC_LINKAGE_SECTION)
+-----+---------+---------------+
|	      name1		|
+-------------------------------+
|	     nargs1		|
+-------------------------------+
.				.
.				.
+-------------------------------+
|	      nameJ		|
+-------------------------------+
|	     nargsJ		|
+-------------------------------+
|	     const1		|
+-------------------------------+
.				.
.				.
+-------------------------------+
|	     constI		|
+-------------------------------+

length = 2N + 2M + L + K + 2J + I + p(M) + p(L) + p(K) + p(J) + 1
   where p(x) = ((x > 0) ? 1 : 0)

format/offset is raw integer, of which only LS 32 bits are used:

     3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
     1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    +-----------------------------+-+---------------+---------------+
    |          offset             |C|    fmthigh    |    fmtlow     |
    +-----------------------------+-+---------------+---------------+

offset field is in insn_t units.

The indexes refer to entries in the compiled_entries array.

*/

#define ASM_RESET_HOOK initialize_C_interface

typedef SCHEME_OBJECT insn_t;

/* Number of insn_t units preceding entry address in which header
   (type and offset info) is stored.  */
#define CC_ENTRY_HEADER_SIZE 1

/* Number of insn_t units preceding entry header in which GC trap
   instructions are stored.  */
#define CC_ENTRY_GC_TRAP_SIZE 0

/* Size of execution cache in SCHEME_OBJECTS.  */
#define UUO_LINK_SIZE 2
#define READ_UUO_TARGET(a, r) read_uuo_target (a)

#undef CMPINT_USE_STRUCS

#define EMBEDDED_CLOSURE_ADDRS_P 1
#define READ_COMPILED_CLOSURE_TARGET(a, r) (read_compiled_closure_target (a))

extern void initialize_C_interface (void);
extern SCHEME_OBJECT initialize_C_compiled_block (const char *);
extern insn_t * read_uuo_target (SCHEME_OBJECT *);
extern insn_t * read_compiled_closure_target (insn_t *);

extern unsigned long c_code_table_export_length (unsigned long *);
extern void export_c_code_table (SCHEME_OBJECT *);
extern void reset_c_code_table (void);
extern bool import_c_code_table (SCHEME_OBJECT *, unsigned long);

#endif /* !SCM_CMPINTMD_H_INCLUDED */
