/* -*-C-*-

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

/* $Id: comlin.h,v 1.7 1999/01/02 06:11:34 cph Exp $
 *
 * This file contains definitions for the scheme command parser.
 *
 */

#ifndef COMLIN_H_INCLUDED
#define COMLIN_H_INCLUDED

#include "ansidecl.h"

#ifndef boolean
#  define boolean	int
#endif
#ifndef true
#  define true		1
#endif
#ifndef false
#  define false		0
#endif

typedef char *string;

/* Argument List Keyword Descriptor Structure */

#define LAST_KYWRD	0
#define BOOLEAN_KYWRD	1
#define INT_KYWRD	2
#define DOUBLE_KYWRD	3
#define STRING_KYWRD	4

#define BOOLEAN_LVALUE(struc)	((boolean *) ((struc).data))
#define INT_LVALUE(struc)	((int *) ((struc).data))
#define DOUBLE_LVALUE(struc)	((double *) ((struc).data))
#define STRING_LVALUE(struc)	((string *) ((struc).data))

struct keyword_struct
{
  int		type_tag;
  string	keyword;
  long		*data;
  string	format;
  boolean	*supplied_p;
};

#define KEYWORD(str, var, type, format, sup)				\
{									\
  type,									\
  ((string) str),							\
  ((long *) var),							\
  format,								\
  sup									\
}

#define END_KEYWORD()	KEYWORD("", NULL, LAST_KYWRD, NULL, NULL)

/* Fake boolean and string formats */

#define BFRMT	((string) NULL)
#define SFRMT	((string) NULL)

/* Exports */

extern char *program_name;

extern void EXFUN (parse_keywords,
		   (int, char **, struct keyword_struct *, boolean));

#endif /* COMLIN_H_INCLUDED */
