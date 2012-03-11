/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

/* The scheme command parser.  */

#ifndef COMLIN_H_INCLUDED
#define COMLIN_H_INCLUDED

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
  void		*data;
  string	format;
  boolean	*supplied_p;
};

#define KEYWORD(str, var, type, format, sup) { type, str, var, format, sup }

#define END_KEYWORD() KEYWORD ("", 0, LAST_KYWRD, 0, 0)

/* Fake boolean and string formats */

#define BFRMT (0)
#define SFRMT (0)

/* Exports */

extern char * program_name;

extern void parse_keywords (int, char **, struct keyword_struct *, boolean);

#endif /* COMLIN_H_INCLUDED */
