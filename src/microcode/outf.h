/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

#ifndef SCM_OUTF_H
#define SCM_OUTF_H 1

#include "config.h"

#include <stdio.h>

typedef struct
{
  enum { OUTF_CONSOLE, OUTF_ERROR, OUTF_FATAL, OUTF_FILE } type;
  void * cookie;
} outf_channel;

extern outf_channel CONSOLE_OUTPUT;
extern outf_channel ERROR_OUTPUT;
extern outf_channel FATAL_OUTPUT;
extern outf_channel FILE_OUTPUT (FILE *);

extern void outf (outf_channel, const char *, ...)
  PRINTFLIKE (2, 3);

extern void outf_console (const char *, ...)
  PRINTFLIKE (1, 2);

extern void outf_error (const char *, ...)
  PRINTFLIKE (1, 2);

extern void outf_fatal (const char *, ...)
  PRINTFLIKE (1, 2);

extern void outf_error_line (const char *, ...)
  PRINTFLIKE (1, 2);

extern void voutf (outf_channel, const char *, va_list);
extern void voutf_console (const char *, va_list);
extern void voutf_error (const char *, va_list);
extern void voutf_fatal (const char *, va_list);
extern void voutf_error_line (const char *, va_list);

extern void outf_flush (outf_channel chan);
extern void outf_flush_console (void);
extern void outf_flush_error (void);
extern void outf_flush_fatal (void);

#endif /* not SCM_OUTF_H */
