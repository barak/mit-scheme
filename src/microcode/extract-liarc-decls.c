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

/* Utility to extract LIARC declarations from source files.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static void process_file (const char *);
static const char * filename_prefix (const char *);
static const char * apply_prefix_rules (const char *);
static void mangle_line (const char *, const char *);
static const char * skip_name (const char *);
static const char * skip_lws (const char *);
static const char * skip_fixed (const char *, char);
static void write_string (const char *);
static void write_char (char);
static const char * read_line (FILE *);
static void * xmalloc (size_t);
static void * xrealloc (void *, size_t);

typedef struct
{
  const char * pattern;
  const char * replacement;
} prefix_rule_t;

static unsigned int n_prefix_rules;
static unsigned int prefix_rules_size;
static prefix_rule_t * prefix_rules;

int
main (int argc, const char ** argv)
{
  const char ** scan = (argv + 1);
  const char ** end = (argv + argc);

  n_prefix_rules = 0;
  prefix_rules_size = 16;
  prefix_rules = (xmalloc (prefix_rules_size * (sizeof (prefix_rule_t))));

  while ((scan < end) && ((strcmp ((*scan), "--rewrite")) == 0))
    {
      if ((scan + 3) > end)
	abort ();
      if (n_prefix_rules == prefix_rules_size)
	{
	  prefix_rules_size *= 2;
	  prefix_rules
	    = (xrealloc (prefix_rules,
			 (prefix_rules_size * (sizeof (prefix_rule_t)))));
	}
      ((prefix_rules[n_prefix_rules]) . pattern) = (scan[1]);
      ((prefix_rules[n_prefix_rules]) . replacement) = (scan[2]);
      n_prefix_rules += 1;
      scan += 3;
    }

  while (scan < end)
    {
      if ((strcmp ((*scan), "--rewrite")) == 0)
	abort ();
      process_file (*scan++);
    }

  return (0);
}

static void
process_file (const char * filename)
{
  const char * prefix_from_file;
  const char * prefix_to_use;
  FILE * s;

  prefix_from_file = (filename_prefix (filename));
  prefix_to_use
    = (apply_prefix_rules ((prefix_from_file == 0)
			   ? ""
			   : prefix_from_file));

  s = (fopen (filename, "r"));
  if (s == 0)
    abort ();

  while (1)
    {
      const char * line = (read_line (s));
      if (line == 0)
	break;
      if (((strncmp (line, "DECLARE_COMPILED_", 17)) == 0)
	  || ((strncmp (line, "DECLARE_DATA_OBJECT", 19)) == 0))
	mangle_line (line, prefix_to_use);
      free ((void *) line);
    }

  if (prefix_from_file != 0)
    free ((void *) prefix_from_file);
  fclose (s);
}

static const char *
filename_prefix (const char * filename)
{
  const char * p = (strrchr (filename, '/'));
  if (p == 0)
    return (0);
  {
    unsigned int n = ((p + 1) - filename);
    char * prefix = (xmalloc (n + 1));
    strncpy (prefix, filename, n);
    (prefix[n]) = '\0';
    return (prefix);
  }
}

static const char *
apply_prefix_rules (const char * prefix)
{
  unsigned int index;

  for (index = 0; (index < n_prefix_rules); index += 1)
    if ((strcmp (((prefix_rules[index]) . pattern), prefix)) == 0)
      return ((prefix_rules[index]) . replacement);
  return (prefix);
}

static void
mangle_line (const char * line, const char * prefix)
{
  const char * scan = (skip_name (line));
  scan = (skip_lws (scan));
  scan = (skip_fixed (scan, '('));
  scan = (skip_lws (scan));
  scan = (skip_fixed (scan, '"'));
  write_string (prefix);
  write_string (scan);
  write_char ('\n');
  fflush (stdout);
}

static const char *
skip_name (const char * scan)
{
  while ((isalnum (*scan)) || ((*scan) == '_'))
    write_char (*scan++);
  return (scan);
}

static const char *
skip_lws (const char * scan)
{
  while (((*scan) == ' ') || ((*scan) == '\t'))
    write_char (*scan++);
  return (scan);
}

static const char *
skip_fixed (const char * scan, char c)
{
  if ((*scan) != c)
    abort ();
  write_char (*scan++);
  return (scan);
}

static void
write_string (const char * s)
{
  while (1)
    {
      char c = (*s++);
      if (c == '\0')
	break;
      write_char (c);
    }
}

static void
write_char (char c)
{
  if ((putc (c, stdout)) == EOF)
    abort ();
}

static const char *
read_line (FILE * s)
{
  size_t index = 0;
  size_t buffer_size = 16;
  char * buffer = (xmalloc (buffer_size));

  while (1)
    {
      int c = (getc (s));
      if (c == EOF)
	{
	  if (!feof (s))
	    abort ();
	  if (index == 0)
	    return (0);
	  break;
	}
      if (c == '\n')
	break;
      if (index == buffer_size)
	{
	  buffer_size *= 2;
	  buffer = (xrealloc (buffer, buffer_size));
	}
      (buffer[index++]) = c;
    }

  if (index == buffer_size)
    {
      buffer_size += 1;
      buffer = (xrealloc (buffer, buffer_size));
    }
  (buffer[index++]) = '\0';

  if (index < buffer_size)
    buffer = (xrealloc (buffer, index));

  return (buffer);
}

static void *
xmalloc (size_t n)
{
  void * p = (malloc (n));
  if (p == 0)
    abort ();
  return (p);
}

static void *
xrealloc (void * p, size_t n)
{
  void * p2 = (realloc (p, n));
  if (p2 == 0)
    abort ();
  return (p2);
}
