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

/* The scheme command parser.  */

#include <stdio.h>
#ifndef toupper
#  include <ctype.h>
#endif

#include "comlin.h"

/* Some string utilities */

char *
remove_initial_substring (char * sub, char * str)
{
  for ( ; *sub != '\0'; sub++, str++)
    {
      if (*sub != *str)
	return (0);
    }
  return (str);
}

bool
STREQUAL (char * s1, char * s2)
{
  for ( ; *s1 != '\0'; s1++, s2++)
    {
      if (toupper(*s1) != toupper(*s2))
	return (false);
    }
  return (*s2 == '\0');
}

/* Usage information */

void
print_usage_and_exit (struct keyword_struct * options, int val)
{
  int i;

  fprintf(stderr, "usage: %s", program_name);

  if ((options[0].type_tag) == LAST_KYWRD)
    {
      fprintf(stderr, "\n");
      exit(val);
    }

  fprintf(stderr, " [args]\n");
  fprintf(stderr, "    where args are as follows:\n");

  for (i = 0;
       ((options[i].type_tag) != LAST_KYWRD);
       i++)
    {
      switch (options[i].type_tag)
	{
	case BOOLEAN_KYWRD:
	  fprintf(stderr, "        %s={true,false}\n",
		  options[i].keyword);
	  break;

	case INT_KYWRD:
	case DOUBLE_KYWRD:
	  fprintf(stderr, "        %s=%s\n",
		  options[i].keyword, options[i].format);
	  break;

	case STRING_KYWRD:
	  fprintf(stderr, "        %s=%%s\n",
		  options[i].keyword);
	  break;
	}
    }
  exit(val);
}

void
supply (struct keyword_struct * options, int j)
{
  if (options[j].supplied_p != 0)
  {
    if (*(options[j].supplied_p))
    {
      fprintf(stderr,
	      "parse_keywords: Repeated keyword: %s\n",
	      options[j].keyword);
      print_usage_and_exit(&options[0], 1);
    }
    else
    {
      *(options[j].supplied_p) = true;
    }
  }
  return;
}

char * program_name;

/* This parser assumes that no keyword is an initial substring of
   another.  */

void
parse_keywords (int argc,
		char **argv,
		struct keyword_struct * options,
		bool allow_others_p)
{
  int i, j, length;
  char * argument;

  program_name = argv[0];
  argv += 1;
  argc -= 1;

  /* Initialize defaults */

  for (length = 0;
       ((options[length].type_tag) != LAST_KYWRD);
       length++)
  {
    if (options[length].supplied_p != 0)
    {
      *(options[length].supplied_p) = false;
    }

    switch (options[length].type_tag)
    {
      case BOOLEAN_KYWRD:
        if (options[length].format != BFRMT)
	{
	  fprintf(stderr,
		  "parse_keywords: format (%s) for boolean keyword %s\n",
		  options[length].format,
		  options[length].keyword);
	  exit(1);
	}
	break;

      case INT_KYWRD:
	break;

      case DOUBLE_KYWRD:
	break;

      case STRING_KYWRD:
        if (options[length].format != SFRMT)
	{
	  fprintf(stderr,
		  "parse_keywords: format (%s) for string keyword %s\n",
		  options[length].format,
		  options[length].keyword);
	  exit(1);
	}
	break;

      default:
        fprintf(stderr, "parse_keywords: bad type %d\n",
		options[length].type_tag);
	exit(1);
    }
  }

  for (i = 0; i < argc; i++)
  {
    for (j = 0; j < length; j++)
    {
      argument = remove_initial_substring(options[j].keyword,argv[i]);
      if (argument != ((char *) NULL))
      {
	switch (options[j].type_tag)
	{

	  case BOOLEAN_KYWRD:
	  {
	    bool value = false;

	    if (*argument != '\0')
	    {
	      if (*argument != '=')
	      {
		fprintf(stderr,
			"parse_keywords: unrecognized parameter: %s\n",
			argv[i]);
		print_usage_and_exit(&options[0], 1);
		/*NOTREACHED*/
		value = false;
	      }
	      else
	      {
		argument = &argument[1];
		if (STREQUAL(argument,"t") || STREQUAL(argument,"true"))
		{
		  value = true;
		}
		else if (STREQUAL(argument,"f") ||
			 STREQUAL(argument,"false") ||
			 STREQUAL(argument,"nil"))
		{
		  value = false;
		}
		else
		{
		  fprintf(stderr,
			  "parse_keywords: Invalid boolean value: %s\n",
			  argv[i]);
		  print_usage_and_exit(&options[0], 1);
		  /*NOTREACHED*/
		  value = false;
		}
	      }
	    }
	    else
	    {
	      value = true;
	    }
	    supply(options, j);
	    *(BOOLEAN_LVALUE(options[j])) = value;
	    break;
	  }

	  case INT_KYWRD:
	    if (*argument != '=')
	    {
	      {
		fprintf(stderr,
			"parse_keywords: %s: %s\n",
			((*argument == '\0')	?
			 "missing integer value"	:
			 "unrecognized parameter"),
			argv[i]);
		print_usage_and_exit(&options[0], 1);
	      }
	    }
	    supply(options, j);
	    sscanf(&argument[1], options[j].format, INT_LVALUE(options[j]));
	    break;

	  case DOUBLE_KYWRD:
	    if (*argument != '=')
	    {
	      {
		fprintf(stderr,
			"parse_keywords: %s: %s\n",
			((*argument == '\0')		?
			 "missing floating point value"	:
			 "unrecognized parameter"),
			argv[i]);
		print_usage_and_exit(&options[0], 1);
	      }
	    }
	    supply(options, j);
	    sscanf(&argument[1], options[j].format, DOUBLE_LVALUE(options[j]));
	    break;

	  case STRING_KYWRD:
	    if (*argument != '=')
	    {
	      {
		fprintf(stderr,
			"parse_keywords: %s: %s\n",
			((*argument == '\0')	?
			 "missing string value"	:
			 "unrecognized parameter"),
			argv[i]);
		print_usage_and_exit(&options[0], 1);
	      }
	    }
	    supply(options, j);
	    *(STRING_LVALUE(options[j])) = &argument[1];
	    break;
	  }
	break;
      }
    }
    if ((j >= length) && (!allow_others_p))
    {
      fprintf(stderr,
	      "parse_keywords: unrecognized parameter: %s\n",
	      argv[i]);
      print_usage_and_exit(&options[0], 1);
    }
  }
}
