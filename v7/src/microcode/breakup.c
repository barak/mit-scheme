/* -*-C-*-

Copyright (c) 1987-91 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/breakup.c,v 9.23 1991/08/13 06:20:37 cph Exp $ */

#include <stdio.h>

#ifndef isdigit
#include <ctype.h>
#endif

#define boolean char
#define false 0
#define true 1

#define isoctal(c) (isdigit(c) && (c != '8') && (c != '9'))

int get_a_char()
{ register int c;
  register int count = 2;
  for (c = getchar();
       isoctal(c) && count >= 0;
       c = getchar(), count -=1)
    putchar(c);
  if (count != 2) return c;
  putchar(c);
  return getchar();
}

main()
{ register int c;
  register boolean after_new_line = true;
  while ((c = getchar()) != EOF)
re_dispatch:
    switch(c)
    { case '\f':
	break;
      case ',':
	putchar(c);
	while (((c = getchar()) == ' ') || (c == '\t'))
        if (c == EOF)
        { fprintf(stderr, "Confused expression: ,\n");
	  exit(1);
        }
	if (c == '\n')
	{ putchar(c);
	  after_new_line = true;
	  break;
	}
	putchar(' ');
	goto re_dispatch;
      case ';':
      case ':':
      case '?':
      case '}':
	putchar(c);
        putchar('\n');
	after_new_line = true;
        break;
      case '\n':
	if (!after_new_line)
	{ after_new_line = true;
	  putchar('\n');
        }
	break;
      case '\'':
	putchar(c);
	c = getchar();
	if (c == EOF)
	{ fprintf(stderr, "Confused character: EOF\n");
	  exit(1);
	}
	putchar(c);
	if (c == '\n')
	{ fprintf(stderr, "Confused character: \\n\n");
	  after_new_line = true;
	  break;
	}
	if (c == '\'')
	{ fprintf(stderr, "Confused character: \\\'\n");
	  break;
	}
	if (c == '\\')
	  c = get_a_char();
	else c = getchar();
	if (c == EOF)
	{ fprintf(stderr, "Confused character: EOF\n");
	  exit(1);
	}
	putchar(c);
	if (c != '\'')
	  fprintf(stderr, "Confused character: %c = 0x%x\n",
		  c);
	break;
      case '"':
	after_new_line = false;
	putchar(c);
	c = getchar();
	while (true)
	{ while ((c != EOF) &&
		 (c != '"') &&
		 (c != '\n') &&
		 (c != '\\'))
	  { putchar(c);
	    c = getchar();
	  }
	  if (c == EOF)
	  { fprintf(stderr, "Confused string: EOF\n");
	    exit(1);
	  }
	  putchar(c);
	  if (c == '\n')
	  { fprintf(stderr, "Confused string: \\n\n");
	    after_new_line = true;
	    break;
	  }
          if (c == '"') break;
	  if (c == '\\')
	    c = get_a_char();
	}
	break;
      case '#':
	if (after_new_line)
	{ while (((c = getchar()) != EOF) && (c != '\n')) ;
       	  if (c == EOF) exit(0);
	  break;
	}
	putchar(c);
	break;
      case '{':
	if (!after_new_line)
          putchar('\n');
        /* Fall Through */
      default:
	after_new_line = false;
	putchar(c);
    }
  fflush(stdout);
  exit(0);
}
