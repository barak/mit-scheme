/* -*-C-*-

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* $Id: breakup.c,v 9.26 2002/11/20 19:46:07 cph Exp $ */

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
