/* -*-C-*-

$Id: dostty.c,v 1.3 1992/10/07 06:23:35 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

#include "msdos.h"
#include "ostty.h"
#include "osenv.h"
#include "dosio.h"
#include "dosterm.h"

/* Standard Input and Output */

static Tchannel input_channel;
static Tchannel output_channel;
int tty_x_size;
int tty_y_size;
  /* 1-based values */
static char * tty_command_beep;
static char * tty_command_clear;

Tchannel
DEFUN_VOID (OS_tty_input_channel)
{
  return (input_channel);
}

Tchannel
DEFUN_VOID (OS_tty_output_channel)
{
  return (output_channel);
}

unsigned int
DEFUN_VOID (OS_tty_x_size)
{
  return (tty_x_size);
}

unsigned int
DEFUN_VOID (OS_tty_y_size)
{
  return (tty_y_size);
}

CONST char *
DEFUN_VOID (OS_tty_command_beep)
{
  return (tty_command_beep);
}

CONST char *
DEFUN_VOID (OS_tty_command_clear)
{
  return (tty_command_clear);
}

#ifndef TERMCAP_BUFFER_SIZE
#define TERMCAP_BUFFER_SIZE 0
#endif

#ifndef DEFAULT_TTY_X_SIZE
#define DEFAULT_TTY_X_SIZE 80
#endif

#ifndef DEFAULT_TTY_Y_SIZE
#define DEFAULT_TTY_Y_SIZE 25
#endif

void
pc_gestalt_screen_x_size (void)
{
  char *psTemp;

  psTemp = (getenv ("MITSCHEME_COLUMNS"));
  if (psTemp == NULL)
  {
    union REGS regs;

    regs.h.ah = 0x0F;
    regs.h.al = 0x00;
    int10h (&regs, &regs);
    tty_x_size = regs.h.ah;
  }
  else
  {
    tty_x_size = (atoi (psTemp));
    if (tty_x_size == 0)
      tty_x_size = DEFAULT_TTY_X_SIZE; /* atoi failed, use default */
  }
  return;
}

void
pc_gestalt_screen_y_size (void)
{
  char *psTemp;

  psTemp = (getenv ("MITSCHEME_LINES"));
  if (psTemp == NULL)
  {
    union REGS regs;

    regs.x.ax = 0x1130;
    regs.h.bh = 0x00;
    regs.h.dl = DEFAULT_TTY_Y_SIZE-1;
    int10h (&regs, &regs);
    tty_y_size = regs.h.dl + 1;
  }
  else
  {
    tty_y_size = (atoi (psTemp));
    if (tty_y_size == 0)
      tty_y_size = DEFAULT_TTY_Y_SIZE; /* atoi failed, use default */
  }
  return;
}

void
DEFUN_VOID (DOS_initialize_tty)
{
  extern Tchannel EXFUN (OS_open_fd, (int fd));
  input_channel = (OS_open_fd (STDIN_FILENO));
  (CHANNEL_INTERNAL (input_channel)) = 1;
  output_channel = (OS_open_fd (STDOUT_FILENO));
  (CHANNEL_INTERNAL (output_channel)) = 1;
  tty_x_size = (-1);
  tty_y_size = (-1);
  tty_command_beep = ALERT_STRING;
  tty_command_clear = "\033[2J";

  /* Figure out the size of the terminal. Tries environment variables.
     If that fails, use default values. */

  pc_gestalt_screen_x_size ();
  pc_gestalt_screen_y_size ();
  return;
}

/* Fake TERMCAP capability */
short ospeed;
char PC;

int 
tputs (string, nlines, outfun)
     register char * string;
     int nlines;
     register int (*outfun) ();
{
  register int padcount = 0;

  if (string == (char *) 0)
    return;
  while (*string >= '0' && *string <= '9')
  {
    padcount += *string++ - '0';
    padcount *= 10;
  }
  if (*string == '.')
  {
    string++;
    padcount += *string++ - '0';
  }
  if (*string == '*')
  {
    string++;
    padcount *= nlines;
  }
  while (*string)
    (*outfun) (*string++);

  return (0);
}
