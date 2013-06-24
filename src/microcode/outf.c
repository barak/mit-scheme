/* -*-C-*-

$Id: outf.c,v 1.19 2008/01/30 20:02:18 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

/* OUTF system

   outf_channel I/O is a substitute for <stdio.h>.  On text-based
   systems it is implemented in terms of stdio.  On windowing systems,
   however, we have to be able to report problems without having an
   obvious text output.

   There are three channels for output:

   CONSOLE_OUTPUT - for normal output to the user
   ERROR_OUTPUT   - for output of exceptional things
   FATAL_OUTPUT   - for details of an impending crash

   Use outf where you would normally think of using fprintf and
   outf_flush where you would normally use fflush.  */

#include "config.h"
#include "outf.h"

#ifdef __WIN32__
#  include <windows.h>
#  include "ntscreen.h"
   extern HANDLE master_tty_window;
#endif

#ifdef __OS2__
   extern char * OS2_thread_fatal_error_buffer (void);
   extern void OS2_message_box (const char *, const char *, int);
   extern void OS2_console_write (const char *, size_t);
#endif

void
outf (outf_channel chan, const char * format, ...)
{
    va_list ap;
    va_start (ap, format);
    voutf (chan, format, ap);
    va_end (ap);
}

void
voutf (outf_channel chan, const char * format, va_list ap)
{
  switch (chan)
    {
    case CONSOLE_OUTPUT: voutf_console (format, ap); break;
    case ERROR_OUTPUT: voutf_error (format, ap); break;
    case FATAL_OUTPUT: voutf_fatal (format, ap); break;
    }
}

void
outf_flush (outf_channel chan)
{
  switch (chan)
    {
    case CONSOLE_OUTPUT: outf_flush_console (); break;
    case ERROR_OUTPUT: outf_flush_error (); break;
    case FATAL_OUTPUT: outf_flush_fatal (); break;
    }
}

void
outf_console (const char * format, ...)
{
  va_list args;
  va_start (args, format);
  voutf_console (format, args);
  va_end (args);
}

void
outf_error (const char * format, ...)
{
  va_list args;
  va_start (args, format);
  voutf_error (format, args);
  va_end (args);
}

void
outf_fatal (const char * format, ...)
{
  va_list args;
  va_start (args, format);
  voutf_fatal (format, args);
  va_end (args);
}

#ifdef __WIN32__

#define OUTF_VARIANTS_DEFINED 1
#define MAX_FATAL_BUF 1000
static char fatal_buf [MAX_FATAL_BUF + 1] = { '\0' };

#ifdef CL386
#  define VSNPRINTF(buffer, length, format, args)			\
     _vsnprintf ((buffer), (length), (format), (args))
#else
#  ifdef __WATCOMC__
#    define VSNPRINTF(buffer, length, format, args)			\
       vsprintf ((buffer), (format), (args))
#  endif
#endif

static void
voutf_master_tty (const char * format, va_list args)
{
  char buf [1000];
  VSNPRINTF (buf, 1000, format, args);
  Screen_WriteText (master_tty_window, buf);
}

void
voutf_console (const char * format, va_list args)
{
  if (master_tty_window != 0)
    voutf_master_tty (format, args);
  else
    vfprintf (stdout, format, args);
}

void
outf_flush_console (void)
{
  if (master_tty_window == 0)
    fflush (stdout);
}

void
voutf_error (const char * format, va_list args)
{
  if (master_tty_window != 0)
    voutf_master_tty (format, args);
  else
    vfprintf (stderr, format, args);
}

void
outf_flush_error (void)
{
  if (master_tty_window == 0)
    fflush (stderr);
}

void
voutf_fatal (const char * format, va_list args)
{
  unsigned int end = (strlen (fatal_buf));
  VSNPRINTF ((& (fatal_buf[end])), (MAX_FATAL_BUF - end), format, args);
}

void
outf_flush_fatal (void)
{
  fprintf (stderr, "%s", fatal_buf);
  fflush (stderr);
  MessageBox
    (0, fatal_buf, "MIT/GNU Scheme terminating", (MB_OK | MB_TASKMODAL));
  (fatal_buf[0]) = '\0';
}

#endif /* __WIN32__ */

#ifdef __OS2__

#define OUTF_VARIANTS_DEFINED 1

void
voutf_console (const char * format, va_list args)
{
  char buffer [4096];
  vsprintf (buffer, format, args);
  OS2_console_write (buffer, (strlen (buffer)));
}

void
outf_flush_console (void)
{
}

void
voutf_error (const char * format, va_list args)
{
  voutf_console (format, args);
}

void
outf_flush_error (void)
{
}

void
voutf_fatal (const char * format, va_list args)
{
  char * buffer = (OS2_thread_fatal_error_buffer ());
  unsigned int end = (strlen (buffer));
  vsprintf ((& (buffer [end])), format, args);
}

void
outf_flush_fatal (void)
{
  char * buffer = (OS2_thread_fatal_error_buffer ());
  OS2_message_box ("MIT/GNU Scheme terminating", buffer, 1);
  (buffer[0]) = '\0';
}

#endif /* __OS2__ */

#ifndef OUTF_VARIANTS_DEFINED

void
voutf_console (const char * format, va_list args)
{
  vfprintf (stdout, format, args);
}

void
outf_flush_console (void)
{
  fflush (stdout);
}

void
voutf_error (const char * format, va_list args)
{
  vfprintf (stderr, format, args);
}

void
outf_flush_error (void)
{
  fflush (stderr);
}

void
voutf_fatal (const char * format, va_list args)
{
  vfprintf (stderr, format, args);
}

void
outf_flush_fatal (void)
{
  fflush (stderr);
}

#endif /* not OUTF_VARIANTS_DEFINED */
