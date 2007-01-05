/* -*-C-*-

$Id: outf.c,v 1.15 2007/01/05 15:33:07 cph Exp $

Copyright (c) 1993-2000 Massachusetts Institute of Technology

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

/*
  OUTF system
    
  outf_channel i/o is a substitute for <stdio.h>.  On text based unix-like
  systems it is implmented in terms of stdio.  On windowing systems, however,
  we have to be able to report problems withou having an obvious text output.
  
  There are three channels for output:
    
    console_output - for normal output to the user
    error_output   - for output of exceptional things
    fatal_output   - for details of an impending crash
      
  Use outf where you would normally think of using fprintf and outf_flush
  where you would normally use fflush.
    
  outf_flush(fatal_output) is special.  It causes the buffered fatal_output
  data to be displayed.  On windowing systems this may cause a window to be
  created to display the information, or allow the window containging the
  information to stay visible `after' the termination of Scheme.
*/

#include <stdio.h>
#include "scheme.h"

#ifdef STDC_HEADERS
#  include <string.h>
#  include <stdarg.h>
#  define VA_START(args, lastarg) va_start(args, lastarg)
#  define VA_DCL
#else
#  include <varargs.h>
#  define VA_START(args, lastarg) va_start(args)
#  define VA_DCL va_dcl
#endif

#ifdef __WIN32__
#  include <windows.h>
#  include "ntscreen.h"
#endif

/* forward reference */
extern void EXFUN
  (voutf, (CONST outf_channel chan, CONST char * format, va_list ap));

#define make_outf_variants(outputter,flusher,chan)			\
void									\
DEFUN (outputter, (format, va_alist), CONST char *format DOTS)		\
     VA_DCL								\
{									\
    va_list args;							\
    VA_START(args, format);						\
    voutf((chan), format, args);					\
}									\
void									\
DEFUN_VOID (flusher)							\
{									\
    outf_flush (chan);							\
}

make_outf_variants(outf_console, outf_flush_console, console_output)
make_outf_variants(outf_error,   outf_flush_error,   error_output)
make_outf_variants(outf_fatal,   outf_flush_fatal,   fatal_output)

void
DEFUN (outf, (chan, format, va_alist),
       outf_channel chan AND
       CONST char *format DOTS)
     VA_DCL
{
    va_list ap;
    VA_START(ap, format);
    voutf(chan, format, ap);
}

static FILE *
DEFUN (outf_channel_to_FILE, (chan), outf_channel chan)
{
    if (chan==fatal_output)   return  stderr;
    if (chan==error_output)   return  stderr;
    if (chan==console_output) return  stdout;
    return  (FILE*)chan;
}

#ifdef __WIN32__

#define USE_WINDOWED_OUTPUT
#define MAX_FATAL_BUF 1000
static char fatal_buf[MAX_FATAL_BUF + 1] = {0};

#ifdef CL386
#  define VSNPRINTF(buffer,length,format,args)				\
     _vsnprintf ((buffer), (length), (format), (args))
#else
#  ifdef __WATCOMC__
#    define VSNPRINTF(buffer,length,format,args)			\
       vsprintf ((buffer), (format), (args))
#  endif
#endif

void
DEFUN (voutf_fatal, (format, args), CONST char *format AND va_list args)
{
    int end = strlen(fatal_buf);
    VSNPRINTF (&fatal_buf[end], MAX_FATAL_BUF - end, format, args);
}

void
DEFUN_VOID (popup_outf_flush_fatal)
{
    fprintf(stderr,"%s", fatal_buf); fflush(stderr);
    MessageBox(0,fatal_buf,"MIT-Scheme terminating", MB_OK|MB_TASKMODAL);
    fatal_buf[0] = 0;
}

void
DEFUN (voutf_master_tty, (chan, format, args),
       outf_channel chan  AND  CONST char *format  AND  va_list args)
{
    extern HANDLE master_tty_window;
    char buf[1000];

    if (master_tty_window) {
      VSNPRINTF (buf, 1000, format, args);
      Screen_WriteText (master_tty_window, buf);
    } else {
      vfprintf (outf_channel_to_FILE(chan), format, args);
    }
}

#else /* not __WIN32__ */
#ifdef __OS2__

extern char * OS2_thread_fatal_error_buffer (void);
extern void OS2_message_box (const char *, const char *, int);

#define USE_WINDOWED_OUTPUT

static void
voutf_fatal (const char * format, va_list args)
{
  char * buffer = (OS2_thread_fatal_error_buffer ());
  unsigned int end = (strlen (buffer));
  vsprintf ((& (buffer [end])), format, args);
}

static void
popup_outf_flush_fatal (void)
{
  char * buffer = (OS2_thread_fatal_error_buffer ());
  OS2_message_box ("Scheme Terminating", buffer, 1);
  (buffer[0]) = '\0';
}

static void
voutf_master_tty (const outf_channel chan, const char * format, va_list args)
{
  extern void OS2_console_write (const char *, size_t);
  char buffer [4096];
  vsprintf (buffer, format, args);
  OS2_console_write (buffer, (strlen (buffer)));
}

#endif /* __OS2__ */
#endif /* not __WIN32__ */

void
DEFUN (voutf, (chan, format, ap),
       CONST outf_channel chan AND
       CONST char * format AND
       va_list ap)
{
#ifdef USE_WINDOWED_OUTPUT

  if (chan == fatal_output)
    voutf_fatal (format, ap);
  else if ((chan == console_output) || (chan == error_output))
    voutf_master_tty (chan, format, ap);
  else
#endif
    vfprintf ((outf_channel_to_FILE (chan)), format, ap);
}

void
DEFUN (outf_flush, (chan), outf_channel chan)
{
#ifdef USE_WINDOWED_OUTPUT
  if (chan == fatal_output)
    popup_outf_flush_fatal ();
  else
#endif
    fflush (outf_channel_to_FILE (chan));
}
