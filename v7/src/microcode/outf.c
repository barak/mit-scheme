/* -*-C-*-

$Id: outf.c,v 1.3 1993/07/18 20:26:48 gjr Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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

#if defined(__STDC__) || defined(WINNT)
#include <stdarg.h>
#define VA_START(args, lastarg) va_start(args, lastarg)
#define VA_DCL
#else
#include <varargs.h>
#define VA_START(args, lastarg) va_start(args)
#define VA_DCL va_dcl
#endif

#include <stdio.h>
#include "scheme.h"

#ifdef WINNT
#include <windows.h>
#include "ntscreen.h"
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

#ifdef WINNT

static int max_fatal_buf = 1000;
static char fatal_buf[1000+1] = {0};

void
DEFUN (voutf_fatal, (format, args), CONST char *format AND va_list args)
{
    int end = strlen(fatal_buf);
    _vsnprintf (&fatal_buf[end], max_fatal_buf - end, format, args);
}


void  popup_outf_flush_fatal()
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
      _vsnprintf (buf, 1000, format, args);
      Screen_WriteText (master_tty_window, buf);
    } else {
      vfprintf (outf_channel_to_FILE(chan), format, args);
    }
}

#endif

void
DEFUN (voutf, (chan, format, ap),
       CONST outf_channel chan AND
       CONST char *format AND
       va_list ap)
{
#ifdef WINNT
         if (chan==fatal_output) voutf_fatal(format, ap);
    else if (chan==console_output) voutf_master_tty(chan, format, ap);
    else if (chan==error_output)   voutf_master_tty(chan, format, ap);
    else
#endif
    vfprintf(outf_channel_to_FILE(chan), format, ap);
}

void
DEFUN (outf_flush, (chan),  outf_channel chan)
{
#ifdef WINNT
    if (chan==fatal_output)  popup_outf_flush_fatal();
    else
#endif
    fflush(outf_channel_to_FILE(chan));
}

