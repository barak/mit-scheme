/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/option.h,v 1.7 1992/02/18 20:34:47 mhwu Exp $

Copyright (c) 1990-1991 Massachusetts Institute of Technology

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

#ifndef SCM_OPTION_H
#define SCM_OPTION_H

#include "ansidecl.h"

extern int option_saved_argc;
extern CONST char ** option_saved_argv;
extern int option_unused_argc;
extern CONST char ** option_unused_argv;

/* Boolean options */
extern int option_emacs_subprocess;
extern int option_force_interactive;
extern int option_disable_core_dump;

/* String options */
extern CONST char ** option_library_path;
extern CONST char * option_band_file;
extern CONST char * option_fasl_file;
extern int option_band_specified;
extern CONST char * option_utabmd_file;

/* Numeric options */
extern unsigned int option_heap_size;
extern unsigned int option_constant_size;
extern unsigned int option_stack_size;

/* Meaningful only to bchscheme */

extern CONST char * option_gc_directory;
extern CONST char * option_gc_drone;
extern CONST char * option_gc_file;
extern int option_gc_keep;
extern int option_gc_read_overlap;
extern int option_gc_window_size;
extern int option_gc_write_overlap;
extern long option_gc_start_position;
extern long option_gc_end_position;

extern void EXFUN (read_command_line_options, (int argc, CONST char ** argv));

extern CONST char * EXFUN (search_for_library_file, (CONST char *));

extern CONST char * EXFUN
  (search_path_for_file,
   (CONST char * option, CONST char * filename, int default_p));

#ifdef DOS386
#define SUB_DIRECTORY_DELIMITER	'\\'
#else
#define SUB_DIRECTORY_DELIMITER '/'
#endif

#endif /* SCM_OPTION_H */
