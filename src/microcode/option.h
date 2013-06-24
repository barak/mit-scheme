/* -*-C-*-

$Id: option.h,v 1.12 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

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
extern int option_empty_list_eq_false;

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
   (CONST char * option, CONST char * filename, int default_p, int fail_p));

#endif /* SCM_OPTION_H */
