/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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

/* Return codes.  These are placed in GET_RET when an
   interpreter operation needs to operate in several phases. */

#ifndef SCM_RETURNS_H
#define SCM_RETURNS_H 1

#include "object.h"

#define RC_END_OF_COMPUTATION           0x00
#define RC_JOIN_STACKLETS               0x01
// unused                               0x02
#define RC_INTERNAL_APPLY               0x03
// unused                               0x04
#define RC_RESTORE_HISTORY              0x05
// unused                               0x06
// unused                               0x07
#define RC_EXECUTE_ASSIGNMENT_FINISH    0x08
#define RC_EXECUTE_DEFINITION_FINISH    0x09
#define RC_EXECUTE_ACCESS_FINISH        0x0A
// unused                               0x0B
#define RC_EXECUTE_SEQUENCE_FINISH      0x0C
// unused                               0x0D
// unused                               0x0E
#define RC_CONDITIONAL_DECIDE           0x0F
#define RC_DISJUNCTION_DECIDE           0x10
// #define RC_COMB_1_PROCEDURE          0x11
#define RC_COMB_APPLY_FUNCTION          0x12
// unused                               0x13
// unused                               0x14
#define RC_COMB_SAVE_VALUE              0x15
// unused                               0x16 through 0x1B
#define RC_SNAP_NEED_THUNK              0x1C
#define RC_REENTER_COMPILED_CODE        0x1D
// unused                               0x1E
// unused                               0x1F
#define RC_NORMAL_GC_DONE               0x20
// unused                               0x21 through 0x28
#define RC_POP_FROM_COMPILED_CODE       0x29 // should never appear on stack
// unused                               0x2A through 0x2D
#define RC_RESTORE_VALUE                0x2E
// HISTORY_CONT
#define RC_RESTORE_DONT_COPY_HISTORY    0x2F
// unused                               0x30 through 0x3F
#define RC_POP_RETURN_ERROR             0x40
#define RC_EVAL_ERROR                   0x41
#define RC_STACK_MARKER                 0x42
#define RC_COMP_INTERRUPT_RESTART       0x43
// unused                               0x44
#define RC_RESTORE_INT_MASK             0x45
#define RC_HALT                         0x46
// unused                               0x47 through 0x52
#define RC_COMP_LOOKUP_TRAP_RESTART     0x53
#define RC_COMP_ASSIGNMENT_TRAP_RESTART 0x54
// unused                               0x55
#define RC_COMP_OP_REF_TRAP_RESTART     0x56
#define RC_COMP_CACHE_REF_APPLY_RESTART 0x57
#define RC_COMP_SAFE_REF_TRAP_RESTART   0x58
#define RC_COMP_UNASSIGNED_TRAP_RESTART 0x59
// unused                               0x5A
#define RC_COMP_LINK_CACHES_RESTART     0x5B
#define RC_HARDWARE_TRAP                0x5C
#define RC_INTERNAL_APPLY_VAL           0x5D
#define RC_COMP_ERROR_RESTART           0x5E

// When adding return codes, add them to the tables in returns.c as well.

#define MAX_RETURN_CODE                 0x5E

typedef enum
{
  RFT_UNDEFINED,
  RFT_WITH_ARG,
  RFT_WITH_ARG_SUBPROBLEM,
  // +-------------+
  // | return code |
  // +-------------+
  // | <arg>       |
  // +-------------+
  RFT_EXP_ENV,
  // +-------------+
  // | return code |
  // +-------------+
  // | expression  |
  // +-------------+
  // | environment |
  // +-------------+
  RFT_HISTORY,
  // +-------------+
  // | return code |
  // +-------------+
  // | history     |
  // +-------------+
  // | offset      |
  // +-------------+
  // | (unused)    |
  // +-------------+
  RFT_APPLY,
  RFT_COMBINATION_APPLY,
  // +-------------+
  // | return code |
  // +-------------+
  // | ignored     |
  // +-------------+
  // | AF header   | (number of words to follow)
  // +-------------+
  // | procedure   |
  // +-------------+
  // | argument 0  |
  // +-------------+
  // | argument 1  |
  // +-------------+
  // .             .
  // .             .
  // .             .
  // +-------------+
  RFT_COMPILED_CODE,
  RFT_COMPILED_CODE_SUBPROBLEM,
  // +-------------+
  // | return code |
  // +-------------+
  // | offset      | (size of cc frames in words)
  // +-------------+
  // .             .
  // . cc frames   .
  // .             .
  // +-------------+
  RFT_COMPILED_ADDRESS,
  // +-------------+
  // | return addr |
  // +-------------+
  // .             .
  // . other words .
  // .             .
  // +-------------+
  RFT_COMBINATION_SAVE,
  // +-------------+
  // | return code |
  // +-------------+
  // | combination | (contains number of blanks + args)
  // +-------------+
  // | environment |
  // +-------------+
  // | NMV header  | (number of blanks to follow)
  // +-------------+
  // .             .
  // . blanks      .
  // .             .
  // +-------------+
  // .             .
  // . saved args  .
  // .             .
  // +-------------+
  RFT_STACK_MARKER,
  // +-------------+
  // | return code |
  // +-------------+
  // | marker 1    |
  // +-------------+
  // | marker 2    |
  // +-------------+
  RFT_HARDWARE_TRAP,
  // +-------------+
  // | return code |
  // +-------------+
  // | signo       |
  // +-------------+
  // | signal name |
  // +-------------+
  // | code name   |
  // +-------------+
  // | sp valid?   |
  // +-------------+
  // | state       |
  // +-------------+
  // | pc info 1   |
  // +-------------+
  // | pc info 2   |
  // +-------------+
  // | extra info  |
  // +-------------+
  RFT_RETURN_TO_INTERPRETER,
  // +-------------+
  // | return addr |
  // +-------------+
  RFT_CC_INTERNAL_APPLY,
  // +-------------+
  // | rti-address |
  // +-------------+
  // | code = 0    |
  // +-------------+
  // | AF header   | (number of words to follow)
  // +-------------+
  // | procedure   |
  // +-------------+
  // | argument 0  |
  // +-------------+
  // | argument 1  |
  // +-------------+
  // .             .
  // .             .
  // .             .
  // +-------------+
  RFT_CC_RESTORE_INTERRUPT_MASK,
  // +-------------+
  // | rti-address |
  // +-------------+
  // | code = 1    |
  // +-------------+
  // | int mask    |
  // +-------------+
  RFT_CC_STACK_MARKER,
  // +-------------+
  // | rti-address |
  // +-------------+
  // | code = 2    |
  // +-------------+
  // | marker 1    |
  // +-------------+
  // | marker 2    |
  // +-------------+
  RFT_CC_BKPT,
  // +-------------+
  // | rti-address |
  // +-------------+
  // | code = 3    |
  // +-------------+
  // | CC proc     |
  // +-------------+
  // | argument 0  |
  // +-------------+
  // | argument 1  |
  // +-------------+
  // .             .
  // .             .
  // .             .
  // +-------------+
  RFT_CC_INVOCATION,
  // +-------------+
  // | rti-address |
  // +-------------+
  // | code = 8    |
  // +-------------+
  // | CC proc     |
  // +-------------+
  // | argument 0  |
  // +-------------+
  // | argument 1  |
  // +-------------+
  // .             .
  // .             .
  // .             .
  // +-------------+
  RFT_LIMIT
} return_frame_type_t;

extern unsigned long MAX_RETURN;
extern SCHEME_OBJECT make_return_code_names_table (void);
extern SCHEME_OBJECT make_frame_type_info_table (void);
extern const char* return_code_name (SCHEME_OBJECT);
extern bool return_address_p (SCHEME_OBJECT);
extern return_frame_type_t return_frame_type (SCHEME_OBJECT*);
extern return_frame_type_t return_code_frame_type (SCHEME_OBJECT);
extern SCHEME_OBJECT* next_stack_frame (SCHEME_OBJECT*);
extern unsigned long next_stack_frame_offset (SCHEME_OBJECT*);

#endif                          // SCM_RETURNS_H
