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

// Each return code is annotated with what their stack frame looks like.  This
// information effectively describes where the next return code is on the stack.

// Stack-frame abbreviations:

// CONT (<object>)
// +-------------+
// | return code |
// +-------------+
// | <object>    |
// +-------------+

// ENV_CONT
// +-------------+
// | return code |
// +-------------+
// | expression  |
// +-------------+
// | environment |
// +-------------+

// HISTORY_CONT
// +-------------+
// | return code |
// +-------------+
// | history     |
// +-------------+
// | offset      |
// +-------------+
// | (unused)    |
// +-------------+

// APPLY_FRAME
// +-------------+
// | header      |
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

#define RC_END_OF_COMPUTATION           0x00 // CONT (ignored?)
#define RC_JOIN_STACKLETS               0x01 // CONT (control point)
// unused                               0x02
#define RC_INTERNAL_APPLY               0x03 // CONT (ignored?) + APPLY_FRAME
// unused                               0x04
#define RC_RESTORE_HISTORY              0x05 // HISTORY_CONT
// unused                               0x06
// unused                               0x07
#define RC_EXECUTE_ASSIGNMENT_FINISH    0x08 // ENV_CONT
#define RC_EXECUTE_DEFINITION_FINISH    0x09 // ENV_CONT
#define RC_EXECUTE_ACCESS_FINISH        0x0A // CONT (expression)
// unused                               0x0B
#define RC_EXECUTE_SEQUENCE_FINISH      0x0C // ENV_CONT
// unused                               0x0D
// unused                               0x0E
#define RC_CONDITIONAL_DECIDE           0x0F // ENV_CONT
#define RC_DISJUNCTION_DECIDE           0x10 // ENV_CONT
// #define RC_COMB_1_PROCEDURE          0x11
#define RC_COMB_APPLY_FUNCTION          0x12 // CONT (expression) + APPLY_FRAME
// unused                               0x13
// unused                               0x14
#define RC_COMB_SAVE_VALUE              0x15 // ENV_CONT + n_args slots
// unused                               0x16 through 0x1B
#define RC_SNAP_NEED_THUNK              0x1C // CONT (delayed)
#define RC_REENTER_COMPILED_CODE        0x1D // CONT (offset)
// unused                               0x1E
// unused                               0x1F
#define RC_NORMAL_GC_DONE               0x20 // CONT (GC result)
// unused                               0x21 through 0x28
#define RC_POP_FROM_COMPILED_CODE       0x29 // ???
// unused                               0x2A through 0x2D
#define RC_RESTORE_VALUE                0x2E // CONT (value)
// HISTORY_CONT
#define RC_RESTORE_DONT_COPY_HISTORY    0x2F // HISTORY_CONT
// unused                               0x30 through 0x3F
#define RC_POP_RETURN_ERROR             0x40 // CONT (value)
#define RC_EVAL_ERROR                   0x41 // ENV_CONT
#define RC_STACK_MARKER                 0x42 // CONT (marker1) + marker2
#define RC_COMP_INTERRUPT_RESTART       0x43 // CONT (offset)
// unused                               0x44
#define RC_RESTORE_INT_MASK             0x45 // CONT (int mask)
#define RC_HALT                         0x46 // CONT (term code)
// unused                               0x47 through 0x52
#define RC_COMP_LOOKUP_TRAP_RESTART     0x53 // CONT (offset)
#define RC_COMP_ASSIGNMENT_TRAP_RESTART 0x54 // CONT (offset)
// unused                               0x55
#define RC_COMP_OP_REF_TRAP_RESTART     0x56 // CONT (offset)
#define RC_COMP_CACHE_REF_APPLY_RESTART 0x57 // CONT (offset)
#define RC_COMP_SAFE_REF_TRAP_RESTART   0x58 // CONT (offset)
#define RC_COMP_UNASSIGNED_TRAP_RESTART 0x59 // CONT (offset)
// unused                               0x5A
#define RC_COMP_LINK_CACHES_RESTART     0x5B // CONT (offset)
#define RC_HARDWARE_TRAP                0x5C // CONT (signal #) + 7 other things
#define RC_INTERNAL_APPLY_VAL           0x5D // CONT (ignored) + APPLY_FRAME
#define RC_COMP_ERROR_RESTART           0x5E // CONT (offset)

/* When adding return codes, add them to the table below as well! */

#define MAX_RETURN_CODE                 0x5E

#define RETURN_NAME_TABLE                                               \
{                                                                       \
/* 0x00 */              "non-existent-continuation",                    \
/* 0x01 */              "join-stacklets",                               \
/* 0x02 */              0,                                              \
/* 0x03 */              "internal-apply",                               \
/* 0x04 */              0,                                              \
/* 0x05 */              "restore-history",                              \
/* 0x06 */              0,                                              \
/* 0x07 */              0,                                              \
/* 0x08 */              "assignment-continue",                          \
/* 0x09 */              "definition-continue",                          \
/* 0x0a */              "access-continue",                              \
/* 0x0b */              0,                                              \
/* 0x0c */              "sequence-continue",                            \
/* 0x0d */              0,                                              \
/* 0x0e */              0,                                              \
/* 0x0f */              "conditional-decide",                           \
/* 0x10 */              "disjunction-decide",                           \
/* 0x11 */              0,                                              \
/* 0x12 */              "combination-apply",                            \
/* 0x13 */              0,                                              \
/* 0x14 */              0,                                              \
/* 0x15 */              "combination-save-value",                       \
/* 0x16 */              0,                                              \
/* 0x17 */              0,                                              \
/* 0x18 */              0,                                              \
/* 0x19 */              0,                                              \
/* 0x1a */              0,                                              \
/* 0x1b */              0,                                              \
/* 0x1c */              "force-snap-thunk",                             \
/* 0x1d */              "reenter-compiled-code",                        \
/* 0x1e */              0,                                              \
/* 0x1f */              0,                                              \
/* 0x20 */              "normal-garbage-collect-done",                  \
/* 0x21 */              0,                                              \
/* 0x22 */              0,                                              \
/* 0x23 */              0,                                              \
/* 0x24 */              0,                                              \
/* 0x25 */              0,                                              \
/* 0x26 */              0,                                              \
/* 0x27 */              0,                                              \
/* 0x28 */              0,                                              \
/* 0x29 */              "pop-from-compiled-code",                       \
/* 0x2a */              0,                                              \
/* 0x2b */              0,                                              \
/* 0x2c */              0,                                              \
/* 0x2d */              0,                                              \
/* 0x2e */              "restore-value",                                \
/* 0x2f */              "restore-dont-copy-history",                    \
/* 0x30 */              0,                                              \
/* 0x31 */              0,                                              \
/* 0x32 */              0,                                              \
/* 0x33 */              0,                                              \
/* 0x34 */              0,                                              \
/* 0x35 */              0,                                              \
/* 0x36 */              0,                                              \
/* 0x37 */              0,                                              \
/* 0x38 */              0,                                              \
/* 0x39 */              0,                                              \
/* 0x3a */              0,                                              \
/* 0x3b */              0,                                              \
/* 0x3c */              0,                                              \
/* 0x3d */              0,                                              \
/* 0x3e */              0,                                              \
/* 0x3f */              0,                                              \
/* 0x40 */              "pop-return-error",                             \
/* 0x41 */              "eval-error",                                   \
/* 0x42 */              "stack-marker",                                 \
/* 0x43 */              "compiler-interrupt-restart",                   \
/* 0x44 */              0,                                              \
/* 0x45 */              "restore-interrupt-mask",                       \
/* 0x46 */              "halt",                                         \
/* 0x47 */              0,                                              \
/* 0x48 */              0,                                              \
/* 0x49 */              0,                                              \
/* 0x4a */              0,                                              \
/* 0x4b */              0,                                              \
/* 0x4c */              0,                                              \
/* 0x4d */              0,                                              \
/* 0x4e */              0,                                              \
/* 0x4f */              0,                                              \
/* 0x50 */              0,                                              \
/* 0x51 */              0,                                              \
/* 0x52 */              0,                                              \
/* 0x53 */              "compiler-reference-trap-restart",              \
/* 0x54 */              "compiler-assignment-trap-restart",             \
/* 0x55 */              0,                                              \
/* 0x56 */              "compiler-operator-lookup-trap-restart",        \
/* 0x57 */              "compiler-lookup-apply-trap-restart",           \
/* 0x58 */              "compiler-safe-reference-trap-restart",         \
/* 0x59 */              "compiler-unassigned?-trap-restart",            \
/* 0x5a */              0,                                              \
/* 0x5b */              "compiler-link-caches-restart",                 \
/* 0x5c */              "hardware-trap",                                \
/* 0x5d */              "internal-apply-val",                           \
/* 0x5e */              "compiler-error-restart"                        \
}
