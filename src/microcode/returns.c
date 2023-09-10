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

// Return codes

#include "scheme.h"
#include "history.h"

static const char* return_code_names_table[] =
{
/* 0x00 */              "end-of-computation",
/* 0x01 */              "join-stacklets",
/* 0x02 */              0,
/* 0x03 */              "internal-apply",
/* 0x04 */              0,
/* 0x05 */              "restore-history",
/* 0x06 */              0,
/* 0x07 */              0,
/* 0x08 */              "assignment-continue",
/* 0x09 */              "definition-continue",
/* 0x0a */              "access-continue",
/* 0x0b */              0,
/* 0x0c */              "sequence-continue",
/* 0x0d */              0,
/* 0x0e */              0,
/* 0x0f */              "conditional-decide",
/* 0x10 */              "disjunction-decide",
/* 0x11 */              0,
/* 0x12 */              "combination-apply",
/* 0x13 */              0,
/* 0x14 */              0,
/* 0x15 */              "combination-save-value",
/* 0x16 */              0,
/* 0x17 */              0,
/* 0x18 */              0,
/* 0x19 */              0,
/* 0x1a */              0,
/* 0x1b */              0,
/* 0x1c */              "force-snap-thunk",
/* 0x1d */              "reenter-compiled-code",
/* 0x1e */              0,
/* 0x1f */              0,
/* 0x20 */              "normal-garbage-collect-done",
/* 0x21 */              0,
/* 0x22 */              0,
/* 0x23 */              0,
/* 0x24 */              0,
/* 0x25 */              0,
/* 0x26 */              0,
/* 0x27 */              0,
/* 0x28 */              0,
/* 0x29 */              "pop-from-compiled-code",
/* 0x2a */              0,
/* 0x2b */              0,
/* 0x2c */              0,
/* 0x2d */              0,
/* 0x2e */              "restore-value",
/* 0x2f */              "restore-dont-copy-history",
/* 0x30 */              0,
/* 0x31 */              0,
/* 0x32 */              0,
/* 0x33 */              0,
/* 0x34 */              0,
/* 0x35 */              0,
/* 0x36 */              0,
/* 0x37 */              0,
/* 0x38 */              0,
/* 0x39 */              0,
/* 0x3a */              0,
/* 0x3b */              0,
/* 0x3c */              0,
/* 0x3d */              0,
/* 0x3e */              0,
/* 0x3f */              0,
/* 0x40 */              "pop-return-error",
/* 0x41 */              "eval-error",
/* 0x42 */              "stack-marker",
/* 0x43 */              "compiler-interrupt-restart",
/* 0x44 */              0,
/* 0x45 */              "restore-interrupt-mask",
/* 0x46 */              "halt",
/* 0x47 */              0,
/* 0x48 */              0,
/* 0x49 */              0,
/* 0x4a */              0,
/* 0x4b */              0,
/* 0x4c */              0,
/* 0x4d */              0,
/* 0x4e */              0,
/* 0x4f */              0,
/* 0x50 */              0,
/* 0x51 */              0,
/* 0x52 */              0,
/* 0x53 */              "compiler-reference-trap-restart",
/* 0x54 */              "compiler-assignment-trap-restart",
/* 0x55 */              0,
/* 0x56 */              "compiler-operator-lookup-trap-restart",
/* 0x57 */              "compiler-lookup-apply-trap-restart",
/* 0x58 */              "compiler-safe-reference-trap-restart",
/* 0x59 */              "compiler-unassigned?-trap-restart",
/* 0x5a */              0,
/* 0x5b */              "compiler-link-caches-restart",
/* 0x5c */              "hardware-trap",
/* 0x5d */              "internal-apply-val",
/* 0x5e */              "compiler-error-restart"
};

static const char* return_frame_type_names_table[] =
{
  0,
  "with-arg",
  "with-arg-subproblem",
  "exp+env",
  "history",
  "apply",
  "combination-apply",
  "return-to-compiled-code",
  "return-to-compiled-code-subproblem",
  "compiled-address",
  "combination-save",
  "stack-marker",
  "hardware-trap",
  "return-to-interpreter",
  "cc-internal-apply",
  "cc-restore-interrupt-mask",
  "cc-stack-marker",
  "cc-bpkt",
  "cc-invocation"
};

static return_frame_type_t return_frame_types_table[] =
{
  RFT_WITH_ARG,                 // 0x00 arg: ignored?
  RFT_WITH_ARG,                 // 0x01 arg: control point
  RFT_UNDEFINED,                // 0x02
  RFT_APPLY,                    // 0x03
  RFT_UNDEFINED,                // 0x04
  RFT_HISTORY,                  // 0x05
  RFT_UNDEFINED,                // 0x06
  RFT_UNDEFINED,                // 0x07
  RFT_EXP_ENV,                  // 0x08
  RFT_EXP_ENV,                  // 0x09
  RFT_WITH_ARG_SUBPROBLEM,      // 0x0a arg: expression
  RFT_UNDEFINED,                // 0x0b
  RFT_EXP_ENV,                  // 0x0c
  RFT_UNDEFINED,                // 0x0d
  RFT_UNDEFINED,                // 0x0e
  RFT_EXP_ENV,                  // 0x0f
  RFT_EXP_ENV,                  // 0x10
  RFT_UNDEFINED,                // 0x11
  RFT_COMBINATION_APPLY,        // 0x12
  RFT_UNDEFINED,                // 0x13
  RFT_UNDEFINED,                // 0x14
  RFT_COMBINATION_SAVE,         // 0x15
  RFT_UNDEFINED,                // 0x16
  RFT_UNDEFINED,                // 0x17
  RFT_UNDEFINED,                // 0x18
  RFT_UNDEFINED,                // 0x19
  RFT_UNDEFINED,                // 0x1a
  RFT_UNDEFINED,                // 0x1b
  RFT_WITH_ARG_SUBPROBLEM,      // 0x1c arg: delayed
  RFT_COMPILED_CODE,            // 0x1d
  RFT_UNDEFINED,                // 0x1e
  RFT_UNDEFINED,                // 0x1f
  RFT_WITH_ARG,                 // 0x20 arg: GC result
  RFT_UNDEFINED,                // 0x21
  RFT_UNDEFINED,                // 0x22
  RFT_UNDEFINED,                // 0x23
  RFT_UNDEFINED,                // 0x24
  RFT_UNDEFINED,                // 0x25
  RFT_UNDEFINED,                // 0x26
  RFT_UNDEFINED,                // 0x27
  RFT_UNDEFINED,                // 0x28
  RFT_UNDEFINED,                // 0x29
  RFT_UNDEFINED,                // 0x2a
  RFT_UNDEFINED,                // 0x2b
  RFT_UNDEFINED,                // 0x2c
  RFT_UNDEFINED,                // 0x2d
  RFT_WITH_ARG,                 // 0x2e arg: value
  RFT_HISTORY,                  // 0x2f
  RFT_UNDEFINED,                // 0x30
  RFT_UNDEFINED,                // 0x31
  RFT_UNDEFINED,                // 0x32
  RFT_UNDEFINED,                // 0x33
  RFT_UNDEFINED,                // 0x34
  RFT_UNDEFINED,                // 0x35
  RFT_UNDEFINED,                // 0x36
  RFT_UNDEFINED,                // 0x37
  RFT_UNDEFINED,                // 0x38
  RFT_UNDEFINED,                // 0x39
  RFT_UNDEFINED,                // 0x3a
  RFT_UNDEFINED,                // 0x3b
  RFT_UNDEFINED,                // 0x3c
  RFT_UNDEFINED,                // 0x3d
  RFT_UNDEFINED,                // 0x3e
  RFT_UNDEFINED,                // 0x3f
  RFT_WITH_ARG,                 // 0x40 arg: value
  RFT_EXP_ENV,                  // 0x41
  RFT_STACK_MARKER,             // 0x42
  RFT_COMPILED_CODE_SUBPROBLEM, // 0x43
  RFT_UNDEFINED,                // 0x44
  RFT_WITH_ARG,                 // 0x45 arg: interrupt mask
  RFT_WITH_ARG,                 // 0x46 arg: termination code
  RFT_UNDEFINED,                // 0x47
  RFT_UNDEFINED,                // 0x48
  RFT_UNDEFINED,                // 0x49
  RFT_UNDEFINED,                // 0x4a
  RFT_UNDEFINED,                // 0x4b
  RFT_UNDEFINED,                // 0x4c
  RFT_UNDEFINED,                // 0x4d
  RFT_UNDEFINED,                // 0x4e
  RFT_UNDEFINED,                // 0x4f
  RFT_UNDEFINED,                // 0x50
  RFT_UNDEFINED,                // 0x51
  RFT_UNDEFINED,                // 0x52
  RFT_COMPILED_CODE_SUBPROBLEM, // 0x53
  RFT_COMPILED_CODE_SUBPROBLEM, // 0x54
  RFT_UNDEFINED,                // 0x55
  RFT_COMPILED_CODE_SUBPROBLEM, // 0x56
  RFT_COMPILED_CODE_SUBPROBLEM, // 0x57
  RFT_COMPILED_CODE_SUBPROBLEM, // 0x58
  RFT_COMPILED_CODE_SUBPROBLEM, // 0x59
  RFT_UNDEFINED,                // 0x5a
  RFT_COMPILED_CODE,            // 0x5b
  RFT_HARDWARE_TRAP,            // 0x5c
  RFT_APPLY,                    // 0x5d
  RFT_COMPILED_CODE_SUBPROBLEM  // 0x5e
};

unsigned long MAX_RETURN = MAX_RETURN_CODE;

SCHEME_OBJECT
make_return_code_names_table (void)
{
  SCHEME_OBJECT table = allocate_vector (MAX_RETURN_CODE + 1, true);
  for (unsigned int rc = 0; rc <= MAX_RETURN_CODE; rc += 1)
    {
      const char* name = return_code_names_table[rc];
      vector_set (table, rc,
                  (name == 0) ? SHARP_F : char_pointer_to_symbol (name));
    }
  return table;
}

const char*
return_code_name (SCHEME_OBJECT ret)
{
  assert (RETURN_CODE_P (ret));
  unsigned long index = object_datum (ret);
  assert (index <= MAX_RETURN_CODE);
  return return_code_names_table[index];
}

bool
return_address_p (SCHEME_OBJECT object)
{
#ifdef CC_SUPPORT_P
  return RETURN_CODE_P (object) || CC_RETURN_P (object);
#else
  return RETURN_CODE_P (object);
#endif
}

return_frame_type_t
return_frame_type (SCHEME_OBJECT* frame)
{
  SCHEME_OBJECT ret = *frame;
  assert (return_address_p (ret));
#ifdef CC_SUPPORT_P
  if (CC_RETURN_P (ret))
    return (ret == return_to_interpreter)
           ? RFT_RETURN_TO_INTERPRETER
           : (ret == reflect_to_interface)
             ? reflect_to_interface_frame_type (frame)
             : RFT_COMPILED_ADDRESS;
  if (CC_ENTRY_P (ret))
    return RFT_COMPILED_ADDRESS;
#endif
  return return_code_frame_type (ret);
}

return_frame_type_t
return_code_frame_type (SCHEME_OBJECT ret)
{
  unsigned long rc = object_datum (ret);
  assert (rc <= MAX_RETURN_CODE);
  return return_frame_types_table[rc];
}

typedef struct
{
  unsigned int index;
  const char* name;
} field_defn_t;

static field_defn_t with_arg_fields [] =
  {
    { 1, 0 }
  };

static field_defn_t exp_env_fields [] =
  {
    { 1, "expression" },
    { 2, "environment" }
  };

static field_defn_t history_fields [] =
  {
    { 1, "history" },
    { 2, "previous-restore-history-offset" }
  };

static field_defn_t apply_fields [] =
  {
    { 3, "procedure" },
    { 4, "arguments" }
  };

static field_defn_t compiled_code_fields [] =
  {
    { 1, "last-return-code" }
  };

static field_defn_t combination_apply_fields [] =
  {
    { 1, "expression" },
    { 4, "arguments" }
  };

static field_defn_t combination_save_fields [] =
  {
    { 1, "expression" },
    { 2, "environment" },
    { 3, "number-of-blanks" },
    { 4, "saved-args" }
  };

static field_defn_t stack_marker_fields [] =
  {
    { 1, "marker-type" },
    { 2, "marker-instance" }
  };

static field_defn_t hardware_trap_fields [] =
  {
    { 1, "signal-number" },
    { 2, "signal-name" },
    { 3, "code-name" },
    { 4, "sp-valid?" },
    { 5, "recovery-state" },
    { 6, "pc-info-1" },
    { 7, "pc-info-2" },
    { 8, "extra-info" }
  };

static field_defn_t cc_int_mask_fields [] =
  {
    { 2, "interrupt-mask" }
  };

static field_defn_t cc_stack_marker_fields [] =
  {
    { 2, "marker-type" },
    { 3, "marker-instance" }
  };

static field_defn_t cc_invocation_fields [] =
  {
    { 2, "procedure" },
    { 3, "arguments" }
  };

#define FTIE(type, subp, hsubp, n_fields, fields)                       \
{                                                                       \
  vector_set (table, type,                                              \
              allocate_ftti_entry                                       \
                (type, subp, hsubp, n_fields, fields));                 \
}

static SCHEME_OBJECT
allocate_ftti_entry (return_frame_type_t type, bool subp, bool history_subp,
                     unsigned int n_fields, field_defn_t* fields)
{
  SCHEME_OBJECT entry = allocate_vector ((2 * n_fields) + 3, true);
  unsigned int i = 0;
  vector_set (entry, i++,
              char_pointer_to_symbol (return_frame_type_names_table[type]));
  vector_set (entry, i++, BOOLEAN_TO_OBJECT (subp));
  vector_set (entry, i++, BOOLEAN_TO_OBJECT (history_subp));
  for (unsigned int j = 0; j < n_fields; j += 1)
    {
      vector_set (entry, i++,
                  (fields[j].name == 0)
                  ? SHARP_F
                  : char_pointer_to_symbol (fields[j].name));
      vector_set (entry, i++, ULONG_TO_FIXNUM (fields[j].index));
    }
  return entry;
}

SCHEME_OBJECT
make_frame_type_info_table (void)
{
  SCHEME_OBJECT table = allocate_vector (RFT_LIMIT, true);
  vector_set (table, RFT_UNDEFINED, SHARP_F);
  FTIE (RFT_WITH_ARG, false, false, 1, with_arg_fields);
  FTIE (RFT_WITH_ARG_SUBPROBLEM, true, true, 1, with_arg_fields);
  FTIE (RFT_EXP_ENV, true, true, 2, exp_env_fields);
  FTIE (RFT_HISTORY, false, false, 2, history_fields);
  FTIE (RFT_APPLY, true, false, 2, apply_fields);
  FTIE (RFT_COMBINATION_APPLY, true, true, 2, combination_apply_fields);
  FTIE (RFT_COMPILED_CODE, false, true, 1, compiled_code_fields);
  FTIE (RFT_COMPILED_CODE_SUBPROBLEM, true, true, 1, compiled_code_fields);
  FTIE (RFT_COMPILED_ADDRESS, true, false, 0, 0);
  FTIE (RFT_COMBINATION_SAVE, true, true, 4, combination_save_fields);
  FTIE (RFT_STACK_MARKER, false, false, 2, stack_marker_fields);
  FTIE (RFT_HARDWARE_TRAP, true, false, 8, hardware_trap_fields);
  FTIE (RFT_RETURN_TO_INTERPRETER, false, true, 0, 0);
  FTIE (RFT_CC_INTERNAL_APPLY, false, false, 2, apply_fields);
  FTIE (RFT_CC_RESTORE_INTERRUPT_MASK, false, false, 1, cc_int_mask_fields);
  FTIE (RFT_CC_STACK_MARKER, false, false, 2, cc_stack_marker_fields);
  FTIE (RFT_CC_BKPT, true, false, 2, cc_invocation_fields);
  FTIE (RFT_CC_INVOCATION, false, false, 2, cc_invocation_fields);
  return table;
}

unsigned long
cpoint_next_frame (SCHEME_OBJECT cpoint, unsigned long index)
{
  SCHEME_OBJECT* frame = vector_loc (cpoint, index);
  switch (return_frame_type (frame))
    {
    case RFT_WITH_ARG:
    case RFT_WITH_ARG_SUBPROBLEM:
      return index + CONT_SIZE;

    case RFT_EXP_ENV:
      return index + ENV_CONT_SIZE;

    case RFT_HISTORY:
      return index + HISTORY_CONT_SIZE;

    case RFT_STACK_MARKER:
      return index + CONT_SIZE + 1;

    case RFT_HARDWARE_TRAP:
      return index + CONT_SIZE + 7;

    case RFT_APPLY:
    case RFT_COMBINATION_APPLY:
    case RFT_CC_INTERNAL_APPLY:
      {
        SCHEME_OBJECT header = apply_frame_ptr_header (frame + CONT_SIZE);
        assert (apply_frame_header_p (header));
        return index + CONT_SIZE + 1 + apply_frame_header_size (header);
      }

    case RFT_COMBINATION_SAVE:
      {
        SCHEME_OBJECT exp = cont_frame_exp (frame);
        assert (combination_p (exp));
        return index + ENV_CONT_SIZE + combination_size (exp);
      }

#ifdef CC_SUPPORT_P
    case RFT_COMPILED_CODE:
    case RFT_COMPILED_CODE_SUBPROBLEM:
      return cpoint_compiled_code_next (cpoint, index);

    case RFT_COMPILED_ADDRESS:
      return cpoint_compiled_address_next (cpoint, index);

    case RFT_RETURN_TO_INTERPRETER:
      return index + 1;

    case RFT_CC_RESTORE_INTERRUPT_MASK:
    case RFT_CC_STACK_MARKER:
    case RFT_CC_BKPT:
    case RFT_CC_INVOCATION:
      return cpoint_reflect_to_interface_next (cpoint, index);
#endif

    default:
      return ULONG_MAX;
    }
}
