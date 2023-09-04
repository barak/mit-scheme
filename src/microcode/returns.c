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
  "exp+env",
  "history",
  "apply",
  "return-to-compiled-code",
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
  RFT_WITH_ARG,                 // 0x0a arg: expression
  RFT_UNDEFINED,                // 0x0b
  RFT_EXP_ENV,                  // 0x0c
  RFT_UNDEFINED,                // 0x0d
  RFT_UNDEFINED,                // 0x0e
  RFT_EXP_ENV,                  // 0x0f
  RFT_EXP_ENV,                  // 0x10
  RFT_UNDEFINED,                // 0x11
  RFT_APPLY,                    // 0x12
  RFT_UNDEFINED,                // 0x13
  RFT_UNDEFINED,                // 0x14
  RFT_COMBINATION_SAVE,         // 0x15
  RFT_UNDEFINED,                // 0x16
  RFT_UNDEFINED,                // 0x17
  RFT_UNDEFINED,                // 0x18
  RFT_UNDEFINED,                // 0x19
  RFT_UNDEFINED,                // 0x1a
  RFT_UNDEFINED,                // 0x1b
  RFT_WITH_ARG,                 // 0x1c arg: delayed
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
  RFT_COMPILED_CODE,            // 0x43
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
  RFT_COMPILED_CODE,            // 0x53
  RFT_COMPILED_CODE,            // 0x54
  RFT_UNDEFINED,                // 0x55
  RFT_COMPILED_CODE,            // 0x56
  RFT_COMPILED_CODE,            // 0x57
  RFT_COMPILED_CODE,            // 0x58
  RFT_COMPILED_CODE,            // 0x59
  RFT_UNDEFINED,                // 0x5a
  RFT_COMPILED_CODE,            // 0x5b
  RFT_HARDWARE_TRAP,            // 0x5c
  RFT_APPLY,                    // 0x5d
  RFT_COMPILED_CODE             // 0x5e
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
#endif
  unsigned long index = object_datum (ret);
  assert (index <= MAX_RETURN_CODE);
  return return_frame_types_table[index];
}

static SCHEME_OBJECT
allocate_ftti_entry (return_frame_type_t type, unsigned long n)
{
  SCHEME_OBJECT entry = allocate_vector ((2 * n) + 1, true);
  vector_set (entry, 0,
              char_pointer_to_symbol (return_frame_type_names_table[type]));
  return entry;
}

static SCHEME_OBJECT
make_ftti_entry (return_frame_type_t type)
{
  SCHEME_OBJECT entry;
  unsigned long i = 1;
  switch (type)
    {
    case RFT_UNDEFINED:
      return SHARP_F;

    case RFT_WITH_ARG:
      entry = allocate_ftti_entry (type, 1);
      vector_set (entry, i++, SHARP_F);
      vector_set (entry, i, ULONG_TO_FIXNUM (1));
      return entry;

    case RFT_EXP_ENV:
      entry = allocate_ftti_entry (type, 2);
      vector_set (entry, i++, char_pointer_to_symbol ("expression"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (1));
      vector_set (entry, i++, char_pointer_to_symbol ("environment"));
      vector_set (entry, i, ULONG_TO_FIXNUM (2));
      return entry;

    case RFT_HISTORY:
      entry = allocate_ftti_entry (type, 2);
      vector_set (entry, i++, char_pointer_to_symbol ("history"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (1));
      vector_set (entry, i++, char_pointer_to_symbol ("next-history-offset"));
      vector_set (entry, i, ULONG_TO_FIXNUM (2));
      return entry;

    case RFT_APPLY:
    case RFT_CC_INTERNAL_APPLY:
      entry = allocate_ftti_entry (type, 2);
      vector_set (entry, i++, char_pointer_to_symbol ("procedure"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (3));
      vector_set (entry, i++, char_pointer_to_symbol ("arguments"));
      vector_set (entry, i, ULONG_TO_FIXNUM (4));
      return entry;

    case RFT_COMPILED_CODE:
      entry = allocate_ftti_entry (type, 1);
      vector_set (entry, i++, char_pointer_to_symbol ("offset"));
      vector_set (entry, i, ULONG_TO_FIXNUM (1));
      return entry;

    case RFT_COMPILED_ADDRESS:
      return allocate_ftti_entry (type, 0);

    case RFT_COMBINATION_SAVE:
      entry = allocate_ftti_entry (type, 4);
      vector_set (entry, i++, char_pointer_to_symbol ("expression"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (1));
      vector_set (entry, i++, char_pointer_to_symbol ("environment"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (2));
      vector_set (entry, i++, char_pointer_to_symbol ("number-of-blanks"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (3));
      vector_set (entry, i++, char_pointer_to_symbol ("saved-args"));
      vector_set (entry, i, ULONG_TO_FIXNUM (4));
      return entry;

    case RFT_STACK_MARKER:
      entry = allocate_ftti_entry (type, 2);
      vector_set (entry, i++, char_pointer_to_symbol ("marker-type"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (1));
      vector_set (entry, i++, char_pointer_to_symbol ("marker-instance"));
      vector_set (entry, i, ULONG_TO_FIXNUM (2));
      return entry;

    case RFT_HARDWARE_TRAP:
      entry = allocate_ftti_entry (type, 8);
      vector_set (entry, i++, char_pointer_to_symbol ("signal-number"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (1));
      vector_set (entry, i++, char_pointer_to_symbol ("signal-name"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (2));
      vector_set (entry, i++, char_pointer_to_symbol ("code-name"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (3));
      vector_set (entry, i++, char_pointer_to_symbol ("sp-valid?"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (4));
      vector_set (entry, i++, char_pointer_to_symbol ("recovery-state"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (5));
      vector_set (entry, i++, char_pointer_to_symbol ("pc-info-1"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (6));
      vector_set (entry, i++, char_pointer_to_symbol ("pc-info-2"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (7));
      vector_set (entry, i++, char_pointer_to_symbol ("extra-info"));
      vector_set (entry, i, ULONG_TO_FIXNUM (8));
      return entry;

    case RFT_RETURN_TO_INTERPRETER:
      return allocate_ftti_entry (type, 0);

    case RFT_CC_RESTORE_INTERRUPT_MASK:
      entry = allocate_ftti_entry (type, 1);
      vector_set (entry, i++, char_pointer_to_symbol ("interrupt-mask"));
      vector_set (entry, i, ULONG_TO_FIXNUM (2));
      return entry;

    case RFT_CC_STACK_MARKER:
      entry = allocate_ftti_entry (type, 2);
      vector_set (entry, i++, char_pointer_to_symbol ("marker-type"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (2));
      vector_set (entry, i++, char_pointer_to_symbol ("marker-instance"));
      vector_set (entry, i, ULONG_TO_FIXNUM (3));
      return entry;

    case RFT_CC_BKPT:
    case RFT_CC_INVOCATION:
      entry = allocate_ftti_entry (type, 2);
      vector_set (entry, i++, char_pointer_to_symbol ("procedure"));
      vector_set (entry, i++, ULONG_TO_FIXNUM (2));
      vector_set (entry, i++, char_pointer_to_symbol ("arguments"));
      vector_set (entry, i, ULONG_TO_FIXNUM (3));
      return entry;

    default:
      abort ();
    }
}

static inline void
init_ftti_entry (SCHEME_OBJECT table, return_frame_type_t type)
{
  vector_set (table, type, make_ftti_entry (type));
}

SCHEME_OBJECT
make_frame_type_info_table (void)
{
  SCHEME_OBJECT table = allocate_vector (RFT_LIMIT, true);
  init_ftti_entry (table, RFT_UNDEFINED);
  init_ftti_entry (table, RFT_WITH_ARG);
  init_ftti_entry (table, RFT_EXP_ENV);
  init_ftti_entry (table, RFT_HISTORY);
  init_ftti_entry (table, RFT_APPLY);
  init_ftti_entry (table, RFT_COMPILED_CODE);
  init_ftti_entry (table, RFT_COMPILED_ADDRESS);
  init_ftti_entry (table, RFT_COMBINATION_SAVE);
  init_ftti_entry (table, RFT_STACK_MARKER);
  init_ftti_entry (table, RFT_HARDWARE_TRAP);
  init_ftti_entry (table, RFT_RETURN_TO_INTERPRETER);
  init_ftti_entry (table, RFT_CC_INTERNAL_APPLY);
  init_ftti_entry (table, RFT_CC_RESTORE_INTERRUPT_MASK);
  init_ftti_entry (table, RFT_CC_STACK_MARKER);
  init_ftti_entry (table, RFT_CC_BKPT);
  init_ftti_entry (table, RFT_CC_INVOCATION);
  return table;
}

SCHEME_OBJECT*
next_stack_frame (SCHEME_OBJECT* frame)
{
  unsigned long offset = next_stack_frame_offset (frame);
  if (offset == ULONG_MAX)
    return 0;
  SCHEME_OBJECT* next_frame = frame + offset;
  if (next_frame < stack_end)
    assert (return_address_p (*next_frame));
  return next_frame;
}

unsigned long
next_stack_frame_offset (SCHEME_OBJECT* frame)
{
  switch (return_frame_type (frame))
    {
    case RFT_WITH_ARG:
      return CONT_SIZE;

    case RFT_EXP_ENV:
      return ENV_CONT_SIZE;

    case RFT_HISTORY:
      return HISTORY_CONT_SIZE;

    case RFT_STACK_MARKER:
      return CONT_SIZE + 1;

    case RFT_HARDWARE_TRAP:
      return CONT_SIZE + 7;

    case RFT_APPLY:
      {
        SCHEME_OBJECT header = apply_frame_ptr_header (frame + CONT_SIZE);
        assert (apply_frame_header_p (header));
        return CONT_SIZE + 1 + apply_frame_header_size (header);
      }

    case RFT_COMBINATION_SAVE:
      {
        SCHEME_OBJECT exp = cont_frame_exp (frame);
        assert (combination_p (exp));
        return ENV_CONT_SIZE + combination_size (exp);
      }

    case RFT_COMPILED_CODE:
      return CONT_SIZE;

#ifdef CC_SUPPORT_P
    case RFT_COMPILED_ADDRESS:
      {
        cc_entry_type_t cet;
        if (read_cc_entry_type
              (&cet,
               CC_RETURN_ADDRESS_TO_ENTRY_ADDRESS (CC_RETURN_ADDRESS (*frame))))
          return ULONG_MAX;
        return 1 + cet.args.for_continuation.offset;
      }
#endif

    case RFT_RETURN_TO_INTERPRETER:
      return 1;

    case RFT_CC_RESTORE_INTERRUPT_MASK:
    case RFT_CC_STACK_MARKER:
    case RFT_CC_BKPT:
    case RFT_CC_INVOCATION:
      return reflect_to_interface_offset (frame);

    default:
      return ULONG_MAX;
    }
}
