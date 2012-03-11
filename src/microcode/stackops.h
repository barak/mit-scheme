/* Emacs: this is -*- C -*- code.

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

#ifndef STACKOPS_H
#define STACKOPS_H 1

/* C code produced
   Thursday August 24, 2006 at 6:20:11 PM  */

typedef enum
{
  stackify_opcode_illegal = 0000,
  stackify_opcode_escape = 0001,
  stackify_opcode_push_Pfixnum = 0002,
  stackify_opcode_push__fixnum = 0003,
  stackify_opcode_push_Pinteger = 0004,
  stackify_opcode_push__integer = 0005,
  stackify_opcode_push_false = 0006,
  stackify_opcode_push_true = 0007,
  stackify_opcode_push_nil = 0010,
  stackify_opcode_push_flonum = 0011,
  stackify_opcode_push_cons_ratnum = 0012,
  stackify_opcode_push_cons_recnum = 0013,
  stackify_opcode_push_string = 0014,
  stackify_opcode_push_symbol = 0015,
  stackify_opcode_push_uninterned_symbol = 0016,
  stackify_opcode_push_char = 0017,
  stackify_opcode_push_bit_string = 0020,
  stackify_opcode_push_empty_cons = 0021,
  stackify_opcode_pop_and_set_car = 0022,
  stackify_opcode_pop_and_set_cdr = 0023,
  stackify_opcode_push_consS = 0024,
  stackify_opcode_push_empty_vector = 0025,
  stackify_opcode_pop_and_vector_set = 0026,
  stackify_opcode_push_vector = 0027,
  stackify_opcode_push_empty_record = 0030,
  stackify_opcode_pop_and_record_set = 0031,
  stackify_opcode_push_record = 0032,
  stackify_opcode_push_lookup = 0033,
  stackify_opcode_store = 0034,
  stackify_opcode_push_constant = 0035,
  stackify_opcode_push_unassigned = 0036,
  stackify_opcode_push_primitive = 0037,
  stackify_opcode_push_primitive_lexpr = 0040,
  stackify_opcode_push_nm_header = 0041,
  stackify_opcode_push_label_entry = 0042,
  stackify_opcode_push_linkage_header_operator = 0043,
  stackify_opcode_push_linkage_header_reference = 0044,
  stackify_opcode_push_linkage_header_assignment = 0045,
  stackify_opcode_push_linkage_header_global = 0046,
  stackify_opcode_push_linkage_header_closure = 0047,
  stackify_opcode_push_ulong = 0050,
  stackify_opcode_push_label_descriptor = 0051,
  stackify_opcode_cc_block_to_entry = 0052,
  stackify_opcode_retag_cc_block = 0053,
  stackify_opcode_push_return_code = 0054,
  stackify_opcode_push_0 = 0200,
  stackify_opcode_push_1 = 0201,
  stackify_opcode_push_2 = 0202,
  stackify_opcode_push_3 = 0203,
  stackify_opcode_push_4 = 0204,
  stackify_opcode_push_5 = 0205,
  stackify_opcode_push_6 = 0206,
  stackify_opcode_push__1 = 0207,
  stackify_opcode_push_consS_0 = 0210,
  stackify_opcode_push_consS_1 = 0211,
  stackify_opcode_push_consS_2 = 0212,
  stackify_opcode_push_consS_3 = 0213,
  stackify_opcode_push_consS_4 = 0214,
  stackify_opcode_push_consS_5 = 0215,
  stackify_opcode_push_consS_6 = 0216,
  stackify_opcode_push_consS_7 = 0217,
  stackify_opcode_pop_and_vector_set_0 = 0220,
  stackify_opcode_pop_and_vector_set_1 = 0221,
  stackify_opcode_pop_and_vector_set_2 = 0222,
  stackify_opcode_pop_and_vector_set_3 = 0223,
  stackify_opcode_pop_and_vector_set_4 = 0224,
  stackify_opcode_pop_and_vector_set_5 = 0225,
  stackify_opcode_pop_and_vector_set_6 = 0226,
  stackify_opcode_pop_and_vector_set_7 = 0227,
  stackify_opcode_push_vector_1 = 0230,
  stackify_opcode_push_vector_2 = 0231,
  stackify_opcode_push_vector_3 = 0232,
  stackify_opcode_push_vector_4 = 0233,
  stackify_opcode_push_vector_5 = 0234,
  stackify_opcode_push_vector_6 = 0235,
  stackify_opcode_push_vector_7 = 0236,
  stackify_opcode_push_vector_8 = 0237,
  stackify_opcode_pop_and_record_set_0 = 0240,
  stackify_opcode_pop_and_record_set_1 = 0241,
  stackify_opcode_pop_and_record_set_2 = 0242,
  stackify_opcode_pop_and_record_set_3 = 0243,
  stackify_opcode_pop_and_record_set_4 = 0244,
  stackify_opcode_pop_and_record_set_5 = 0245,
  stackify_opcode_pop_and_record_set_6 = 0246,
  stackify_opcode_pop_and_record_set_7 = 0247,
  stackify_opcode_push_record_1 = 0250,
  stackify_opcode_push_record_2 = 0251,
  stackify_opcode_push_record_3 = 0252,
  stackify_opcode_push_record_4 = 0253,
  stackify_opcode_push_record_5 = 0254,
  stackify_opcode_push_record_6 = 0255,
  stackify_opcode_push_record_7 = 0256,
  stackify_opcode_push_record_8 = 0257,
  stackify_opcode_push_lookup_0 = 0260,
  stackify_opcode_push_lookup_1 = 0261,
  stackify_opcode_push_lookup_2 = 0262,
  stackify_opcode_push_lookup_3 = 0263,
  stackify_opcode_push_lookup_4 = 0264,
  stackify_opcode_push_lookup_5 = 0265,
  stackify_opcode_push_lookup_6 = 0266,
  stackify_opcode_push_lookup_7 = 0267,
  stackify_opcode_store_0 = 0270,
  stackify_opcode_store_1 = 0271,
  stackify_opcode_store_2 = 0272,
  stackify_opcode_store_3 = 0273,
  stackify_opcode_store_4 = 0274,
  stackify_opcode_store_5 = 0275,
  stackify_opcode_store_6 = 0276,
  stackify_opcode_store_7 = 0277,
  stackify_opcode_push_primitive_0 = 0300,
  stackify_opcode_push_primitive_1 = 0301,
  stackify_opcode_push_primitive_2 = 0302,
  stackify_opcode_push_primitive_3 = 0303,
  stackify_opcode_push_primitive_4 = 0304,
  stackify_opcode_push_primitive_5 = 0305,
  stackify_opcode_push_primitive_6 = 0306,
  stackify_opcode_push_primitive_7 = 0307,
  N_STACKIFY_OPCODE = 0310
} stackify_opcode_t;

#endif /* !STACKOPS_H */
