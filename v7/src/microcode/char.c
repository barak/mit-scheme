/*	Emacs -*-C-*-an't tell the language			*/

/****************************************************************
*                                                               *
*                         Copyright (c) 1986                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* File: character.c
 *
 * This file contains the character primitives.
 */

#include <ctype.h>
#include "scheme.h"
#include "primitive.h"
#include "character.h"

/* pieces of characters primitives
1.  MAKE-CHAR                  Makes a char from its bits and its code.
                               A char is a 32
			       TC_CHARACTER typecode in the 8 bits near the
			       msb, the next 12 bits unused, the next 5 bits
			       for the bits (control, hyper, meta, etc.) and
			       the last 7, including the lsb for the code
			       field, i.e., what letter it is.
2.  CHAR-BITS                  Gets those 5 bits bits.
3.  CHAR-CODE                  Gets those 7 code bits.
*/

Built_In_Primitive(Prim_Make_Char, 2, "MAKE-CHAR")
{
  long bucky_bits, code;
  Primitive_2_Args();

  Arg_1_Type( TC_FIXNUM);
  Arg_2_Type( TC_FIXNUM);
  Range_Check( code, Arg1, 0, (MAX_CODE - 1), ERR_ARG_1_BAD_RANGE);
  Range_Check( bucky_bits, Arg2, 0, (MAX_BITS - 1), ERR_ARG_2_BAD_RANGE);
  return (make_char( bucky_bits, code));
}

Built_In_Primitive( Prim_Char_Bits, 1, "CHAR-BITS")
{
  Primitive_1_Args();

  Arg_1_Type( TC_CHARACTER);
  return (Make_Unsigned_Fixnum( char_bits( Arg1)));
}

Built_In_Primitive( Prim_Char_Code, 1, "CHAR-CODE")
{
  Primitive_1_Args();

  Arg_1_Type( TC_CHARACTER);
  return (Make_Unsigned_Fixnum( char_code( Arg1)));
}

/* Primitives for converting characters:
1.  CHAR->INTEGER                 Converts a char to its 12 bit numerical
                                           value in extended ascii.
2.  INTEGER->CHAR                 Converts the other way.
3.  CHAR-UPCASE                   Converts a char to upcase.
4.  CHAR-DOWNCASE                 Converts a char to lowercase.
5.  ASCII->CHAR                   Converts an ascii value to a char, including
                                           doing bit twiddleing to make sure
					   the control bit is set correctly.
6.  CHAR->ASCII                   Converts a char back to the ascii value,
                                           signalling an error if there are
					   problems.
7.  CHAR-ASCII?                   Converts a char similarly, but signals false
                                           if there are problems.
8.  CHAR->JESSE-JACKSON           Converts a char to a fundamentalist preacher
                                           who runs for President.  Shouldn't
					   be used in the Democratic Party.
*/

Built_In_Primitive( Prim_Char_To_Integer, 1, "CHAR->INTEGER")
{
  Primitive_1_Args();

  Arg_1_Type( TC_CHARACTER);
  return (Make_Unsigned_Fixnum( (Arg1 & MASK_EXTNDD_CHAR)));
}

Built_In_Primitive( Prim_Integer_To_Char, 1, "INTEGER->CHAR")
{
  long integ;
  Primitive_1_Args();

  Arg_1_Type( TC_FIXNUM);
  Sign_Extend_Range_Check( integ, Arg1, 0, (MAX_EXTNDD_CHAR - 1),
			  ERR_ARG_1_BAD_RANGE);
  return (Make_Non_Pointer( TC_CHARACTER, integ));
}

Built_In_Primitive( Prim_Char_Downcase, 1, "CHAR-DOWNCASE")
{
  long ascii;
  Primitive_1_Args();

  Arg_1_Type( TC_CHARACTER);
  return make_char( char_bits( Arg1), Real_To_Lower( char_code( Arg1)));
}

Built_In_Primitive( Prim_Char_Upcase, 1, "CHAR-UPCASE")
{
  Primitive_1_Args();

  Arg_1_Type( TC_CHARACTER);
  return make_char( char_bits( Arg1), Real_To_Upper( char_code( Arg1)));
}

Built_In_Primitive( Prim_Ascii_To_Char, 1, "ASCII->CHAR")
{
  long ascii;
  Primitive_1_Args();

  Arg_1_Type( TC_FIXNUM);
  Range_Check( ascii, Arg1, 0, (MAX_ASCII - 1), ERR_ARG_1_BAD_RANGE);
  return (c_char_to_scheme_char( ascii));
}

Built_In_Primitive( Prim_Char_Ascii_P, 1, "CHAR-ASCII?")
{
  long ascii;
  Primitive_1_Args();

  Arg_1_Type( TC_CHARACTER);
  ascii = scheme_char_to_c_char( Arg1);
  return ((ascii == NOT_ASCII) ? NIL : Make_Unsigned_Fixnum( ascii));
}

Built_In_Primitive( Prim_Char_To_Ascii, 1, "CHAR->ASCII")
{
  long ascii;
  Primitive_1_Args();

  Arg_1_Type( TC_CHARACTER);
  ascii = scheme_char_to_c_char( Arg1);
  if (ascii == NOT_ASCII)
    Primitive_Error( ERR_ARG_1_BAD_RANGE);
  return (Make_Unsigned_Fixnum( ascii));
}

forward Boolean ascii_control_p();

long
ascii_to_mit_ascii( ascii)
     long ascii;
{
  long bucky_bits, code;

  bucky_bits = (((ascii & 0200) != 0) ? CHAR_BITS_META : 0);
  code = (ascii & 0177);
  if (ascii_control_p( code))
    {
      code |= 0100;		/* Convert to non-control code. */
      bucky_bits |= CHAR_BITS_CONTROL;
    }
  return ((bucky_bits << CODE_LENGTH) | code);
}

long
mit_ascii_to_ascii( mit_ascii)
     long mit_ascii;
{
  long bucky_bits, code;

  bucky_bits = ((mit_ascii >> CODE_LENGTH) & CHAR_MASK_BITS);
  code = (mit_ascii & CHAR_MASK_CODE);
  if ((bucky_bits & (~ CHAR_BITS_CONTROL_META)) != 0)
    return (NOT_ASCII);
  else
    {
      if ((bucky_bits & CHAR_BITS_CONTROL) != 0)
	{
	  code = (Real_To_Upper( code) & (~ 0100));
	  if (!ascii_control_p( code))
	    return (NOT_ASCII);
	}
      else
	{
	  if (ascii_control_p( code))
	    return (NOT_ASCII);
	}
      return (((bucky_bits & CHAR_BITS_META) != 0) ? (code | 0200) : code);
    }
}

Boolean
ascii_control_p( code)
     int code;
{
  switch (code)
    {
    case 000:
    case 001:
    case 002:
    case 003:
    case 004:
    case 005:
    case 006:
    case 007:
    case 016:
    case 017:
    case 020:
    case 021:
    case 022:
    case 023:
    case 024:
    case 025:
    case 026:
    case 027:
    case 030:
    case 031:
    case 034:
    case 035:
    case 036:
      return (true);

    default:
      return (false);
    }
}
