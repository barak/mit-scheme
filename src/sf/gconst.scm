#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

|#

;;;; SCode Optimizer: Global Constants List
;;; package: (scode-optimizer)

(declare (usual-integrations))

(define global-constant-objects
  '(CHAR-BITS-LIMIT
    CHAR-CODE-LIMIT
    CHAR-INTEGER-LIMIT
    FALSE
    LAMBDA-TAG:UNNAMED			;needed for cold load
    SYSTEM-GLOBAL-ENVIRONMENT		;suppresses warnings about (access ...)
    THE-EMPTY-STREAM
    TRUE
    UNDEFINED-CONDITIONAL-BRANCH
    UNSPECIFIC))

(define global-primitives
  '((%RECORD %RECORD)
    (%RECORD-LENGTH %RECORD-LENGTH)
    (%RECORD-REF %RECORD-REF)
    (%RECORD-SET! %RECORD-SET!)
    (%RECORD? %RECORD?)
    (BIT-STRING->UNSIGNED-INTEGER BIT-STRING->UNSIGNED-INTEGER)
    (BIT-STRING-ALLOCATE BIT-STRING-ALLOCATE)
    (BIT-STRING-AND! BIT-STRING-AND!)
    (BIT-STRING-ANDC! BIT-STRING-ANDC!)
    (BIT-STRING-CLEAR! BIT-STRING-CLEAR!)
    (BIT-STRING-FILL! BIT-STRING-FILL!)
    (BIT-STRING-LENGTH BIT-STRING-LENGTH)
    (BIT-STRING-MOVE! BIT-STRING-MOVE!)
    (BIT-STRING-MOVEC! BIT-STRING-MOVEC!)
    (BIT-STRING-OR! BIT-STRING-OR!)
    (BIT-STRING-REF BIT-STRING-REF)
    (BIT-STRING-SET! BIT-STRING-SET!)
    (BIT-STRING-XOR! BIT-STRING-XOR!)
    (BIT-STRING-ZERO? BIT-STRING-ZERO?)
    (BIT-STRING=? BIT-STRING=?)
    (BIT-STRING? BIT-STRING?)
    (BIT-SUBSTRING-FIND-NEXT-SET-BIT BIT-SUBSTRING-FIND-NEXT-SET-BIT)
    (BIT-SUBSTRING-MOVE-RIGHT! BIT-SUBSTRING-MOVE-RIGHT!)
    (BYTEVECTOR-LENGTH BYTEVECTOR-LENGTH 1)
    (BYTEVECTOR-U8-REF BYTEVECTOR-U8-REF 2)
    (BYTEVECTOR-U8-SET! BYTEVECTOR-U8-SET! 3)
    (BYTEVECTOR? BYTEVECTOR? 1)
    (CAR CAR)
    (CDR CDR)
    (CELL-CONTENTS CELL-CONTENTS)
    (CELL? CELL?)
    (CHAR->INTEGER CHAR->INTEGER)
    (CHAR? CHAR?)
    (COMPILED-CODE-ADDRESS->BLOCK COMPILED-CODE-ADDRESS->BLOCK)
    (COMPILED-CODE-ADDRESS->OFFSET COMPILED-CODE-ADDRESS->OFFSET)
    (CONS CONS)
    (EQ? EQ?)
    (ERROR-PROCEDURE ERROR-PROCEDURE)
    (EXACT-INTEGER? INTEGER?)
    (FALSE? NOT)
    (FIX:* MULTIPLY-FIXNUM)
    (FIX:+ PLUS-FIXNUM)
    (FIX:- MINUS-FIXNUM)
    (FIX:-1+ MINUS-ONE-PLUS-FIXNUM)
    (FIX:1+ ONE-PLUS-FIXNUM)
    (FIX:< LESS-THAN-FIXNUM?)
    (FIX:= EQUAL-FIXNUM?)
    (FIX:> GREATER-THAN-FIXNUM?)
    (FIX:AND FIXNUM-AND)
    (FIX:ANDC FIXNUM-ANDC)
    (FIX:DIVIDE DIVIDE-FIXNUM)
    (FIX:FIXNUM? FIXNUM?)
    (FIX:GCD GCD-FIXNUM)
    (FIX:LSH FIXNUM-LSH)
    (FIX:NEGATIVE? NEGATIVE-FIXNUM?)
    (FIX:NOT FIXNUM-NOT)
    (FIX:OR FIXNUM-OR)
    (FIX:POSITIVE? POSITIVE-FIXNUM?)
    (FIX:QUOTIENT FIXNUM-QUOTIENT)
    (FIX:REMAINDER FIXNUM-REMAINDER)
    (FIX:XOR FIXNUM-XOR)
    (FIX:ZERO? ZERO-FIXNUM?)
    (FIXNUM? FIXNUM?)
    (FLO:* FLONUM-MULTIPLY)
    (FLO:+ FLONUM-ADD)
    (FLO:- FLONUM-SUBTRACT)
    (FLO:/ FLONUM-DIVIDE)
    (FLO:< FLONUM-LESS?)
    (FLO:= FLONUM-EQUAL?)
    (FLO:> FLONUM-GREATER?)
    (FLO:ABS FLONUM-ABS)
    (FLO:ACOS FLONUM-ACOS)
    (FLO:ASIN FLONUM-ASIN)
    (FLO:ATAN FLONUM-ATAN)
    (FLO:ATAN2 FLONUM-ATAN2)
    (FLO:CEILING FLONUM-CEILING)
    (FLO:CEILING->EXACT FLONUM-CEILING->EXACT)
    (FLO:COS FLONUM-COS)
    (FLO:EXP FLONUM-EXP)
    (FLO:EXPM1 FLONUM-EXPM1)
    (FLO:EXPT FLONUM-EXPT)
    (FLO:FLONUM? FLONUM?)
    (FLO:FLOOR FLONUM-FLOOR)
    (FLO:FLOOR->EXACT FLONUM-FLOOR->EXACT)
    (FLO:LOG FLONUM-LOG)
    (FLO:LOG1P FLONUM-LOG1P)
    (FLO:NEGATE FLONUM-NEGATE)
    (FLO:NEGATIVE? FLONUM-NEGATIVE?)
    (FLO:POSITIVE? FLONUM-POSITIVE?)
    (FLO:ROUND FLONUM-ROUND)
    (FLO:ROUND->EXACT FLONUM-ROUND->EXACT)
    (FLO:SIN FLONUM-SIN)
    (FLO:SQRT FLONUM-SQRT)
    (FLO:TAN FLONUM-TAN)
    (FLO:TRUNCATE FLONUM-TRUNCATE)
    (FLO:TRUNCATE->EXACT FLONUM-TRUNCATE->EXACT)
    (FLO:VECTOR-CONS FLOATING-VECTOR-CONS)
    (FLO:VECTOR-LENGTH FLOATING-VECTOR-LENGTH)
    (FLO:VECTOR-REF FLOATING-VECTOR-REF)
    (FLO:VECTOR-SET! FLOATING-VECTOR-SET!)
    (FLO:ZERO? FLONUM-ZERO?)
    (GET-FIXED-OBJECTS-VECTOR GET-FIXED-OBJECTS-VECTOR)
    (GET-INTERRUPT-ENABLES GET-INTERRUPT-ENABLES)
    (HUNK3-CONS HUNK3-CONS)
    (INDEX-FIXNUM? INDEX-FIXNUM?)
    (INT:* INTEGER-MULTIPLY)
    (INT:+ INTEGER-ADD)
    (INT:- INTEGER-SUBTRACT)
    (INT:-1+ INTEGER-SUBTRACT-1)
    (INT:1+ INTEGER-ADD-1)
    (INT:< INTEGER-LESS?)
    (INT:= INTEGER-EQUAL?)
    (INT:> INTEGER-GREATER?)
    (INT:DIVIDE INTEGER-DIVIDE)
    (INT:INTEGER? INTEGER?)
    (INT:NEGATE INTEGER-NEGATE)
    (INT:NEGATIVE? INTEGER-NEGATIVE?)
    (INT:POSITIVE? INTEGER-POSITIVE?)
    (INT:QUOTIENT INTEGER-QUOTIENT)
    (INT:REMAINDER INTEGER-REMAINDER)
    (INT:ZERO? INTEGER-ZERO?)
    (INTEGER->CHAR INTEGER->CHAR)
    (LEXICAL-ASSIGNMENT LEXICAL-ASSIGNMENT)
    (LEXICAL-REFERENCE LEXICAL-REFERENCE)
    (LEXICAL-UNASSIGNED? LEXICAL-UNASSIGNED?)
    (LEXICAL-UNBOUND? LEXICAL-UNBOUND?)
    (LEXICAL-UNREFERENCEABLE? LEXICAL-UNREFERENCEABLE?)
    (LOCAL-ASSIGNMENT LOCAL-ASSIGNMENT)
    (MAKE-BIT-STRING MAKE-BIT-STRING)
    (MAKE-CELL MAKE-CELL)
    (MAKE-NON-POINTER-OBJECT MAKE-NON-POINTER-OBJECT)
    (NOT NOT)
    (NULL? NULL?)
    (OBJECT-DATUM OBJECT-DATUM)
    (OBJECT-NEW-TYPE OBJECT-SET-TYPE)
    (OBJECT-TYPE OBJECT-TYPE)
    (OBJECT-TYPE? OBJECT-TYPE?)
    (PAIR? PAIR?)
    (PRIMITIVE-PROCEDURE-ARITY PRIMITIVE-PROCEDURE-ARITY)
    (PRIMITIVE-PROCEDURE-DOCUMENTATION PRIMITIVE-PROCEDURE-DOCUMENTATION)
    (READ-BITS! READ-BITS!)
    (SET-CAR! SET-CAR!)
    (SET-CDR! SET-CDR!)
    (SET-CELL-CONTENTS! SET-CELL-CONTENTS!)
    (SET-INTERRUPT-ENABLES! SET-INTERRUPT-ENABLES!)
    (SET-STRING-LENGTH! SET-STRING-LENGTH!)
    (STACK-ADDRESS-OFFSET STACK-ADDRESS-OFFSET)
    (STRING->CHAR-SYNTAX STRING->SYNTAX-ENTRY)
    (STRING-ALLOCATE STRING-ALLOCATE)
    (STRING-LENGTH STRING-LENGTH)
    (STRING-REF STRING-REF)
    (STRING-SET! STRING-SET!)
    (STRING? STRING?)
    (SYSTEM-HUNK3-CXR0 SYSTEM-HUNK3-CXR0)
    (SYSTEM-HUNK3-CXR1 SYSTEM-HUNK3-CXR1)
    (SYSTEM-HUNK3-CXR2 SYSTEM-HUNK3-CXR2)
    (SYSTEM-HUNK3-SET-CXR0! SYSTEM-HUNK3-SET-CXR0!)
    (SYSTEM-HUNK3-SET-CXR1! SYSTEM-HUNK3-SET-CXR1!)
    (SYSTEM-HUNK3-SET-CXR2! SYSTEM-HUNK3-SET-CXR2!)
    (SYSTEM-LIST->VECTOR SYSTEM-LIST-TO-VECTOR)
    (SYSTEM-PAIR-CAR SYSTEM-PAIR-CAR)
    (SYSTEM-PAIR-CDR SYSTEM-PAIR-CDR)
    (SYSTEM-PAIR-CONS SYSTEM-PAIR-CONS)
    (SYSTEM-PAIR-SET-CAR! SYSTEM-PAIR-SET-CAR!)
    (SYSTEM-PAIR-SET-CDR! SYSTEM-PAIR-SET-CDR!)
    (SYSTEM-PAIR? SYSTEM-PAIR?)
    (SYSTEM-SUBVECTOR->LIST SYSTEM-SUBVECTOR-TO-LIST)
    (SYSTEM-VECTOR-LENGTH SYSTEM-VECTOR-SIZE)
    (SYSTEM-VECTOR-REF SYSTEM-VECTOR-REF)
    (SYSTEM-VECTOR-SET! SYSTEM-VECTOR-SET!)
    (SYSTEM-VECTOR? SYSTEM-VECTOR?)
    (UNSIGNED-INTEGER->BIT-STRING UNSIGNED-INTEGER->BIT-STRING)
    (VECTOR VECTOR)
    (VECTOR-8B-REF VECTOR-8B-REF)
    (VECTOR-8B-SET! VECTOR-8B-SET!)
    (VECTOR-LENGTH VECTOR-LENGTH)
    (VECTOR-REF VECTOR-REF)
    (VECTOR-SET! VECTOR-SET!)
    (VECTOR? VECTOR?)
    (WITH-HISTORY-DISABLED WITH-HISTORY-DISABLED)
    (WITH-INTERRUPT-MASK WITH-INTERRUPT-MASK)
    (WRITE-BITS! WRITE-BITS!)))