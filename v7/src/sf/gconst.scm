#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/gconst.scm,v 4.12 1991/08/14 02:08:29 jinx Exp $

Copyright (c) 1987-91 Massachusetts Institute of Technology

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
MIT in each case. |#

;;;; SCode Optimizer: Global Constants List
;;; package: (scode-optimizer)

(declare (usual-integrations))

;;; This is a list of names that are bound in the global environment.
;;; Normally the compiler will replace references to one of these
;;; names with the value of that name, which is a constant.

(define global-constant-objects
  '(
    *THE-NON-PRINTING-OBJECT*
    ASCII->CHAR
    BIT-STRING->UNSIGNED-INTEGER
    BIT-STRING-ALLOCATE
    BIT-STRING-AND!
    BIT-STRING-ANDC!
    BIT-STRING-CLEAR!
    BIT-STRING-FILL!
    BIT-STRING-LENGTH
    BIT-STRING-MOVE!
    BIT-STRING-MOVEC!
    BIT-STRING-OR!
    BIT-STRING-REF
    BIT-STRING-SET!
    BIT-STRING-XOR!
    BIT-STRING-ZERO?
    BIT-STRING=?
    BIT-STRING?
    BIT-SUBSTRING-FIND-NEXT-SET-BIT
    BIT-SUBSTRING-MOVE-RIGHT!
    CAR
    CDR
    CELL-CONTENTS
    CELL?
    CHAR->ASCII
    CHAR->INTEGER
    CHAR-ASCII?
    CHAR-BITS
    CHAR-BITS-LIMIT
    CHAR-CODE
    CHAR-CODE-LIMIT
    CHAR-DOWNCASE
    CHAR-INTEGER-LIMIT
    CHAR-UPCASE
    CHAR:NEWLINE
    COMPILED-CODE-ADDRESS->BLOCK
    COMPILED-CODE-ADDRESS->OFFSET
    CONS
    ENABLE-INTERRUPTS!
    EQ?
    ERROR-PROCEDURE
    EXECUTE-AT-NEW-STATE-POINT
    FALSE
    FALSE?
    FIX:*
    FIX:+
    FIX:-
    FIX:-1+
    FIX:1+
    FIX:<
    ;; FIX:= handled by expanding it to EQ?
    FIX:>
    FIX:AND
    FIX:ANDC
    FIX:DIVIDE
    FIX:GCD
    FIX:LSH
    FIX:NEGATIVE?
    FIX:NOT
    FIX:OR
    FIX:POSITIVE?
    FIX:QUOTIENT
    FIX:REMAINDER
    FIX:XOR
    ;; FIX:ZERO? handled by expanding it to (EQ? x 0)
    FLO:*
    FLO:+
    FLO:-
    FLO:/
    FLO:<
    FLO:=
    FLO:>
    FLO:ABS
    FLO:ACOS
    FLO:ASIN
    FLO:ATAN
    FLO:ATAN2
    FLO:CEILING
    FLO:CEILING->EXACT
    FLO:COS
    FLO:EXP
    FLO:EXPT
    FLO:FLOOR
    FLO:FLOOR->EXACT
    FLO:LOG
    FLO:NEGATE
    FLO:NEGATIVE?
    FLO:POSITIVE?
    FLO:ROUND
    FLO:ROUND->EXACT
    FLO:SIN
    FLO:SQRT
    FLO:TAN
    FLO:TRUNCATE
    FLO:TRUNCATE->EXACT
    FLO:VECTOR-CONS
    FLO:VECTOR-LENGTH
    FLO:VECTOR-REF
    FLO:VECTOR-SET!
    FLO:ZERO?
    FORCE
    GENERAL-CAR-CDR
    GET-FIXED-OBJECTS-VECTOR
    GET-FLUID-BINDINGS
    GET-NEXT-CONSTANT
    HUNK3-CONS
    INT:*
    INT:+
    INT:-
    INT:-1+
    INT:1+
    INT:<
    INT:=
    INT:>
    INT:DIVIDE
    INT:NEGATE
    INT:NEGATIVE?
    INT:POSITIVE?
    INT:QUOTIENT
    INT:REMAINDER
    INT:ZERO?
    INTEGER->CHAR
    INTEGER-DIVIDE-QUOTIENT
    INTEGER-DIVIDE-REMAINDER
    INTERRUPT-BIT/GC
    INTERRUPT-BIT/GLOBAL-1
    INTERRUPT-BIT/GLOBAL-2
    INTERRUPT-BIT/GLOBAL-3
    INTERRUPT-BIT/GLOBAL-GC
    INTERRUPT-BIT/KBD
    INTERRUPT-BIT/STACK
    INTERRUPT-BIT/SUSPEND
    INTERRUPT-BIT/TIMER
    INTERRUPT-MASK/ALL
    INTERRUPT-MASK/GC-OK
    INTERRUPT-MASK/NONE
    LAMBDA-TAG:FLUID-LET
    LAMBDA-TAG:LET
    LAMBDA-TAG:MAKE-ENVIRONMENT
    LAMBDA-TAG:UNNAMED
    LENGTH
    LEXICAL-ASSIGNMENT
    LEXICAL-REFERENCE
    LEXICAL-UNASSIGNED?
    LEXICAL-UNBOUND?
    LEXICAL-UNREFERENCEABLE?
    LIST->VECTOR
    LOCAL-ASSIGNMENT
    MAKE-BIT-STRING
    MAKE-CELL
    MAKE-CHAR
    MAKE-NON-POINTER-OBJECT
    ;; MODULO ; expanded to primitive.  Global defn. is not.
    NOT
    NULL?
    OBJECT-CONSTANT?
    OBJECT-DATUM
    OBJECT-GC-TYPE
    OBJECT-NEW-TYPE
    OBJECT-PURE?
    OBJECT-TYPE
    OBJECT-TYPE?
    PAIR?
    PRIMITIVE-PROCEDURE-ARITY
    PROCESS-TIME-CLOCK
    ;; QUOTIENT ; expanded to primitive.  Global defn. is not.
    READ-BITS!
    REAL-TIME-CLOCK
    ;; REMAINDER ; expanded to primitive.  Global defn. is not.
    SET-CAR!
    SET-CDR!
    SET-CELL-CONTENTS!
    SET-CURRENT-DYNAMIC-STATE!
    SET-FLUID-BINDINGS!
    SET-INTERRUPT-ENABLES!
    SET-STRING-LENGTH!
    STRING->SYMBOL
    STRING-ALLOCATE
    STRING-HASH
    STRING-HASH-MOD
    STRING-LENGTH
    STRING-MAXIMUM-LENGTH
    STRING-REF
    STRING-SET!
    STRING?
    SUBSTRING-CI=?
    SUBSTRING-DOWNCASE!
    SUBSTRING-FIND-NEXT-CHAR-IN-SET
    SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET
    SUBSTRING-MATCH-BACKWARD
    SUBSTRING-MATCH-BACKWARD-CI
    SUBSTRING-MATCH-FORWARD
    SUBSTRING-MATCH-FORWARD-CI
    SUBSTRING-MOVE-LEFT!
    SUBSTRING-MOVE-RIGHT!
    SUBSTRING-UPCASE!
    SUBSTRING<?
    SUBSTRING=?
    SUBVECTOR->LIST
    SUBVECTOR-FILL!
    SUBVECTOR-MOVE-LEFT!
    SUBVECTOR-MOVE-RIGHT!
    SYSTEM-GLOBAL-ENVIRONMENT
    SYSTEM-HUNK3-CXR0
    SYSTEM-HUNK3-CXR1
    SYSTEM-HUNK3-CXR2
    SYSTEM-HUNK3-SET-CXR0!
    SYSTEM-HUNK3-SET-CXR1!
    SYSTEM-HUNK3-SET-CXR2!
    SYSTEM-LIST->VECTOR
    SYSTEM-PAIR-CAR
    SYSTEM-PAIR-CDR
    SYSTEM-PAIR-CONS
    SYSTEM-PAIR-SET-CAR!
    SYSTEM-PAIR-SET-CDR!
    SYSTEM-PAIR?
    SYSTEM-SUBVECTOR->LIST
    SYSTEM-VECTOR-LENGTH
    SYSTEM-VECTOR-REF
    SYSTEM-VECTOR-SET!
    SYSTEM-VECTOR?
    THE-EMPTY-STREAM
    TRANSLATE-TO-STATE-POINT
    TRUE
    UNDEFINED-CONDITIONAL-BRANCH
    UNSIGNED-INTEGER->BIT-STRING
    UNSPECIFIC
    VECTOR
    VECTOR-8B-FILL!
    VECTOR-8B-FIND-NEXT-CHAR
    VECTOR-8B-FIND-NEXT-CHAR-CI
    VECTOR-8B-FIND-PREVIOUS-CHAR
    VECTOR-8B-FIND-PREVIOUS-CHAR-CI
    VECTOR-8B-REF
    VECTOR-8B-SET!
    VECTOR-LENGTH
    VECTOR-REF
    VECTOR-SET!
    WITH-HISTORY-DISABLED
    WITH-INTERRUPT-MASK
    WRITE-BITS!
    ))