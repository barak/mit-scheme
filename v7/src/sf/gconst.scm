#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/gconst.scm,v 3.4 1987/12/17 20:34:34 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

(declare (usual-integrations))

;;; This is a list of names that are bound in the global environment.
;;; Normally the compiler will replace references to one of these
;;; names with the value of that name, which is a constant.

(define global-constant-objects
  '(TRUE FALSE SYSTEM-GLOBAL-ENVIRONMENT
    
    SCODE-EVAL FORCE WITH-THREADED-CONTINUATION
    SET-INTERRUPT-ENABLES! WITH-INTERRUPT-MASK WITH-INTERRUPTS-REDUCED
    GET-FIXED-OBJECTS-VECTOR WITH-HISTORY-DISABLED
    PRIMITIVE-PROCEDURE-ARITY NOT FALSE?
    STRING->SYMBOL ERROR-PROCEDURE

    ;; Environment
    LEXICAL-REFERENCE LEXICAL-ASSIGNMENT LOCAL-ASSIGNMENT
    LEXICAL-UNASSIGNED? LEXICAL-UNBOUND? LEXICAL-UNREFERENCEABLE?

    ;; Pointers
    EQ?
    PRIMITIVE-SET-TYPE MAKE-NON-POINTER-OBJECT
    PRIMITIVE-TYPE? PRIMITIVE-TYPE PRIMITIVE-DATUM

    ;; Numbers
    ZERO? POSITIVE? NEGATIVE? 1+ -1+
    INTEGER-DIVIDE INTEGER-DIVIDE-QUOTIENT INTEGER-DIVIDE-REMAINDER
    TRUNCATE ROUND FLOOR CEILING
    SQRT EXP LOG SIN COS 

    ;; Fixnum Arithmetic
    FIX:ZERO? FIX:NEGATIVE? FIX:POSITIVE? FIX:= FIX:< FIX:>
    FIX:1+ FIX:-1+ FIX:+ FIX:- FIX:* FIX:DIVIDE FIX:GCD

    ;; Basic Compound Datatypes
    CONS PAIR? CAR CDR SET-CAR! SET-CDR! GENERAL-CAR-CDR
    NULL? LENGTH MEMQ ASSQ FIRST HEAD EMPTY-STREAM?

    VECTOR-CONS VECTOR-LENGTH VECTOR-REF VECTOR-SET!
    LIST->VECTOR SUBVECTOR->LIST
    SUBVECTOR-MOVE-RIGHT! SUBVECTOR-MOVE-LEFT! SUBVECTOR-FILL!

    ;; Strings
    STRING-ALLOCATE STRING? STRING-REF STRING-SET!
    STRING-LENGTH STRING-MAXIMUM-LENGTH SET-STRING-LENGTH!
    SUBSTRING=? SUBSTRING-CI=? SUBSTRING<?
    SUBSTRING-MOVE-RIGHT! SUBSTRING-MOVE-LEFT!
    SUBSTRING-FIND-NEXT-CHAR-IN-SET
    SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET
    SUBSTRING-MATCH-FORWARD SUBSTRING-MATCH-BACKWARD
    SUBSTRING-MATCH-FORWARD-CI SUBSTRING-MATCH-BACKWARD-CI
    SUBSTRING-UPCASE! SUBSTRING-DOWNCASE! STRING-HASH STRING-HASH-MOD

    ;; Byte Vectors (actually, String/Character operations)
    VECTOR-8B-REF VECTOR-8B-SET! VECTOR-8B-FILL!
    VECTOR-8B-FIND-NEXT-CHAR VECTOR-8B-FIND-PREVIOUS-CHAR
    VECTOR-8B-FIND-NEXT-CHAR-CI VECTOR-8B-FIND-PREVIOUS-CHAR-CI

    BIT-STRING-ALLOCATE MAKE-BIT-STRING BIT-STRING?
    BIT-STRING-LENGTH BIT-STRING-REF BIT-STRING-CLEAR! BIT-STRING-SET!
    BIT-STRING-ZERO? BIT-STRING=?
    BIT-STRING-FILL! BIT-STRING-MOVE! BIT-STRING-MOVEC!
    BIT-STRING-OR! BIT-STRING-AND! BIT-STRING-ANDC!
    BIT-SUBSTRING-MOVE-RIGHT!
    BIT-STRING->UNSIGNED-INTEGER UNSIGNED-INTEGER->BIT-STRING
    READ-BITS! WRITE-BITS!
    BIT-SUBSTRING-FIND-NEXT-SET-BIT

    MAKE-CELL CELL? CELL-CONTENTS SET-CELL-CONTENTS!

    ;; Characters
    MAKE-CHAR CHAR-CODE CHAR-BITS
    CHAR-ASCII? ASCII->CHAR CHAR->ASCII
    INTEGER->CHAR CHAR->INTEGER
    CHAR-UPCASE CHAR-DOWNCASE

    ;; System Compound Datatypes
    SYSTEM-PAIR-CONS SYSTEM-PAIR?
    SYSTEM-PAIR-CAR SYSTEM-PAIR-SET-CAR!
    SYSTEM-PAIR-CDR SYSTEM-PAIR-SET-CDR!

    SYSTEM-HUNK3-CXR0 SYSTEM-HUNK3-SET-CXR0!
    SYSTEM-HUNK3-CXR1 SYSTEM-HUNK3-SET-CXR1!
    SYSTEM-HUNK3-CXR2 SYSTEM-HUNK3-SET-CXR2!

    SYSTEM-LIST-TO-VECTOR SYSTEM-SUBVECTOR-TO-LIST SYSTEM-VECTOR?
    SYSTEM-VECTOR-SIZE SYSTEM-VECTOR-REF SYSTEM-VECTOR-SET!
    ))