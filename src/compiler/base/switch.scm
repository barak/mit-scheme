#| -*-Scheme-*-

$Id: switch.scm,v 4.29 2005/12/19 19:29:56 cph Exp $

Copyright 1987,1988,1989,1990,1991,1992 Massachusetts Institute of Technology
Copyright 1993,1994,2001,2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Compiler Option Switches
;;; package: (compiler)

(declare (usual-integrations))

;;; Binary switches

(define compiler:enable-integration-declarations? #t)
(define compiler:compile-by-procedures? #t)
(define compiler:noisy? #t)
(define compiler:show-time-reports? #f)
(define compiler:show-procedures? #t)
(define compiler:show-phases? #f)
(define compiler:show-subphases? #f)
(define compiler:preserve-data-structures? #f)
(define compiler:code-compression? #t)
(define compiler:cache-free-variables? #t)
(define compiler:implicit-self-static? #t)
(define compiler:optimize-environments? #t)
(define compiler:analyze-side-effects? #t)
(define compiler:cse? #t)
(define compiler:open-code-primitives? #t)
(define compiler:generate-rtl-files? #f)
(define compiler:generate-lap-files? #f)
(define compiler:intersperse-rtl-in-lap? #t)
(define compiler:generate-range-checks? #f)
(define compiler:generate-type-checks? #f)
(define compiler:generate-stack-checks? #t)
(define compiler:open-code-flonum-checks? #f)
(define compiler:use-multiclosures? #f)
(define compiler:coalescing-constant-warnings? #t)
(define compiler:cross-compiling? #f)
;; This only works in the C back end, right now
(define compiler:compress-top-level? #f)
(define compiler:avoid-scode? #t)

;; If true, the compiler is allowed to assume that fixnum operations
;; are only applied to inputs for which the operation is closed, i.e.
;; generates a valid fixnum.  If false, the compiler will ensure that
;; the result of a fixnum operation is a fixnum, although it may be an
;; incorrect result for screw cases.
(define compiler:assume-safe-fixnums? #t)

;; The switch COMPILER:OPEN-CODE-FLOATING-POINT-ARITHMETIC? is in machin.scm.

;;; Nary switches

(define compiler:package-optimization-level
  ;; Possible values: NONE LOW HYBRID HIGH
  'HYBRID)

(define compiler:default-top-level-declarations
  '((UUO-LINK ALL)))

;;; Hook: bind this to a procedure of one argument and it will receive
;;; each phase of the compiler as a thunk.  It is expected to call the
;;; thunk after any appropriate processing.
(define compiler:phase-wrapper #f)