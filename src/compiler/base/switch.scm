#| -*-Scheme-*-

$Id: switch.scm,v 4.26 2001/12/20 03:04:02 cph Exp $

Copyright (c) 1988-1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
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
(define compiler:use-multiclosures? #t)
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