#| -*-Scheme-*-

$Id: switch.scm,v 4.25 1999/01/02 06:06:43 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Compiler Option Switches
;;; package: (compiler)

(declare (usual-integrations))

;;; Binary switches

(define compiler:enable-integration-declarations? true)
(define compiler:enable-expansion-declarations? false)
(define compiler:compile-by-procedures? true)
(define compiler:noisy? true)
(define compiler:show-time-reports? false)
(define compiler:show-procedures? true)
(define compiler:show-phases? false)
(define compiler:show-subphases? false)
(define compiler:preserve-data-structures? false)
(define compiler:code-compression? true)
(define compiler:cache-free-variables? true)
(define compiler:implicit-self-static? true)
(define compiler:optimize-environments? true)
(define compiler:analyze-side-effects? true)
(define compiler:cse? true)
(define compiler:open-code-primitives? true)
(define compiler:generate-rtl-files? false)
(define compiler:generate-lap-files? false)
(define compiler:intersperse-rtl-in-lap? true)
(define compiler:generate-range-checks? false)
(define compiler:generate-type-checks? false)
(define compiler:generate-stack-checks? true)
(define compiler:open-code-flonum-checks? false)
(define compiler:use-multiclosures? true)
(define compiler:coalescing-constant-warnings? true)
(define compiler:cross-compiling? false)
;; This only works in the C back end, right now
(define compiler:compress-top-level? false)
(define compiler:avoid-scode? true)

;; If true, the compiler is allowed to assume that fixnum operations
;; are only applied to inputs for which the operation is closed, i.e.
;; generates a valid fixnum.  If false, the compiler will ensure that
;; the result of a fixnum operation is a fixnum, although it may be an
;; incorrect result for screw cases.
(define compiler:assume-safe-fixnums? true)

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
(define compiler:phase-wrapper
  false)