#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/compiler/etc/asm.scm,v 1.2 1991/02/15 00:25:17 cph Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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

;;;; Source (lap) assembler

(declare (usual-integrations))

;; To be loaded in (compiler top-level)

;;; Example of `lap->code' usage:

(define bar
  ;; defines bar to be a procedure that adds 1 to its argument
  ;; with no type or range checks.
  (scode-eval
   (lap->code
    'start
    `((pea (@pcr proc))
      (or b (& ,(* (microcode-type 'compiled-entry) 4)) (@a 7))
      (mov l (@a+ 7) (@ao 6 8))
      (and b (& #x3) (@a 7))
      (rts)
      (dc uw #x0202)
      (block-offset proc)
      (label proc)
      (mov l (@a+ 7) (d 0))
      (addq l (& 1) (d 0))
      (mov l (d 0) (@ao 6 8))
      (and b (& #x3) (@a 7))
      (rts)))
   '()))

(define (lap->code label lap)
  (in-compiler
   (lambda ()
     (set! *lap* lap)
     (set! *entry-label* label)
     (set! *current-label-number* 0)
     (set! *next-constant* 0)
     (set! *interned-constants* '())
     (set! *interned-variables* '())
     (set! *interned-assignments* '())
     (set! *interned-uuo-links* '())
     (set! *block-label* (generate-label))
     (set! *external-labels* '())
     (set! *ic-procedure-headers* '())
     (phase/assemble)
     (phase/link)
     *result*)))