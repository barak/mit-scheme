#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/pcsample/pribinut.scm,v 1.1 1995/07/28 14:14:08 adams Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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

;;;; Primitive, Builtin and Utility support
;;; package: (pribinut)

(declare (usual-integrations))

(define (initialize-package!)
  (install-pribinut))

(define-primitives
  (get-primitive-counts 0)
  (get-primitive-name   1))


;; Primitives-- NB: *not* memoizeable since can dynamically load new ucode!

(define (get-primitive-count)
  "()\n\
  Returns the sum of the number of defined and undefined primitive procedures.\
  "
  (let ((defined-dot-undefined (get-primitive-counts)))
    (+ (car defined-dot-undefined)
       (cdr defined-dot-undefined))))


;; GJR Hack: given that mumble-get returns #F is nonesuch, we can walk up
;;           through the indices until we find the first failure. Moreover,
;;           Since there is no mechanism for dynacmically loading new builtins
;;           or utilities, this result can be memoized.

(define (count-mumbles mumble-getter)
  (do ((i 0 (1+ i)))
      ((not (mumble-getter i))	; first index to fail to be gotten is it
       i)))


;; Builtins

(define (get-builtin-name index)
  ((ucode-primitive builtin-index->name 1) index))

(define     *builtin-count-promise*)	; tba
(define (get-builtin-count)
  "()\n\
  Returns the number of ``builtin'' hooks defined in the running Scheme system.\
  "
  (force *builtin-count-promise*))

(define (install-builtin-count-promise)
  (set! *builtin-count-promise*
	(delay (count-mumbles get-builtin-name)))
  unspecific)


;; Utilities

(define (get-utility-name index)
  ((ucode-primitive utility-index->name 1) index))

(define     *utility-count-promise*)	; tba
(define (get-utility-count)
  "()\n\
  Returns the number of ``utility'' hooks defined in the running Scheme system.\
  "
  (force *utility-count-promise*))

(define (install-utility-count-promise)
  (set! *utility-count-promise*
	(delay (count-mumbles get-utility-name)))
  unspecific)


;; Install

(define (install-pribinut)
  (install-builtin-count-promise)
  (install-utility-count-promise)
  ;; re-cache counts in code new frobs have been added to the microcode
  (add-event-receiver! event:after-restore install-pribinut))


;;; fini
