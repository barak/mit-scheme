#| -*-Scheme-*-

$Id: pribinut.scm,v 1.3 2002/11/20 19:46:18 cph Exp $

Copyright (c) 1993, 1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

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
