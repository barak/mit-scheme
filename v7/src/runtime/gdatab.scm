#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/gdatab.scm,v 14.5 1990/09/19 00:32:28 cph Rel $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;;; Global Databases
;;; package: (runtime global-database)

(declare (usual-integrations))

(define (initialize-package!)
  (set! event:after-restore (make-event-distributor))
  (set! event:after-restart (make-event-distributor))
  (set! event:before-exit (make-event-distributor))
  (set! tagged-pair-methods (make-1d-table))
  (set! tagged-vector-methods (make-1d-table))
  (set! named-structure-descriptions (make-1d-table)))

(define event:after-restore)
(define event:after-restart)
(define event:before-exit)
(define tagged-pair-methods)
(define tagged-vector-methods)
(define named-structure-descriptions)

(define (unparser/tagged-pair-method tag)
  (and (not (future? tag))
       (1d-table/get tagged-pair-methods tag false)))

(define (unparser/set-tagged-pair-method! tag method)
  (1d-table/put! tagged-pair-methods tag method))

(define (unparser/tagged-vector-method tag)
  (and (not (future? tag))
       (1d-table/get tagged-vector-methods tag false)))

(define (unparser/set-tagged-vector-method! tag method)
  (1d-table/put! tagged-vector-methods tag method))

(define (named-structure/get-tag-description tag)
  (1d-table/get named-structure-descriptions tag false))

(define (named-structure/set-tag-description! tag description)
  (1d-table/put! named-structure-descriptions tag description))

;;; Support for old-style methods

(define (add-unparser-special-pair! tag method)
  (unparser/set-tagged-pair-method! tag (convert-old-method method)))

(define (add-unparser-special-object! tag method)
  (unparser/set-tagged-vector-method! tag (convert-old-method method)))

(define (unparse-with-brackets thunk)
  (write-string "#[")
  (thunk)
  (write-char #\]))

(define (convert-old-method method)
  (lambda (state object)
    (with-output-to-port (unparser-state/port state)
      (lambda ()
	(method object)))))