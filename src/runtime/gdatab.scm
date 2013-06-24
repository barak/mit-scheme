#| -*-Scheme-*-

$Id: gdatab.scm,v 14.6 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1988, 1989, 1990, 1999 Massachusetts Institute of Technology

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