#| -*-Scheme-*-

$Id: genmult.scm,v 1.5 2003/02/14 18:28:32 cph Exp $

Copyright 1995-1999 Massachusetts Institute of Technology

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

;;;; Multiplexed Generic Procedures

;;; This code assumes that a non-multiplexed generic procedure
;;; generator is equivalent to the same generator stored in a
;;; multiplexer.  Multiplexers assume that each of their generators is
;;; applicable to a particular set of objects, and that the set does
;;; not intersect any of the sets handled by other generators stored
;;; in the same multiplexer.  Combining these two assumptions means
;;; that a non-multiplexed generator must follow the convention for
;;; multiplexed generators, even though there is no reason to do so in
;;; the absence of multiplexers.

;;; This convention is encouraged by hiding the low-level procedures
;;; that allow direct access to a generic procedure's generator, and
;;; forcing the programmer to go through the multiplexing interface.
;;; That way, multiplexing appears to be an integral part of the
;;; generic-procedure interface.

(declare (usual-integrations))

(define (generic-procedure-generator-list generic)
  (let ((m (generic-procedure-generator generic)))
    (if m
	(if (multiplexer? m)
	    (list-copy (multiplexer-list m))
	    (list m))
	'())))

(define (add-generic-procedure-generator generic generator)
  (let ((m (generic-procedure-generator generic)))
    (if (multiplexer? m)
	(begin
	  (purge-generic-procedure-cache generic)
	  (add-generator m generator))
	(add-generator (install-multiplexer generic) generator))))

(define (remove-generic-procedure-generator generic generator)
  (let ((m (generic-procedure-generator generic)))
    (if (multiplexer? m)
	(begin
	  (purge-generic-procedure-cache generic)
	  (set-multiplexer-list! m (delq! generator (multiplexer-list m)))
	  (maybe-deinstall-multiplexer generic))
	(if (eq? generator m)
	    (set-generic-procedure-generator! generic #f)))))

(define (remove-generic-procedure-generators generic tags)
  (for-each (lambda (generator)
	      (if (generator generic tags)
		  (remove-generic-procedure-generator generic generator)))
	    (generic-procedure-generator-list generic)))

(define (generic-procedure-default-generator generic)
  (let ((m (generic-procedure-generator generic)))
    (and (multiplexer? m)
	 (multiplexer-default m))))

(define (set-generic-procedure-default-generator! generic generator)
  (let ((m (generic-procedure-generator generic)))
    (cond ((multiplexer? m)
	   (purge-generic-procedure-cache generic)
	   (set-multiplexer-default! m generator)
	   (maybe-deinstall-multiplexer generic))
	  (generator
	   (set-multiplexer-default! (install-multiplexer generic)
				     generator)))))

(define (install-multiplexer generic)
  (let ((m (make-multiplexer)))
    (let ((g (generic-procedure-generator generic)))
      (if g
	  (add-generator m g)))
    (set-generic-procedure-generator! generic m)
    m))

(define (add-generator m generator)
  (set-multiplexer-list! m (cons generator (multiplexer-list m))))

(define (maybe-deinstall-multiplexer generic)
  (let* ((m (generic-procedure-generator generic))
	 (generators (multiplexer-list m)))
    (cond ((and (null? generators)
		(not (multiplexer-default m)))
	   (set-generic-procedure-generator! generic #f))
	  ((and (null? (cdr generators))
		(not (multiplexer-default m)))
	   (set-generic-procedure-generator! generic (car generators))))))

(define (make-multiplexer)
  (make-entity (lambda (multiplexer generic tags)
		 (multiplexer-dispatch multiplexer generic tags))
	       (make-multiplexer-record '() #f)))

(define (multiplexer? object)
  (and (entity? object)
       (multiplexer-record? (entity-extra object))))

(define (multiplexer-list multiplexer)
  (multiplexer-record/list (entity-extra multiplexer)))

(define (set-multiplexer-list! multiplexer list)
  (set-multiplexer-record/list! (entity-extra multiplexer) list))

(define (multiplexer-default multiplexer)
  (multiplexer-record/default (entity-extra multiplexer)))

(define (set-multiplexer-default! multiplexer default)
  (set-multiplexer-record/default! (entity-extra multiplexer) default))

(define-structure (multiplexer-record (conc-name multiplexer-record/))
  list
  default)

(define (multiplexer-dispatch multiplexer generic tags)
  (let loop ((generators (multiplexer-list multiplexer)))
    (if (null? generators)
	(let ((default (multiplexer-default multiplexer)))
	  (and default
	       (default generic tags)))
	(let ((procedure ((car generators) generic tags)))
	  (cond ((not procedure)
		 (loop (cdr generators)))
		((there-exists? (cdr generators)
		   (lambda (generator)
		     (generator generic tags)))
		 (lambda args
		   (error:extra-applicable-methods generic args)))
		(else procedure))))))

(define multiplexer-tag)
(define del-rassq)
(define condition-type:extra-applicable-methods)
(define error:extra-applicable-methods)

(define (initialize-multiplexer!)
  (set! multiplexer-tag (list 'GENERIC-PROCEDURE-MULTIPLEXER))
  (set! del-rassq (delete-association-procedure list-deletor eq? cdr))
  unspecific)

(define (initialize-conditions!)
  (set! condition-type:extra-applicable-methods
	(make-condition-type 'EXTRA-APPLICABLE-METHODS condition-type:error
	    '(OPERATOR OPERANDS)
	  (lambda (condition port)
	    (write-string "Too many applicable methods for " port)
	    (write (access-condition condition 'OPERATOR) port)
	    (write-string " with these arguments: " port)
	    (write (access-condition condition 'OPERANDS) port)
	    (write-string "." port))))
  (set! error:extra-applicable-methods
	(condition-signaller condition-type:extra-applicable-methods
			     '(OPERATOR OPERANDS)
			     standard-error-handler))
  unspecific)