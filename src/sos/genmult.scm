#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
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
	  (do ((this (multiplexer-list m) (cdr this))
	       (prev #f
		     (if (eq? (car this) generator)
			 (begin
			   (if prev
			       (set-cdr! prev (cdr this))
			       (set-multiplexer-list! m (cdr this)))
			   prev)
			 this)))
	      ((not (pair? this))))
	  (maybe-deinstall-multiplexer generic))
	(if (eq? generator m)
	    (set-generic-procedure-generator! generic #f)))))

(define (remove-generic-procedure-generators generic tags)
  (do ((gens (generic-procedure-generator-list generic) (cdr gens)))
      ((not (pair? gens)))
    (let ((generator (car gens)))
      (if (generator generic tags)
	  (remove-generic-procedure-generator generic generator)))))

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
    (cond ((and (not (multiplexer-default m))
		(not (pair? generators)))
	   (set-generic-procedure-generator! generic #f))
	  ((and (not (multiplexer-default m))
		(not (pair? (cdr generators))))
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
    (if (pair? generators)
	(let ((procedure ((car generators) generic tags)))
	  (if procedure
	      (if (let find-extra ((generators (cdr generators)))
		    (if (pair? generators)
			(if ((car generators) generic tags)
			    #t
			    (find-extra (cdr generators)))
			#f))
		  (lambda args
		    (error:extra-applicable-methods generic args))
		  procedure)
	      (loop (cdr generators))))
	(let ((default (multiplexer-default multiplexer)))
	  (and default
	       (default generic tags))))))

(define multiplexer-tag
  (list 'generic-procedure-multiplexer))

(define condition-type:extra-applicable-methods
  (make-condition-type 'extra-applicable-methods condition-type:error
      '(OPERATOR OPERANDS)
    (lambda (condition port)
      (write-string "Too many applicable methods for " port)
      (write (access-condition condition 'operator) port)
      (write-string " with these arguments: " port)
      (write (access-condition condition 'operands) port)
      (write-string "." port))))

(define error:extra-applicable-methods
  (condition-signaller condition-type:extra-applicable-methods
		       '(operator operands)
		       standard-error-handler))