#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Tags for efficient dispatching

;;; From "Efficient Method Dispatch in PCL", Gregor Kiczales and Luis
;;; Rodriguez, Proceedings of the 1990 ACM Conference on Lisp and
;;; Functional Programming.  Parts of this code are based on the
;;; September 16, 1992 PCL implementation.

(declare (usual-integrations))

(define (make-dispatch-tag contents)
  (let ((tag
	 ((ucode-primitive object-set-type)
	  (ucode-type record)
	  ((ucode-primitive vector-cons) dispatch-tag-index-end #f))))
    (%record-set! tag 0 dispatch-tag-marker)
    (%record-set! tag 1 contents)
    (do ((i dispatch-tag-index-start (fix:+ i 1)))
	((fix:= i dispatch-tag-index-end))
      (%record-set! tag i (get-dispatch-tag-cache-number)))
    tag))

(define-integrable (dispatch-tag? object)
  (and (%record? object)
       (eq? dispatch-tag-marker (%record-ref object 0))))

(define-integrable dispatch-tag-marker
  ((ucode-primitive string->symbol) "#[dispatch-tag]"))

(define-integrable dispatch-tag-index-start 2)
(define-integrable dispatch-tag-index-end 10)

(define-integrable (dispatch-tag-ref t i)
  (%record-ref t i))

(define-integrable (dispatch-tag-set! t i x)
  (%record-set! t i x))

(define (dispatch-tag-contents tag)
  (guarantee dispatch-tag? tag 'DISPATCH-TAG-CONTENTS)
  (%record-ref tag 1))

(declare (integrate-operator next-dispatch-tag-index))
(define (next-dispatch-tag-index index)
  (and (fix:< (fix:+ index 1) dispatch-tag-index-end)
       (fix:+ index 1)))

(define-integrable dispatch-tag-cache-number-adds-ok
  ;; This constant controls the number of non-zero bits tag cache
  ;; numbers will have.
  ;;
  ;; The value of this constant is the number of tag cache numbers
  ;; that can be added and still be certain the result will be a
  ;; fixnum.  This is implicitly used by all the code that computes
  ;; primary cache locations from multiple tags.
  4)

(define-deferred get-dispatch-tag-cache-number
  (let ((modulus
	 (int:quotient
	  (let loop ((n 2)) (if (fix:fixnum? n) (loop (int:* n 2)) n))
	  dispatch-tag-cache-number-adds-ok))
	(state (make-random-state)))
    (lambda ()
      (random modulus state))))

;;;; Object Tags

;;; We assume that most new data types will be constructed from tagged
;;; vectors, and therefore we should optimize the path for such
;;; structures as much as possible.

(define (dispatch-tag object)
  (declare (integrate object))
  (declare (ignore-reference-traps (set microcode-type-tag-table
					microcode-type-method-table)))
  (if (and (%record? object)
	   (%record? (%record-ref object 0))
	   (eq? dispatch-tag-marker (%record-ref (%record-ref object 0) 0)))
      (%record-ref object 0)
      (if (vector-ref microcode-type-tag-table (object-type object))
	  (vector-ref microcode-type-tag-table (object-type object))
	  ((vector-ref microcode-type-method-table (object-type object))
	   object))))

(define (make-built-in-tag names)
  (let ((tags (map built-in-dispatch-tag names)))
    (if (any (lambda (tag) tag) tags)
	(let ((tag (car tags)))
	  (if (not (and (every (lambda (tag*)
				 (eq? tag* tag))
			       (cdr tags))
			(let ((names* (dispatch-tag-contents tag)))
			  (and (every (lambda (name)
					(memq name names*))
				      names)
			       (every (lambda (name)
					(memq name names))
				      names*)))))
	      (error "Illegal built-in tag redefinition:" names))
	  tag)
	(let ((tag (make-dispatch-tag (list-copy names))))
	  (set! built-in-tags (cons tag built-in-tags))
	  tag))))

(define (built-in-dispatch-tags)
  (list-copy built-in-tags))

(define (built-in-dispatch-tag name)
  (find (lambda (tag)
	  (memq name (dispatch-tag-contents tag)))
	built-in-tags))

;;;; Initialization

(define built-in-tags)
(define microcode-type-tag-table)
(define microcode-type-method-table)

(define (initialize-tag-tables!)
  (set! built-in-tags '())
  (set! microcode-type-tag-table
	(make-initialized-vector (microcode-type/code-limit)
	  (lambda (code)
	    (make-built-in-tag
	     (let ((names (microcode-type/code->names code)))
	       (if (pair? names)
		   names
		   '(object)))))))
  (set! microcode-type-method-table
	(make-vector (microcode-type/code-limit) #f))

  (let ((defmethod
	 (lambda (name get-method)
	   (let ((code (microcode-type/name->code name)))
	     (vector-set! microcode-type-method-table code
			  (get-method
			   (vector-ref microcode-type-tag-table code)))
	     (vector-set! microcode-type-tag-table code #f)))))
    (defmethod 'compiled-entry
      (lambda (default-tag)
	(let ((procedure-tag (make-built-in-tag '(compiled-procedure)))
	      (return-tag (make-built-in-tag '(compiled-return-address)))
	      (expression-tag (make-built-in-tag '(compiled-expression))))
	  (lambda (object)
	    (case (system-hunk3-cxr0
		   ((ucode-primitive compiled-entry-kind 1) object))
	      ((0) procedure-tag)
	      ((1) return-tag)
	      ((2) expression-tag)
	      (else default-tag))))))
    (defmethod 'false
      (lambda (default-tag)
	(let ((boolean-tag (make-built-in-tag '(boolean))))
	  (lambda (object)
	    (if (eq? object #f)
		boolean-tag
		default-tag)))))
    (defmethod 'constant
      (lambda (default-tag)
	(let ((boolean-tag (make-built-in-tag '(boolean)))
	      (null-tag (make-built-in-tag '(null)))
	      (eof-tag (make-built-in-tag '(eof)))
	      (default-object-tag (make-built-in-tag '(default)))
	      (keyword-tag (make-built-in-tag '(lambda-keyword))))
	  (lambda (object)
	    (if (eof-object? object)
		eof-tag
		(case object
		  ((#t) boolean-tag)
		  ((()) null-tag)
		  ((#!default) default-object-tag)
		  ((#!optional #!rest #!key #!aux) keyword-tag)
		  (else default-tag)))))))
    (defmethod 'record
      (lambda (default-tag)
	(let ((dt-tag (make-built-in-tag '(dispatch-tag))))
	  (lambda (object)
	    (if (eq? dispatch-tag-marker (%record-ref object 0))
		dt-tag
		default-tag)))))

    ;; Flonum length can change size on different architectures, so we
    ;; measure one.
    (let ((flonum-length (system-vector-length microcode-id/floating-epsilon)))
      (defmethod 'flonum
	(lambda (default-tag)
	  (let ((flonum-vector-tag (make-built-in-tag '(flonum-vector))))
	    (lambda (object)
	      (if (fix:= flonum-length (system-vector-length object))
		  default-tag
		  flonum-vector-tag))))))))