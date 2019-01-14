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

;;;; Support for tagged objects

(declare (usual-integrations))

(define-structure (vector-tag
		   (constructor %make-vector-tag (parent name index noop)))
  (parent false read-only true)
  (name false read-only true)
  (index false read-only true)
  (description false)
  (method-alist '())

  ;; This property was stored in the method alist, but it is used so
  ;; frequently that it deserves its own slot.
  (noop false)
  )

(define make-vector-tag
  (let ((root-tag (%make-vector-tag false 'OBJECT false false)))
    (define-print-method (lambda (object)
			   (and (vector? object)
				(fix:> (vector-length object) 0)
				(eq? root-tag (vector-ref object 0))))
      (standard-print-method
       (lambda (object)
	 (string "LIAR:" (vector-tag-name (tagged-vector/tag object))))))
    (named-lambda (make-vector-tag parent name enumeration)
      (%make-vector-tag (or parent root-tag)
			name
			(and enumeration
			     (enumeration/name->index enumeration
						      name))
			;; Propagate this downward at construction time
			;; to avoid having to crawl upward at use time.
			(and parent (vector-tag-noop parent))))))

(define (define-vector-tag-unparser tag unparser)
  (define-print-method (lambda (object)
			 (and (vector? object)
			      (fix:> (vector-length object) 0)
			      (eq? tag (vector-ref object 0))))
    unparser)
  (vector-tag-name tag))

(define (vector-tag-put! tag key value)
  (let ((entry (assq key (vector-tag-method-alist tag))))
    (if entry
	(set-cdr! entry value)
	(set-vector-tag-method-alist! tag
				      (cons (cons key value)
					    (vector-tag-method-alist tag))))))

(define (vector-tag-get tag key)
  (let ((value
	 (or (assq key (vector-tag-method-alist tag))
	     (let loop ((tag (vector-tag-parent tag)))
	       (and tag
		    (or (assq key (vector-tag-method-alist tag))
			(loop (vector-tag-parent tag))))))))
    (and value (cdr value))))

(define (define-vector-tag-method tag name method)
  (vector-tag-put! tag name method)
  name)

(define (vector-tag-method tag name)
  (or (vector-tag-get tag name)
      (error "Unbound method" name tag)))

(define-integrable make-tagged-vector
  vector)

(define-integrable (tagged-vector/tag vector)
  (vector-ref vector 0))

(define-integrable (tagged-vector/index vector)
  (vector-tag-index (tagged-vector/tag vector)))

(define (tagged-vector? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (vector-tag? (tagged-vector/tag object))))

(define (->tagged-vector object)
  (let ((object
	 (if (exact-nonnegative-integer? object)
	     (unhash-object object)
	     object)))
    (and (or (tagged-vector? object)
	     (named-structure? object))
	 object)))

(define (tagged-vector/predicate tag)
  (lambda (object)
    (and (vector? object)
	 (not (zero? (vector-length object)))
	 (eq? tag (tagged-vector/tag object)))))

(define (tagged-vector/subclass-predicate tag)
  (lambda (object)
    (and (vector? object)
	 (not (zero? (vector-length object)))
	 (let loop ((tag* (tagged-vector/tag object)))
	   (and (vector-tag? tag*)
		(or (eq? tag tag*)
		    (loop (vector-tag-parent tag*))))))))

(define (tagged-vector/description object)
  (cond ((named-structure? object)
	 pp-description)
	((tagged-vector? object)
	 (vector-tag-description (tagged-vector/tag object)))
	(else
	 (error "Not a tagged vector" object))))