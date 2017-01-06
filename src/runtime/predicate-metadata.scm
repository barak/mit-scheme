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

;;;; Predicates: metadata
;;; package: (runtime predicate-metadata)

(declare (usual-integrations))

(define predicate?)
(define get-predicate-tag)
(define set-predicate-tag!)
(define delete-predicate-tag!)
(define (initialize-metadata-table!)
  (let ((table (make-hashed-metadata-table)))
    (set! predicate? (table 'has?))
    (set! get-predicate-tag (table 'get-if-available))
    (set! set-predicate-tag! (table 'put!))
    (set! delete-predicate-tag! (table 'delete!))
    unspecific))

(define boot-registrations (cons '() '()))
(define (register-predicate! . args)
  (let ((next (cons args '())))
    (if (pair? (car boot-registrations))
	(set-cdr! (cdr boot-registrations) next)
	(set-car! boot-registrations next))
    (set-cdr! boot-registrations next)))

(define (register-predicate!/after-boot predicate name . keylist)
  (guarantee keyword-list? keylist 'register-predicate!)
  (let ((tag
         (make-tag predicate
                   name
                   (get-keyword-value keylist 'extra)
                   (get-keyword-value keylist 'description)))
        (superset (get-keyword-value keylist '<=)))
  (if (not (default-object? superset))
      (set-tag<=! tag (predicate->tag superset)))))

(define (predicate->tag predicate #!optional caller)
  (let ((tag (get-predicate-tag predicate #f)))
    (if (not tag)
        (error:wrong-type-argument predicate "predicate" caller))
    tag))

(define (predicate-name predicate)
  (tag-name (predicate->tag predicate 'predicate-name)))

(define (predicate-description predicate)
  (let ((tag (get-predicate-tag predicate #f)))
    (if tag
        (tag-description tag)
        (string-append "object satisfying " (object->description predicate)))))

(define (set-predicate<=! predicate superset)
  (set-tag<=! (predicate->tag predicate 'set-predicate<=!)
              (predicate->tag superset 'set-predicate<=!)))

(define (unregister-predicate! predicate)
  (delete-tag! (predicate->tag predicate)))

(define (guarantee predicate object #!optional caller)
  (if (not (predicate object))
      (error:not-a predicate object caller))
  object)

(define (error:not-a predicate object #!optional caller)
  (error:wrong-type-argument object (predicate-description predicate) caller))

(define (guarantee-list-of predicate object #!optional caller)
  (if (not (list-of-type? object predicate))
      (error:not-a-list-of predicate object caller))
  object)

(define (error:not-a-list-of predicate object #!optional caller)
  (error:wrong-type-argument object
                             (string-append "list of "
                                            (predicate-description predicate))
                             caller))

(define (simple-predicate? object)
  (let ((tag (get-predicate-tag object #f)))
    (and tag
         (not (tag-extra tag)))))

(define (make-tag predicate name #!optional extra description)
  (guarantee-procedure-of-arity predicate 1 'make-tag)
  (guarantee tag-name? name 'make-tag)
  (if (predicate? predicate)
      (error "Predicate is already registered:" predicate))
  (let ((tag
         (%make-tag predicate
                    name
                    (if (default-object? description)
                        #f
                        (guarantee string? description 'make-tag))
                    (if (default-object? extra) #f extra)
		    (make-strong-eq-hash-table)
		    (make-strong-eq-hash-table))))
    (set-predicate-tag! predicate tag)
    tag))

(define (tag-name? object)
  (or (symbol? object)
      (and (list? object)
           (every tag-name? object))))

(define-record-type <tag>
    (%make-tag predicate name description extra subsets supersets)
    tag?
  (predicate tag->predicate)
  (name tag-name)
  (description %tag-description)
  (extra tag-extra)
  (subsets tag-subsets)
  (supersets tag-supersets))

(set-record-type-unparser-method! <tag>
  (simple-unparser-method 'tag
    (lambda (tag)
      (list (tag-name tag)))))

(define (tag-description tag)
  (or (%tag-description tag)
      (object->description (tag-name tag))))

(define (object->description object)
  (call-with-output-string
    (lambda (port)
      (write object port))))

(define (get-tag-subsets tag)
  (hash-table-keys (tag-subsets tag)))

(define (get-tag-supersets tag)
  (hash-table-keys (tag-supersets tag)))

(define (set-tag<=! tag superset)
  (event-distributor/invoke! event:predicate-metadata 'set-tag<=! tag superset)
  (%link! tag superset))

(define (delete-tag! tag)
  (event-distributor/invoke! event:predicate-metadata 'delete-tag! tag)
  ;; Directly link subsets to supersets.
  (for-each (lambda (subset)
	      (for-each (lambda (superset)
			  (%link! subset superset))
			(get-tag-supersets tag)))
	    (get-tag-subsets tag))
  ;; Delete this tag from subsets and supersets.
  (for-each (lambda (subset)
	      (hash-table-delete! (tag-supersets subset) tag))
	    (get-tag-subsets tag))
  (for-each (lambda (superset)
	      (hash-table-delete! (tag-subsets superset) tag))
	    (get-tag-supersets tag))
  (delete-predicate-tag! tag))

(define (%link! subset superset)
  (hash-table-set! (tag-subsets superset) subset subset)
  (hash-table-set! (tag-supersets subset) superset superset))

(define event:predicate-metadata (make-event-distributor))

(define the-top-tag)
(define the-bottom-tag)
(define (initialize-package!)
  (initialize-metadata-table!)

  ;; Transition to post-boot registration
  (set! register-predicate! register-predicate!/after-boot)
  (do ((regs (car boot-registrations) (cdr regs)))
      ((not (pair? regs)))
    (apply register-predicate! (car regs)))
  (set! boot-registrations)

  (register-predicate! predicate? 'predicate)
  (register-predicate! tag-name? 'tag-name)
  (register-predicate! tag? 'tag)
  (register-predicate! any-object? '(conjoin) 'description "any object")
  (register-predicate! no-object? '(disjoin) 'description "no object")

  (set! the-top-tag (predicate->tag any-object?))
  (set! the-bottom-tag (predicate->tag no-object?))
  unspecific)

(define (top-tag) the-top-tag)
(define (top-tag? object) (eqv? the-top-tag object))

(define (bottom-tag) the-bottom-tag)
(define (bottom-tag? object) (eqv? the-bottom-tag object))

(define (any-object? object) object #t)
(define (no-object? object) object #f)