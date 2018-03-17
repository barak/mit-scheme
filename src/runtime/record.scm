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

;;;; Records
;;; package: (runtime record)

;;; adapted from JAR's implementation
;;; conforms to R4RS proposal

(declare (usual-integrations))

(define-primitives
  (vector-cons 2))

(define (%copy-record record)
  (let ((length (%record-length record)))
    (let ((result (%make-record (%record-ref record 0) length)))
      (do ((index 1 (fix:+ index 1)))
	  ((fix:= index length))
	(%record-set! result index (%record-ref record index)))
      result)))

(define (make-record-type type-name field-names
			  #!optional
			  default-inits unparser-method entity-unparser-method)
  ;; The unparser-method and entity-unparser-method arguments should be removed
  ;; after the 9.3 release.
  (declare (ignore entity-unparser-method))
  (let ((caller 'make-record-type))
    (if (not (list-of-unique-symbols? field-names))
	(error:not-a list-of-unique-symbols? field-names caller))
    (let* ((names (list->vector field-names))
	   (n (vector-length names)))
      (if (not (or (default-object? default-inits)
		   (%valid-default-inits? default-inits n)))
	  (error:wrong-type-argument default-inits
				     "default initializers"
				     caller))
      (letrec*
	  ((predicate
	    (lambda (object)
	      (%record-type-instance? tag object)))
	   (tag
	    (%make-record-type (->type-name type-name)
			       predicate
			       names
			       (if (default-object? default-inits)
				   (vector-cons n #f)
				   (list->vector default-inits))
			       #f
			       #f)))
	(%set-record-type-instance-marker! tag tag)
	(set-predicate<=! predicate record?)
	(if (and unparser-method
		 (not (default-object? unparser-method)))
	    (define-unparser-method predicate unparser-method))
	tag))))

(define (%valid-default-inits? default-inits n-fields)
  (fix:= n-fields (length default-inits)))

(defer-boot-action 'record-procedures
  (lambda ()
    (set! %valid-default-inits?
	  (named-lambda (%valid-default-inits? default-inits n-fields)
	    (and (fix:= n-fields (length default-inits))
		 (every (lambda (init)
			  (or (not init)
			      (thunk? init)))
			default-inits))))
    unspecific))

(define (initialize-record-procedures!)
  (run-deferred-boot-actions 'record-procedures))

(define %record-metatag)
(define record-type?)
(define %make-record-type)
(add-boot-init!
 (lambda ()
   (set! %record-metatag (make-dispatch-metatag 'record-tag))
   (set! record-type? (dispatch-tag->predicate %record-metatag))
   (set! %make-record-type
	 (dispatch-metatag-constructor %record-metatag 'make-record-type))
   unspecific))

;; Can be deleted after 9.3 release:
(define (record-type-dispatch-tag record-type)
  record-type)

(define-integrable (%record-type-field-names record-type)
  (dispatch-tag-extra-ref record-type 0))

(define-integrable (%record-type-default-inits record-type)
  (dispatch-tag-extra-ref record-type 1))

(define-integrable (%record-type-applicator record-type)
  (dispatch-tag-extra-ref record-type 2))

(define-integrable (%set-record-type-applicator! record-type applicator)
  (%dispatch-tag-extra-set! record-type 2 applicator))

(define-integrable (%record-type-instance-marker record-type)
  (dispatch-tag-extra-ref record-type 3))

(define-integrable (%set-record-type-instance-marker! record-type marker)
  (%dispatch-tag-extra-set! record-type 3 marker))

(define (%initialize-applicator-context!)
  (set-fixed-objects-item! 'record-dispatch-tag %record-metatag)
  (set-fixed-objects-item! 'record-applicator-index
			   (%dispatch-tag-extra-index 2)))

(define-integrable (%record-type-n-fields record-type)
  (vector-length (%record-type-field-names record-type)))

(define-integrable (%record-type-length record-type)
  (fix:+ 1 (%record-type-n-fields record-type)))

(define (record-type-name record-type)
  (guarantee record-type? record-type 'record-type-name)
  (symbol->string (dispatch-tag-name record-type)))

(define (record-type-field-names record-type)
  (guarantee record-type? record-type 'record-type-field-names)
  (vector->list (%record-type-field-names record-type)))

(define (record-type-default-value-by-index record-type field-index)
  (let ((init
	 (vector-ref (%record-type-default-inits record-type)
		     (fix:- field-index 1))))
    (and init
	 (init))))

(define (record-type-applicator record-type)
  (guarantee record-type? record-type 'record-type-applicator)
  (%record-type-applicator record-type))

(define (set-record-type-applicator! record-type applicator)
  (guarantee record-type? record-type 'set-record-type-applicator!)
  (if applicator
      (guarantee procedure? applicator 'set-record-type-applicator!))
  (%set-record-type-applicator! record-type applicator))

(define (record? object)
  (and (%record? object)
       (let ((marker (%record-ref object 0)))
	 (or (record-type? marker)
	     (%record-type-proxy? marker)))))

(define (%record-type-instance? type object)
  (and (%record? object)
       (eq? (%record-ref object 0)
	    (%record-type-instance-marker type))))

(define (record-type-descriptor record)
  (let ((marker (%record-ref record 0)))
    (cond ((record-type? marker) marker)
	  ((%record-type-proxy? marker) (%proxy->record-type marker))
	  (else (error:not-a record? record 'record-type-descriptor)))))

(define (%record-type-fasdumpable? type)
  (%record-type-proxy? (%record-type-instance-marker type)))

(define (%record-type-proxy? object)
  (and (object-type? (ucode-type constant) object)
       (let ((v (object-new-type (ucode-type fixnum) object)))
	 (and (fix:>= v #x100)
	      (fix:< v #x200)))))
(register-predicate! %record-type-proxy? 'record-type-proxy)

(define (set-record-type-fasdumpable! type proxy)
  (defer-boot-action 'record-procedures
    (lambda ()
      (set-record-type-fasdumpable! type proxy))))

(defer-boot-action 'record-procedures
  (lambda ()
    (set! set-record-type-fasdumpable!
	  (named-lambda (set-record-type-fasdumpable! type proxy)
	    (guarantee record-type? type 'set-record-type-fasdumpable!)
	    (guarantee %record-type-proxy? proxy 'set-record-type-fasdumpable!)
	    (without-interrupts
	     (lambda ()
	       (if (%record-type-fasdumpable? type)
		   (error "Record type already fasdumpable:" type))
	       (if (%proxy->record-type proxy)
		   (error "Record-type proxy already in use:" proxy))
	       (%set-proxied-record-type! proxy type)
	       (%set-record-type-instance-marker! type proxy)))))
    unspecific))

(define-integrable (%record-type-proxy->index marker)
  (fix:- (object-new-type (ucode-type fixnum) marker) #x100))

(define-integrable (%index->record-type-proxy index)
  (object-new-type (ucode-type constant) (fix:+ index #x100)))

(define-integrable (%proxy->record-type proxy)
  (vector-ref %proxied-record-types (%record-type-proxy->index proxy)))

(define-integrable (%set-proxied-record-type! proxy type)
  (vector-set! %proxied-record-types (%record-type-proxy->index proxy) type))

(define %proxied-record-types)
(defer-boot-action 'record-procedures
  (lambda ()
    (set! %proxied-record-types (fixed-objects-item 'proxied-record-types))
    unspecific))

(define record-type-proxy:pathname (%index->record-type-proxy 0))
(define record-type-proxy:host (%index->record-type-proxy 1))

;;;; Constructors

(define (record-constructor record-type #!optional field-names)
  (guarantee record-type? record-type 'record-constructor)
  (if (or (default-object? field-names)
	  (equal? field-names (record-type-field-names record-type)))
      (%record-constructor-default-names record-type)
      (begin
	(if (not (list? field-names))
	    (error:not-a list? field-names 'record-constructor))
	(%record-constructor-given-names record-type field-names))))

(define %record-constructor-default-names
  (let-syntax
      ((expand-cases
	(sc-macro-transformer
	 (lambda (form environment)
	   (let ((tag (close-syntax (list-ref form 1) environment))
		 (n-fields (close-syntax (list-ref form 2) environment))
		 (limit (close-syntax (list-ref form 3) environment))
		 (default (close-syntax (list-ref form 4) environment))
		 (make-name
		  (lambda (i)
		    (intern (string-append "v" (number->string i))))))
	     (let loop ((i 0) (names '()))
	       (if (fix:< i limit)
		   `(if (fix:= ,n-fields ,i)
			(lambda (,@names)
			  (%record (%record-type-instance-marker ,tag) ,@names))
			,(loop (fix:+ i 1)
			       (append names (list (make-name i)))))
		   default)))))))
    (lambda (record-type)
      (let ((n-fields (%record-type-n-fields record-type)))
	(expand-cases record-type n-fields 16
	  (let ((reclen (fix:+ 1 n-fields)))
	    (letrec
		((constructor
		  (lambda field-values
		    (let ((record
			   (%make-record
			    (%record-type-instance-marker record-type)
			    reclen))
			  (lose
			   (lambda ()
			     (error:wrong-number-of-arguments constructor
							      n-fields
							      field-values))))
		      (do ((i 1 (fix:+ i 1))
			   (vals field-values (cdr vals)))
			  ((not (fix:< i reclen))
			   (if (not (null? vals)) (lose)))
			(if (not (pair? vals)) (lose))
			(%record-set! record i (car vals)))
		      record))))
	      constructor)))))))

(define (%record-constructor-given-names record-type field-names)
  (let* ((indexes
	  (map (lambda (field-name)
		 (record-type-field-index record-type field-name #t))
	       field-names))
	 (defaults
	   (let* ((n (%record-type-length record-type))
		  (seen? (vector-cons n #f)))
	     (do ((indexes indexes (cdr indexes)))
		 ((not (pair? indexes)))
	       (vector-set! seen? (car indexes) #t))
	     (do ((i 1 (fix:+ i 1))
		  (k 0 (if (vector-ref seen? i) k (fix:+ k 1))))
		 ((not (fix:< i n))
		  (let ((v (vector-cons k #f)))
		    (do ((i 1 (fix:+ i 1))
			 (j 0
			    (if (vector-ref seen? i)
				j
				(begin
				  (vector-set! v j i)
				  (fix:+ j 1)))))
			((not (fix:< i n))))
		    v))))))
    (letrec
	((constructor
	  (lambda field-values
	    (let ((lose
		   (lambda ()
		     (error:wrong-number-of-arguments constructor
						      (length indexes)
						      field-values))))
	      (let ((record
		     (%make-record
		      (%record-type-instance-marker record-type)
		      (%record-type-length record-type))))
		(do ((indexes indexes (cdr indexes))
		     (values field-values (cdr values)))
		    ((not (pair? indexes))
		     (if (not (null? values)) (lose)))
		  (if (not (pair? values)) (lose))
		  (%record-set! record (car indexes) (car values)))
		(let ((v (%record-type-default-inits record-type))
		      (n (vector-length defaults)))
		  (do ((i 0 (fix:+ i 1)))
		      ((not (fix:< i n)))
		    (let* ((index (vector-ref defaults i))
			   (init (vector-ref v (fix:- index 1))))
		      (and init (%record-set! record index (init))))))
		record)))))
      constructor)))

(define (record-keyword-constructor record-type)
  (letrec
      ((constructor
	(lambda keyword-list
	  (let ((n (%record-type-length record-type)))
	    (let ((record
		   (%make-record (%record-type-instance-marker record-type) n))
		  (seen? (vector-cons n #f)))
	      (do ((kl keyword-list (cddr kl)))
		  ((not (and (pair? kl)
			     (symbol? (car kl))
			     (pair? (cdr kl))))
		   (if (not (null? kl))
		       (error:not-a keyword-list? keyword-list constructor)))
		(let ((i (record-type-field-index record-type (car kl) #t)))
		  (if (not (vector-ref seen? i))
		      (begin
			(%record-set! record i (cadr kl))
			(vector-set! seen? i #t)))))
	      (let ((v (%record-type-default-inits record-type)))
		(do ((i 1 (fix:+ i 1)))
		    ((not (fix:< i n)))
		  (if (not (vector-ref seen? i))
		      (let ((init (vector-ref v (fix:- i 1))))
			(and init (%record-set! record i (init)))))))
	      record)))))
    constructor))

(define (copy-record record)
  (guarantee record? record 'copy-record)
  (%copy-record record))

(define (record-predicate record-type)
  (guarantee record-type? record-type 'record-predicate)
  (dispatch-tag->predicate record-type))

(define (record-accessor record-type field-name)
  (guarantee record-type? record-type 'record-accessor)
  (let ((predicate (record-predicate record-type))
	(index (record-type-field-index record-type field-name #t)))
    (let-syntax
	((expand-cases
	  (sc-macro-transformer
	   (lambda (form use-env)
	     (declare (ignore use-env))
	     (let ((limit (cadr form))
		   (gen-accessor
		    (lambda (i)
		      `(lambda (record)
			 (if (not (%record-type-instance? record-type record))
			     (error:not-a predicate record))
			 (%record-ref record ,i)))))
	       (let loop ((i 1))
		 (if (fix:<= i limit)
		     `(if (fix:= index ,i)
			  ,(gen-accessor i)
			  ,(loop (fix:+ i 1)))
		     (gen-accessor 'index))))))))
      (expand-cases 16))))

(define (record-modifier record-type field-name)
  (guarantee record-type? record-type 'record-modifier)
  (let ((predicate (record-predicate record-type))
	(index (record-type-field-index record-type field-name #t)))
    (let-syntax
	((expand-cases
	  (sc-macro-transformer
	   (lambda (form use-env)
	     (declare (ignore use-env))
	     (let ((limit (cadr form))
		   (gen-accessor
		    (lambda (i)
		      `(lambda (record field-value)
			 (if (not (%record-type-instance? record-type record))
			     (error:not-a predicate record))
			 (%record-set! record ,i field-value)))))
	       (let loop ((i 1))
		 (if (fix:<= i limit)
		     `(if (fix:= index ,i)
			  ,(gen-accessor i)
			  ,(loop (fix:+ i 1)))
		     (gen-accessor 'index))))))))
      (expand-cases 16))))

(define record-copy copy-record)
(define record-updater record-modifier)

(define (record-type-field-index record-type name error?)
  (let* ((names (%record-type-field-names record-type))
	 (n (vector-length names)))
    (let loop ((i 0))
      (if (fix:< i n)
	  (if (eq? (vector-ref names i) name)
	      (fix:+ i 1)
	      (loop (fix:+ i 1)))
	  (and error?
	       (record-type-field-index record-type
					(error:no-such-slot record-type name)
					error?))))))

(define (->type-name object)
  (cond ((string? object) (string->symbol object))
	((symbol? object) object)
	(else (error:wrong-type-argument object "type name" #f))))

(define (list-of-unique-symbols? object)
  (and (list-of-type? object symbol?)
       (let loop ((elements object))
	 (if (pair? elements)
	     (and (not (memq (car elements) (cdr elements)))
		  (loop (cdr elements)))
	     #t))))

(define-guarantee record-type "record type")
(define-guarantee record "record")

;;;; Printing

(define-unparser-method %record?
  (standard-unparser-method '%record #f))

(define-unparser-method record?
  (standard-unparser-method
   (lambda (record)
     (strip-angle-brackets
      (dispatch-tag-name (record-type-descriptor record))))
   #f))

(add-boot-init!
 (lambda ()
   (define-unparser-method record-type?
     (simple-unparser-method 'record-type
       (lambda (type)
	 (list (dispatch-tag-name type)))))))

(define-pp-describer %record?
  (lambda (record)
    (let loop ((i (fix:- (%record-length record) 1)) (d '()))
      (if (fix:< i 0)
	  d
	  (loop (fix:- i 1)
		(cons (list i (%record-ref record i)) d))))))

(define-pp-describer record?
  (lambda (record)
    (let ((type (record-type-descriptor record)))
      (map (lambda (field-name)
	     `(,field-name
	       ,((record-accessor type field-name) record)))
	   (record-type-field-names type)))))

;;; For backwards compatibility:
(define (set-record-type-unparser-method! record-type method)
  (define-unparser-method (record-predicate record-type)
    method))

;;;; Runtime support for DEFINE-STRUCTURE

(define rtd:structure-type)
(define make-define-structure-type)
(define structure-type?)
(define structure-type/physical-type)
(define structure-type/name)
(define structure-type/field-names)
(define structure-type/field-indexes)
(define structure-type/default-inits)
(define structure-type/unparser-method)
(define structure-type/tag)
(define structure-type/length)
(add-boot-init!
 (lambda ()
   ;; unparser-method arg should be removed after 9.3 is released.
   (set! rtd:structure-type
	 (make-record-type "structure-type"
			   '(physical-type name field-names field-indexes
					   default-inits unparser-method tag
					   length)))
   (set! make-define-structure-type
	 (record-constructor rtd:structure-type))
   (set! structure-type?
	 (record-predicate rtd:structure-type))
   (set! structure-type/physical-type
	 (record-accessor rtd:structure-type 'physical-type))
   (set! structure-type/name
	 (record-accessor rtd:structure-type 'name))
   (set! structure-type/field-names
	 (record-accessor rtd:structure-type 'field-names))
   (set! structure-type/field-indexes
	 (record-accessor rtd:structure-type 'field-indexes))
   (set! structure-type/default-inits
	 (record-accessor rtd:structure-type 'default-inits))
   (set! structure-type/unparser-method
	 (record-accessor rtd:structure-type 'unparser-method))
   (set! structure-type/tag
	 (record-accessor rtd:structure-type 'tag))
   (set! structure-type/length
	 (record-accessor rtd:structure-type 'length))
   unspecific))

(define-integrable (structure-type/field-index type field-name)
  (vector-ref (structure-type/field-indexes type)
	      (structure-type/field-name-index type field-name)))

(define-integrable (structure-type/default-init type field-name)
  (structure-type/default-init-by-index
   type
   (structure-type/field-name-index type field-name)))

(define-integrable (structure-type/default-init-by-index type field-name-index)
  (vector-ref (structure-type/default-inits type) field-name-index))

(define (structure-type/field-name-index type field-name)
  (let ((names (structure-type/field-names type)))
    (let ((n (vector-length names)))
      (let loop ((i 0))
	(if (not (fix:< i n))
	    (error:no-such-slot type field-name))
	(if (eq? (vector-ref names i) field-name)
	    i
	    (loop (fix:+ i 1)))))))

(define (named-structure? object)
  (or (named-list? object)
      (named-vector? object)
      (record? object)))

(define (named-list? object)
  (and (pair? object)
       (structure-type-tag? (car object) 'list)
       (list? (cdr object))))

(define (named-vector? object)
  (and (vector? object)
       (fix:> (vector-length object) 0)
       (structure-type-tag? (vector-ref object 0) 'vector)))

(define (structure-type-tag? tag physical-type)
  (let ((type (tag->structure-type tag)))
    (and type
	 (eq? (structure-type/physical-type type) physical-type))))

(define (tag->structure-type tag)
  (if (structure-type? tag)
      tag
      (let ((type (named-structure/get-tag-description tag)))
	(and (structure-type? type)
	     type))))

;;; Starting here can be removed after 9.3 release...

(define (named-list-with-unparser? object)
  (and (named-list? object)
       (tag->unparser-method (car object))))

(define (named-vector-with-unparser? object)
  (and (named-vector? object)
       (tag->unparser-method (vector-ref object 0))))

(define (tag->unparser-method tag)
  (structure-type/unparser-method (tag->structure-type tag)))

;;; ...and ending here.

(define-pp-describer named-list?
  (lambda (pair)
    (let ((type (tag->structure-type (car pair))))
      (map (lambda (field-name index)
	     `(,field-name ,(list-ref pair index)))
	   (vector->list (structure-type/field-names type))
	   (vector->list (structure-type/field-indexes type))))))

(define-pp-describer named-vector?
  (lambda (vector)
    (let ((type (tag->structure-type (vector-ref vector 0))))
      (map (lambda (field-name index)
	     `(,field-name ,(vector-ref vector index)))
	   (vector->list (structure-type/field-names type))
	   (vector->list (structure-type/field-indexes type))))))

(define (define-structure/default-value-by-index type field-name-index)
  ((structure-type/default-init-by-index type field-name-index)))

(define (define-structure/keyword-constructor type)
  (let ((names (structure-type/field-names type))
	(indexes (structure-type/field-indexes type))
	(inits (structure-type/default-inits type))
	(tag (structure-type/tag type))
	(len (structure-type/length type)))
    (let ((n (vector-length names)))
      (lambda arguments
	(let ((v (vector-cons len #f)))
	  (if tag
	      (vector-set! v 0 tag))
	  (let ((seen? (make-vector n #f)))
	    (do ((args arguments (cddr args)))
		((not (pair? args)))
	      (if (not (pair? (cdr args)))
		  (error:not-a keyword-list? arguments #f))
	      (let ((field-name (car args)))
		(let loop ((i 0))
		  (if (not (fix:< i n))
		      (error:no-such-slot type field-name))
		  (if (eq? (vector-ref names i) field-name)
		      (if (not (vector-ref seen? i))
			  (begin
			    (vector-set! v
					 (vector-ref indexes i)
					 (cadr args))
			    (vector-set! seen? i #t)))
		      (loop (fix:+ i 1))))))
	    (do ((i 0 (fix:+ i 1)))
		((not (fix:< i n)))
	      (if (not (vector-ref seen? i))
		  (let ((init (vector-ref inits i)))
		    (and init (vector-set! v (vector-ref indexes i) (init)))))))
	  (if (eq? (structure-type/physical-type type) 'LIST)
	      (do ((i (fix:- len 1) (fix:- i 1))
		   (list '() (cons (vector-ref v i) list)))
		  ((not (fix:>= i 0)) list))
	      v))))))

;;;; Support for safe accessors

(define (define-structure/vector-accessor type field-name)
  (let ((index (structure-type/field-index type field-name)))
    (if (structure-type/tag type)
	(lambda (structure)
	  (check-vector-tagged structure type)
	  (vector-ref structure index))
	(lambda (structure)
	  (check-vector-untagged structure type)
	  (vector-ref structure index)))))

(define (define-structure/vector-modifier type field-name)
  (let ((index (structure-type/field-index type field-name)))
    (if (structure-type/tag type)
	(lambda (structure value)
	  (check-vector-tagged structure type)
	  (vector-set! structure index value))
	(lambda (structure value)
	  (check-vector-untagged structure type)
	  (vector-set! structure index value)))))

(define (define-structure/list-accessor type field-name)
  (let ((index (structure-type/field-index type field-name)))
    (if (structure-type/tag type)
	(lambda (structure)
	  (check-list-tagged structure type)
	  (list-ref structure index))
	(lambda (structure)
	  (check-list-untagged structure type)
	  (list-ref structure index)))))

(define (define-structure/list-modifier type field-name)
  (let ((index (structure-type/field-index type field-name)))
    (if (structure-type/tag type)
	(lambda (structure value)
	  (check-list-tagged structure type)
	  (set-car! (list-tail structure index) value))
	(lambda (structure value)
	  (check-list-untagged structure type)
	  (set-car! (list-tail structure index) value)))))

(define-integrable (check-vector-tagged structure type)
  (if (not (and (vector? structure)
		(fix:= (vector-length structure)
		       (structure-type/length type))
		(eq? (vector-ref structure 0) (structure-type/tag type))))
      (error:wrong-type-argument structure type #f)))

(define-integrable (check-vector-untagged structure type)
  (if (not (and (vector? structure)
		(fix:= (vector-length structure)
		       (structure-type/length type))))
      (error:wrong-type-argument structure type #f)))

(define-integrable (check-list-tagged structure type)
  (if (not (and (eq? (list?->length structure) (structure-type/length type))
		(eq? (car structure) (structure-type/tag type))))
      (error:wrong-type-argument structure type #f)))

(define-integrable (check-list-untagged structure type)
  (if (not (eq? (list?->length structure) (structure-type/length type)))
      (error:wrong-type-argument structure type #f)))

;;;; Conditions

(define condition-type:slot-error)
(define condition-type:uninitialized-slot)
(define condition-type:no-such-slot)
(define error:uninitialized-slot)
(define error:no-such-slot)

(define (initialize-conditions!)
  (set! condition-type:slot-error
	(make-condition-type 'SLOT-ERROR condition-type:cell-error
	    '()
	  (lambda (condition port)
	    (write-string "Anonymous error for slot " port)
	    (write (access-condition condition 'LOCATION) port)
	    (write-string "." port))))
  (set! condition-type:uninitialized-slot
	(make-condition-type 'UNINITIALIZED-SLOT condition-type:slot-error
	    '(RECORD)
	  (lambda (condition port)
	    (write-string "Attempt to reference slot " port)
	    (write (access-condition condition 'LOCATION) port)
	    (write-string " in record " port)
	    (write (access-condition condition 'RECORD) port)
	    (write-string " failed because the slot is not initialized."
			  port))))
  (set! condition-type:no-such-slot
	(make-condition-type 'NO-SUCH-SLOT condition-type:slot-error
	    '(RECORD-TYPE)
	  (lambda (condition port)
	    (write-string "No slot named " port)
	    (write (access-condition condition 'LOCATION) port)
	    (write-string " in records of type " port)
	    (write (access-condition condition 'RECORD-TYPE) port)
	    (write-string "." port))))
  (set! error:uninitialized-slot
	(let ((signal
	       (condition-signaller condition-type:uninitialized-slot
				    '(RECORD LOCATION)
				    standard-error-handler)))
	  (lambda (record index)
	    (let* ((location (%record-field-name record index))
		   (ls (write-to-string location)))
	      (call-with-current-continuation
	       (lambda (k)
		 (store-value-restart ls
				      (lambda (value)
					(%record-set! record index value)
					(k value))
		   (lambda ()
		     (use-value-restart
		      (string-append
		       "value to use instead of the contents of slot "
		       ls)
		      k
		      (lambda () (signal record location)))))))))))
  (set! error:no-such-slot
	(let ((signal
	       (condition-signaller condition-type:no-such-slot
				    '(RECORD-TYPE LOCATION)
				    standard-error-handler)))
	  (lambda (record-type name)
	    (call-with-current-continuation
	     (lambda (k)
	       (use-value-restart
		(string-append "slot name to use instead of "
			       (write-to-string name))
		k
		(lambda () (signal record-type name))))))))
  unspecific)

(define (%record-field-name record index)
  (or (and (fix:> index 0)
	   (record? record)
	   (let ((names
		  (%record-type-field-names (record-type-descriptor record))))
	     (and (fix:<= index (vector-length names))
		  (vector-ref names (fix:- index 1)))))
      index))

(define (record-type-field-name record-type index)
  (guarantee record-type? record-type 'record-type-field-name)
  (guarantee fix:fixnum? index 'record-type-field-name)
  (let ((names (%record-type-field-names record-type))
	(index* (fix:- index 1)))
    (if (not (fix:>= index* 0)
	     (fix:< index* (vector-length names)))
	(error:bad-range-argument index 'record-type-field-name))
    (vector-ref names index*)))

(define (store-value-restart location k thunk)
  (let ((location (write-to-string location)))
    (with-restart 'store-value
	(string-append "Initialize slot " location " to a given value.")
	k
	(string->interactor (string-append "Set " location " to"))
      thunk)))

(define (use-value-restart noun-phrase k thunk)
  (with-restart 'use-value
      (string-append "Specify a " noun-phrase ".")
      k
      (string->interactor (string-titlecase noun-phrase))
    thunk))

(define ((string->interactor string))
  (values (prompt-for-evaluated-expression string)))