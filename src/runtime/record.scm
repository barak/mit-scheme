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

;;;; Records
;;; package: (runtime record)

;;; adapted from JAR's implementation
;;; conforms to R7RS and SRFI 131

(declare (usual-integrations))
(declare (integrate-external "dispatch-tag"))

(add-boot-deps! '(runtime tagged-dispatch))

(define-primitives
  (vector-cons 2))

(define (make-record-type type-name field-specs . options)
  (guarantee valid-field-specs? field-specs 'make-record-type)
  (let ((type-name (->type-name type-name 'make-record-type)))
    (receive (parent-type applicator instance-marker)
	(make-record-type-options (if (and (pair? options)
					   (null? (cdr options)))
				      ;; SRFI 131 compatibility
				      (cons 'parent-type options)
				      options)
				  'make-record-type)
      (if parent-type
	  (begin
	    (if applicator
		(error:bad-range-argument applicator 'make-record-type))
	    (if (and instance-marker
		     (not (%record-type-fasdumpable? parent-type)))
		(error:bad-range-argument instance-marker 'make-record-type))))
      (%make-record-type type-name field-specs parent-type
			 applicator instance-marker))))

(define (valid-field-specs? object)
  (and (list? object)
       (every field-spec? object)
       (not (any-duplicates? object eq? field-spec-name))))
(register-predicate! valid-field-specs? 'valid-field-specs '<= list?)

(define (field-spec? object)
  (or (symbol? object)
      (and (pair? object)
	   (symbol? (car object))
	   (or (and (pair? (cdr object))
		    (%valid-default-init? (cadr object))
		    (null? (cddr object)))
	       (keyword-list? (cdr object))))))

(define (make-field-spec name init)
  (if init
      (list name 'default-init init)
      name))

(define (field-spec-name spec)
  (if (pair? spec) (car spec) spec))

(define (field-spec-options spec)
  (if (pair? spec) (cdr spec) '()))

(define (%valid-default-init? object)
  (or (not object)
      (thunk? object)))

(define %record-metatag)
(define record-type?)
(define %%make-record-type)
(define make-record-type-options)
(define make-record-field-options)
(add-boot-init!
 (lambda ()
   (set! %record-metatag (make-dispatch-metatag 'record-tag))
   (set! record-type? (dispatch-tag->predicate %record-metatag))
   (set! %%make-record-type
	 (dispatch-metatag-constructor %record-metatag 'make-record-type))
   (set! make-record-type-options
	 (keyword-option-parser
	  (list (list 'parent-type record-type? (lambda () #f))
		(list 'applicator procedure? (lambda () #f))
		(list 'instance-marker %record-type-proxy? (lambda () #f)))))
   (set! make-record-field-options
	 (keyword-option-parser
	  (list (list 'default-init thunk? (lambda () #f)))))
   unspecific))

(define (->type-name object caller)
  (cond ((string? object) (string->symbol object))
	((symbol? object) object)
	(else (error:wrong-type-argument object "type name" caller))))

(define (%make-record-type type-name field-specs parent-type
			   applicator instance-marker)
  (let* ((start-index (if parent-type (%record-type-end-index parent-type) 0))
	 (end-index (+ start-index 1 (length field-specs))))

    (define (make-type predicate)
      (let ((fields-by-index
	     (generate-fields-by-index field-specs parent-type start-index)))
	(%%make-record-type type-name
			    predicate
			    start-index
			    end-index
			    fields-by-index
			    (generate-fields-by-name fields-by-index)
			    parent-type
			    applicator
			    instance-marker)))

    (let ((type
	   (if instance-marker
	       (%make-marked-type start-index end-index parent-type
				  instance-marker make-type)
	       (%make-normal-type start-index end-index parent-type
				  make-type))))
      (set-predicate<=! (record-predicate type)
			(if parent-type
			    (record-predicate parent-type)
			    record?))
      type)))

(define (generate-fields-by-index field-specs parent-type start-index)
  (let ((partial-fields
	 (let ((v (make-vector (length field-specs)))
	       (offset (fix:+ start-index 1)))
	   (do ((specs field-specs (cdr specs))
		(index 0 (fix:+ index 1)))
	       ((not (pair? specs)) v)
	     (receive (init) (parse-field-options (car specs))
	       (vector-set! v
			    index
			    (make-field (field-spec-name (car specs))
					init
					(fix:+ offset index))))))))
    (if parent-type
	(vector-append (%record-type-fields-by-index parent-type)
		       partial-fields)
	partial-fields)))

(define (parse-field-options field-spec)
  (let ((options (field-spec-options field-spec)))
    (if (and (pair? options) (null? (cdr options)))
	;; backwards compatibility
	(values (car options))
	(make-record-field-options options 'make-record-type))))

(define (generate-fields-by-name fields-by-index)
  (let loop ((fields (reverse (vector->list fields-by-index))) (filtered '()))
    (if (pair? fields)
	(loop (cdr fields)
	      (if (any (let ((name (field-name (car fields))))
			 (lambda (field)
			   (eq? (field-name field) name)))
		       filtered)
		  filtered
		  (cons (car fields) filtered)))
	(let ((v (list->vector filtered)))
	  (sort! v
		 (lambda (f1 f2)
		   (symbol<? (field-name f1)
			     (field-name f2))))
	  v))))

(define-integrable (make-field name init index)
  (vector name init index))

(define-integrable (field-name field)
  (vector-ref field 0))

(define-integrable (field-init field)
  (vector-ref field 1))

(define-integrable (field-index field)
  (vector-ref field 2))

(define (%make-marked-type start-index end-index parent-type instance-marker
			   make-type)
  (make-type
   (cond ((not parent-type)
	  (lambda (object)
	    (%pred=0 instance-marker object)))
	 ((not (%record-type-parent parent-type))
	  (let ((marker2 (%record-type-instance-marker parent-type)))
	    (lambda (object)
	      (%pred=1 start-index end-index instance-marker marker2 object))))
	 (else
	  (let ((index2 (%record-type-start-index parent-type))
		(marker2 (%record-type-instance-marker parent-type))
		(type3 (%record-type-parent parent-type)))
	    (let ((marker3 (%record-type-instance-marker type3)))
	      (lambda (object)
		(%pred>1 start-index end-index instance-marker index2 marker2
			 marker3 type3 object))))))))

(define (%make-normal-type start-index end-index parent-type make-type)
  (letrec
      ((type
	(make-type
	 (cond ((not parent-type)
		(lambda (object)
		  (%pred=0 type object)))
	       ((not (%record-type-parent parent-type))
		(let ((marker2 (%record-type-instance-marker parent-type)))
		  (lambda (object)
		    (%pred=1 start-index end-index type marker2 object))))
	       (else
		(let ((index2 (%record-type-start-index parent-type))
		      (marker2 (%record-type-instance-marker parent-type))
		      (type3 (%record-type-parent parent-type)))
		  (let ((marker3 (%record-type-instance-marker type3)))
		    (lambda (object)
		      (%pred>1 start-index end-index type index2 marker2
			       marker3 type3 object)))))))))
    (%set-record-type-instance-marker! type type)
    type))

(define-integrable (%pred=0 marker1 object)
  (and (%record? object)
       (%pred-check-marker 0 marker1 object)))

(define-integrable (%pred=1 start-index end-index marker1 marker2 object)
  (and (%pred-prefix end-index object)
       (%pred-check-marker start-index marker1 object)
       (%pred-check-marker 0 marker2 object)))

(define-integrable (%pred>1 start-index end-index marker1 start2 marker2
			    marker3 type3 object)
  (and (%pred-prefix end-index object)
       (%pred-check-marker start-index marker1 object)
       (%pred-check-marker start2 marker2 object)
       (%pred-check-marker 0 marker3 object)
       (let loop ((type (%record-type-parent type3)))
	 (if type
	     (and (%pred-check-marker (%record-type-start-index type)
				      (%record-type-instance-marker type)
				      object)
		  (loop (%record-type-parent type)))
	     #t))))

(define-integrable (%pred-prefix end-index object)
  (and (%record? object)
       (fix:>= (%record-length object) end-index)))

(define-integrable (%pred-check-marker index marker object)
  (eq? marker (%record-ref object index)))

(define-integrable (%record-type-start-index record-type)
  (%dispatch-tag-extra-ref record-type 0))

(define-integrable (%record-type-end-index record-type)
  (%dispatch-tag-extra-ref record-type 1))

(define-integrable (%record-type-fields-by-index record-type)
  (%dispatch-tag-extra-ref record-type 2))

(define-integrable (%record-type-fields-by-name record-type)
  (%dispatch-tag-extra-ref record-type 3))

(define-integrable (%record-type-parent record-type)
  (%dispatch-tag-extra-ref record-type 4))

(define-integrable (%record-type-applicator record-type)
  (%dispatch-tag-extra-ref record-type 5))

(seq:after-microcode-tables 'add-action!
  (lambda ()
    (set-fixed-objects-item! 'record-dispatch-tag %record-metatag)
    (set-fixed-objects-item! 'record-applicator-index
			     (%dispatch-tag-extra-index 5))))

(define-integrable (%record-type-instance-marker record-type)
  (%dispatch-tag-extra-ref record-type 6))

(define (%set-record-type-instance-marker! record-type marker)
  (%dispatch-tag-extra-set! record-type 6 marker))

(define (%record-type-field-by-name record-type name)
  (or (%record-type-field-by-name-no-error record-type name)
      (%record-type-field-by-name record-type
				  (error:no-such-slot record-type name))))

(define (%record-type-field-by-name-no-error record-type name)
  (vector-binary-search (%record-type-fields-by-name record-type)
			symbol<?
			field-name
			name))

(define (%record-type-field-by-index record-type index)
  (or (%record-type-field-by-index-no-error record-type index)
      (%record-type-field-by-index record-type
				   (error:no-such-slot record-type index))))

(define (%record-type-field-by-index-no-error record-type index)
  (vector-binary-search (%record-type-fields-by-index record-type)
			fix:<
			field-index
			index))

(define (%record-type-fasdumpable? type)
  (%record-type-proxy? (%record-type-instance-marker type)))

(define (record-type-name record-type)
  (guarantee record-type? record-type 'record-type-name)
  (symbol->string (%dispatch-tag-name record-type)))

(define (record-type-field-names record-type)
  (guarantee record-type? record-type 'record-type-field-names)
  (%record-type-field-names record-type))

(define (%record-type-field-names record-type)
  (%map-record-type-fields field-name
			   (%record-type-fields-by-index record-type)))

(define (record-type-field-specs record-type)
  (guarantee record-type? record-type 'record-type-field-specs)
  (%map-record-type-fields (lambda (field)
			     (make-field-spec (field-name field)
					      (field-init field)))
			   (%record-type-fields-by-index record-type)))

(define (%map-record-type-fields procedure fields)
  (let loop ((i (fix:- (vector-length fields) 1)) (tail '()))
    (if (fix:>= i 0)
	(loop (fix:- i 1)
	      (cons (procedure (vector-ref fields i))
		    tail))
	tail)))

(define (record-type-field-index record-type name)
  (guarantee record-type? record-type 'record-type-field-index)
  (guarantee symbol? name 'record-type-field-index)
  (let ((field (%record-type-field-by-name-no-error record-type name)))
    (and field
	 (field-index field))))

(define (record-type-parent record-type)
  (guarantee record-type? record-type 'record-type-parent)
  (%record-type-parent record-type))

(define (applicable-record? object)
  (and (%record->applicator object) #t))

(define (%record->applicator object)
  (and (%record? object)
       (let ((record-type (%record->root-type object)))
	 (and record-type
	      (%record-type-applicator record-type)))))

(define (record-applicator record)
  (let ((applicator (%record->applicator record)))
    (if (not applicator)
	(error:not-a applicable-record? record 'record-applicator))
    applicator))

(define (%record-type-proxy? object)
  (and (object-type? (ucode-type constant) object)
       (let ((v (object-new-type (ucode-type fixnum) object)))
	 (and (fix:>= v #x100)
	      (fix:< v #x200)))))
(register-predicate! %record-type-proxy? 'record-type-proxy)

(define-integrable (%record-type-proxy->index marker)
  (fix:- (object-new-type (ucode-type fixnum) marker) #x100))

(define-integrable (%index->record-type-proxy index)
  (object-new-type (ucode-type constant) (fix:+ index #x100)))

(define-integrable (%proxy->record-type proxy)
  (vector-ref %proxied-record-types (%record-type-proxy->index proxy)))

(define-integrable (%set-proxied-record-type! proxy type)
  (vector-set! %proxied-record-types (%record-type-proxy->index proxy) type))

(define %proxied-record-types)
(seq:after-microcode-tables 'add-action!
  (lambda ()
    (set! %proxied-record-types (fixed-objects-item 'proxied-record-types))
    unspecific))

(let-syntax
    ((enumerate-proxies
      (sc-macro-transformer
       (lambda (form use-env)
	 (syntax-check '(_ * symbol) form)
	 `(begin
	    ,@(map (lambda (name index)
		     `(define ,(symbol 'record-type-proxy: name)
			(%index->record-type-proxy ,index)))
		   (cdr form)
		   (iota (length (cdr form)))))))))
  (enumerate-proxies pathname host))

(define (record? object)
  (and (%record? object)
       (%record->root-type object)
       #t))

(define (record-type-descriptor record)
  (guarantee record? record 'record-type-descriptor)
  (%record->leaf-type record))

(define-integrable (%record->root-type record)
  (%record-type-ref record 0))

(define (%record->leaf-type record)
  (let loop ((type (%record-type-ref record 0)))
    (let ((type*
	   (let ((end (%record-type-end-index type)))
	     (and (fix:> (%record-length record) end)
		  (%record-type-ref record end)))))
      (if type*
	  (loop type*)
	  type))))

(define (%record-type-ref record index)
  (let ((marker (%record-ref record index)))
    (cond ((record-type? marker) marker)
	  ((%record-type-proxy? marker) (%proxy->record-type marker))
	  (else #f))))

(define (record-constructor record-type #!optional field-names)
  (guarantee record-type? record-type 'record-constructor)
  (if (or (default-object? field-names)
	  (%default-field-names? record-type field-names))
      (%record-constructor-default-names record-type)
      (begin
	(guarantee list? field-names 'record-constructor)
	(if (any-duplicates? field-names eq?)
	    (error:bad-range-argument field-names 'record-constructor))
	(%record-constructor-given-names record-type field-names))))

(define (%default-field-names? record-type field-names)
  (let* ((fields (%record-type-fields-by-index record-type))
	 (n-fields (vector-length fields)))
    (let loop ((names field-names) (i 0))
      (if (and (pair? names) (fix:< i n-fields))
	  (and (eq? (car names) (field-name (vector-ref fields i)))
	       (loop (cdr names) (fix:+ i 1)))
	  (and (null? names) (fix:= i n-fields))))))

(define (%typed-record-maker record-type)
  (if (%record-type-parent record-type)
      (lambda ()
	(let ((record (%make-record #f (%record-type-end-index record-type))))
	  (let loop ((type record-type))
	    (%record-set! record
			  (%record-type-start-index type)
			  (%record-type-instance-marker type))
	    (if (%record-type-parent type)
		(loop (%record-type-parent type))))
	  record))
      (lambda ()
	(%make-record (%record-type-instance-marker record-type)
		      (%record-type-end-index record-type)))))

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
      (let* ((indices
	      (vector-map field-index
			  (%record-type-fields-by-index record-type)))
	     (arity (vector-length indices))
	     (%make-typed-record (%typed-record-maker record-type)))

	(define (general-case)
	  (define (constructor . field-values)
	    (if (not (fix:= arity (length field-values)))
		(error:wrong-number-of-arguments constructor
						 arity
						 field-values))
	    (let ((record (%make-typed-record)))
	      (do ((i 0 (fix:+ i 1))
		   (vals field-values (cdr vals)))
		  ((not (fix:< i arity)) unspecific)
		(%record-set! record (vector-ref indices i) (car vals)))
	      record))
	  constructor)

	(if (%record-type-parent record-type)
	    (general-case)
	    (expand-cases record-type arity 16 (general-case)))))))

(define (%record-constructor-given-names record-type field-names)
  (let* ((fields
	  (map (lambda (field-name)
		 (%record-type-field-by-name record-type field-name))
	       field-names))
	 (defaults
	  (list->vector
	   (filter field-init
		   (lset-difference
		    eq?
		    (vector->list
		     (%record-type-fields-by-index record-type))
		    fields))))
	 (indices (list->vector (map field-index fields)))
	 (arity (vector-length indices))
	 (%make-typed-record (%typed-record-maker record-type)))

    (define (constructor . field-values)
      (if (not (fix:= arity (length field-values)))
	  (error:wrong-number-of-arguments constructor arity field-values))

      (let ((record (%make-typed-record)))

	(do ((i 0 (fix:+ i 1))
	     (vals field-values (cdr vals)))
	    ((not (fix:< i arity)) unspecific)
	  (%record-set! record (vector-ref indices i) (car vals)))

	(let ((n (vector-length defaults)))
	  (do ((i 0 (fix:+ i 1)))
	      ((not (fix:< i n)) unspecific)
	    (let ((field (vector-ref defaults i)))
	      (%record-set! record
			    (field-index field)
			    ((field-init field))))))
	record))

    constructor))

(define (record-keyword-constructor record-type)
  (guarantee record-type? record-type 'record-keyword-constructor)
  (let ((names (%record-type-field-names record-type))
	(%make-typed-record (%typed-record-maker record-type)))

    (define (constructor . keyword-list)
      (if (not (restricted-keyword-list? keyword-list names))
	  (error:not-a keyword-list? keyword-list constructor))

      (let ((record (%make-typed-record))
	    (all-fields
	     (cons #f
		   (vector->list (%record-type-fields-by-index record-type)))))

	(define (set-value! name value)
	  (let ((field (%record-type-field-by-name record-type name)))
	    (let loop ((fields (cdr all-fields)) (prev all-fields))
	      (if (pair? fields)
		  (if (eq? field (car fields))
		      (set-cdr! prev (cdr fields))
		      (loop (cdr fields) fields))
		  (error "Duplicate keyword:" name)))
	    (%record-set! record (field-index field) value)))

	(do ((kl keyword-list (cddr kl)))
	    ((not (pair? kl)) unspecific)
	  (set-value! (car kl) (cadr kl)))

	(let loop ((fields (cdr all-fields)))
	  (if (pair? fields)
	      (begin
		(if (field-init (car fields))
		    (%record-set! record
				  (field-index (car fields))
				  ((field-init (car fields)))))
		(loop (cdr fields)))))

	record))

    constructor))

(define (copy-record record)
  (guarantee record? record 'copy-record)
  (%copy-record record))

(define (%copy-record record)
  (let ((length (%record-length record)))
    (let ((result (%make-record (%record-ref record 0) length)))
      (do ((index 1 (fix:+ index 1)))
	  ((not (fix:< index length)) unspecific)
	(%record-set! result index (%record-ref record index)))
      result)))

(define (record-predicate record-type)
  (guarantee record-type? record-type 'record-predicate)
  (%dispatch-tag->predicate record-type))

(define (record-accessor record-type field-name)
  (guarantee record-type? record-type 'record-accessor)
  (let ((predicate (record-predicate record-type))
	(index
	 (field-index (%record-type-field-by-name record-type field-name))))
    (let-syntax
	((expand-cases
	  (sc-macro-transformer
	   (lambda (form use-env)
	     (declare (ignore use-env))
	     (let ((limit (cadr form))
		   (gen-accessor
		    (lambda (i)
		      `(lambda (record)
			 (guarantee predicate record)
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
	(index
	 (field-index (%record-type-field-by-name record-type field-name))))
    (let-syntax
	((expand-cases
	  (sc-macro-transformer
	   (lambda (form use-env)
	     (declare (ignore use-env))
	     (let ((limit (cadr form))
		   (gen-accessor
		    (lambda (i)
		      `(lambda (record field-value)
			 (guarantee predicate record)
			 (%record-set! record ,i field-value)))))
	       (let loop ((i 1))
		 (if (fix:<= i limit)
		     `(if (fix:= index ,i)
			  ,(gen-accessor i)
			  ,(loop (fix:+ i 1)))
		     (gen-accessor 'index))))))))
      (expand-cases 16))))

;;;; Printing

(define-print-method %record?
  (standard-print-method '%record))

(define-pp-describer %record?
  (lambda (record)
    (let loop ((i (fix:- (%record-length record) 1)) (d '()))
      (if (fix:< i 0)
	  d
	  (loop (fix:- i 1)
		(cons (list i (%record-ref record i)) d))))))

(define-print-method record?
  (standard-print-method
   (lambda (record)
     (dispatch-tag-print-name (record-type-descriptor record)))))

(define-pp-describer record?
  (lambda (record)
    (let ((type (record-type-descriptor record)))
      (map (lambda (field)
	     `(,(field-name field)
	       ,(%record-ref record (field-index field))))
	   (vector->list (%record-type-fields-by-index type))))))

(add-boot-init!
 (lambda ()
   (define-print-method record-type? %print-record-type)
   (define-pp-describer record-type? %pp-record-type)))

(define %print-record-type
  (standard-print-method 'record-type
    (lambda (type)
      (list (dispatch-tag-print-name type)))))

(define (%pp-record-type record-type)
  `((name ,(%dispatch-tag-name record-type))
    (predicate ,(%dispatch-tag->predicate record-type))
    (start-index ,(%record-type-start-index record-type))
    (end-index ,(%record-type-end-index record-type))
    (fields-by-index ,(%record-type-fields-by-index record-type))
    (fields-by-name ,(%record-type-fields-by-name record-type))
    (parent ,(%record-type-parent record-type))
    (instance-marker ,(%record-type-instance-marker record-type))
    (applicator ,(%record-type-applicator record-type))))

;;; For backwards compatibility:
(define (set-record-type-unparser-method! record-type method)
  (define-print-method (record-predicate record-type)
    method))

;;;; Runtime support for DEFINE-STRUCTURE

(define rtd:structure-type)
;; RELNOTE: rename without "new-"
(define new-make-define-structure-type)
(define structure-type?)
(define structure-type/physical-type)
(define structure-type/name)
(define structure-type/field-names)
(define structure-type/field-indexes)
(define structure-type/default-inits)
(define structure-type/tag)
(define structure-type/length)
(add-boot-init!
 (lambda ()
   (set! rtd:structure-type
	 (make-record-type "structure-type"
			   '(physical-type name field-names field-indexes
					   default-inits tag length)))
   (set! new-make-define-structure-type
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
   (set! structure-type/tag
	 (record-accessor rtd:structure-type 'tag))
   (set! structure-type/length
	 (record-accessor rtd:structure-type 'length))
   unspecific))

;; RELNOTE: delete
(define (make-define-structure-type physical-type name field-names field-indexes
				    default-inits unparser-method tag length)
  (declare (ignore unparser-method))
  (new-make-define-structure-type physical-type name field-names field-indexes
				  default-inits tag length))

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

(define named-structure-descriptions)
(seq:after-thread-low 'add-action!
  (lambda ()
    (set! named-structure-descriptions (make-1d-table))
    unspecific))

(define (named-structure/get-tag-description tag)
  (1d-table/get named-structure-descriptions tag #f))

(define (named-structure/set-tag-description! tag description)
  (1d-table/put! named-structure-descriptions tag description))

(define (define-structure/default-value-by-index type field-name-index)
  ((structure-type/default-init-by-index type field-name-index)))

(define (record-type-default-value-by-index record-type field-index)
  (guarantee record-type? record-type 'record-type-default-value-by-index)
  (let ((init
	 (field-init (%record-type-field-by-index record-type field-index))))
    (and init
	 (init))))

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
		((not (pair? args)) unspecific)
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
		((not (fix:< i n)) unspecific)
	      (if (not (vector-ref seen? i))
		  (let ((init (vector-ref inits i)))
		    (and init (vector-set! v (vector-ref indexes i) (init)))))))
	  (if (eq? (structure-type/physical-type type) 'list)
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
	  (set-car! (drop structure index) value))
	(lambda (structure value)
	  (check-list-untagged structure type)
	  (set-car! (drop structure index) value)))))

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
(seq:after-conditions 'add-action!
  (lambda ()
   (set! condition-type:slot-error
	 (make-condition-type 'slot-error condition-type:cell-error
	     '()
	   (lambda (condition port)
	     (write-string "Anonymous error for slot " port)
	     (write (access-condition condition 'location) port)
	     (write-string "." port))))
   (set! condition-type:uninitialized-slot
	 (make-condition-type 'uninitialized-slot condition-type:slot-error
	     '(record)
	   (lambda (condition port)
	     (write-string "Attempt to reference slot " port)
	     (write (access-condition condition 'location) port)
	     (write-string " in record " port)
	     (write (access-condition condition 'record) port)
	     (write-string " failed because the slot is not initialized."
			   port))))
   (set! condition-type:no-such-slot
	 (make-condition-type 'no-such-slot condition-type:slot-error
	     '(record-type)
	   (lambda (condition port)
	     (write-string "No slot named " port)
	     (write (access-condition condition 'location) port)
	     (write-string " in records of type " port)
	     (write (access-condition condition 'record-type) port)
	     (write-string "." port))))
   (set! error:uninitialized-slot
	 (let ((signal
		(condition-signaller condition-type:uninitialized-slot
				     '(record location)
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
				     '(record-type location)
				     standard-error-handler)))
	   (lambda (record-type name)
	     (call-with-current-continuation
	      (lambda (k)
		(use-value-restart
		 (string-append "slot name to use instead of "
				(write-to-string name))
		 k
		 (lambda () (signal record-type name))))))))
   unspecific))

(define (%record-field-name record index)
  (or (let ((type (and (record? record) (%record->leaf-type record))))
	(and type
	     (let ((field (%record-type-field-by-index-no-error type index)))
	       (and field
		    (field-name field)))))
      index))

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