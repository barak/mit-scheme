;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/macros.scm,v 1.43 1989/03/14 08:01:25 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Editor Macros

(declare (usual-integrations))

(define edwin-syntax-table
  (make-syntax-table syntax-table/system-internal))

;;; DEFINE-NAMED-STRUCTURE is a simple alternative to DEFSTRUCT,
;;; which defines a vector-based tagged data structure.  The first
;;; argument is a string, which will be stored in the structure's 0th
;;; slot.  The remaining arguments are symbols, which should be the
;;; names of the slots.  Do not use the slot names %TAG or %SIZE.

(syntax-table-define edwin-syntax-table 'DEFINE-NAMED-STRUCTURE
  (lambda (name . slots)
    (define ((make-symbols x) y)
      (make-symbol x y))

    (define (make-symbol . args)
      (string->symbol (apply string-append args)))

    (let ((structure-string (string-upcase name))
	  (slot-strings (map symbol->string slots)))
      (let ((prefix (string-append structure-string "-")))
	(let ((structure-name (string->symbol structure-string))
	      (tag-name (make-symbol "%" prefix "TAG"))
	      (constructor-name (make-symbol "%MAKE-" structure-string))
	      (predicate-name (make-symbol structure-string "?"))
	      (slot-names
	       (map (make-symbols (string-append prefix "INDEX:"))
		    slot-strings))
	      (selector-names (map (make-symbols prefix) slot-strings)))
	  (define (slot-loop slot-names n)
	    (if (null? slot-names)
		'()
		(cons `(DEFINE ,(car slot-names) ,n)
		      (slot-loop (cdr slot-names) (1+ n)))))

	  (define (selector-loop selector-names n)
	    (if (null? selector-names)
		'()
		(cons `(DEFINE-INTEGRABLE
			 (,(car selector-names) ,structure-name)
			 (VECTOR-REF ,structure-name ,n))
		      (selector-loop (cdr selector-names) (1+ n)))))

	  `(BEGIN (DEFINE ,tag-name ,name)
		  (DEFINE (,constructor-name)
		    (LET ((,structure-name
			   (MAKE-VECTOR ,(1+ (length slots)) '())))
		      (VECTOR-SET! ,structure-name 0 ,tag-name)
		      ,structure-name))
		  (DEFINE (,predicate-name OBJECT)
		    (AND (VECTOR? OBJECT)
			 (NOT (ZERO? (VECTOR-LENGTH OBJECT)))
			 (EQ? ,tag-name (VECTOR-REF OBJECT 0))))
		  (UNPARSER/SET-TAGGED-VECTOR-METHOD!
		   ,tag-name
		   (UNPARSER/STANDARD-METHOD ',structure-name))
		  ,@(slot-loop slot-names 1)
		  ,@(selector-loop selector-names 1)))))))

(syntax-table-define edwin-syntax-table 'DEFINE-COMMAND
  (lambda (bvl description . body)
    (let ((name (car bvl))
	  (bvl (cdr bvl)))
      (let ((pname (symbol-append (canonicalize-name name) '-COMMAND)))
	`(BEGIN
	   ,(if (null? bvl)
		(let ((argument (string->uninterned-symbol "ARGUMENT")))
		  `(DEFINE (,pname #!OPTIONAL ,argument)
		     ,argument		;ignore
		     ,@body))
		(let ((arg-names
		       (map (lambda (arg) (if (pair? arg) (car arg) arg))
			    bvl)))
		  `(DEFINE (,pname #!OPTIONAL ,@arg-names)
		     (LET* ,(map (lambda (name arg)
				   (let ((init (and (pair? arg) (cadr arg))))
				     `(,name
				       (IF ,(if (not init)
						`(DEFAULT-OBJECT? ,name)
						`(OR (DEFAULT-OBJECT? ,name)
						     (NOT ,name)))
					   ,init
					   ,name))))
				 arg-names
				 bvl)
		       ,@body))))
	   (MAKE-COMMAND ',name ',description ,pname))))))

(syntax-table-define edwin-syntax-table 'DEFINE-VARIABLE
  (lambda (name description . tail)
    (let ((variable-name (canonicalize-name name)))
      `(BEGIN
	 (DEFINE ,variable-name ,@tail)
	 (MAKE-VARIABLE ',name ',description ',variable-name)))))

(syntax-table-define edwin-syntax-table 'REF-VARIABLE
  (lambda (name)
    (canonicalize-name name)))

(syntax-table-define edwin-syntax-table 'SET-VARIABLE!
  (lambda (name . tail)
    `(BEGIN
       (SET! ,(canonicalize-name name) ,@tail)
       UNSPECIFIC)))

(syntax-table-define edwin-syntax-table 'GLOBAL-SET-VARIABLE!
  (lambda (name . tail)
    (let ((variable-name (canonicalize-name name)))
      `(BEGIN
	 (UNMAKE-LOCAL-BINDING! ',variable-name)
	 (SET! ,variable-name ,@tail)
	 UNSPECIFIC))))

(syntax-table-define edwin-syntax-table 'LOCAL-SET-VARIABLE!
  (lambda (name . tail)
    `(MAKE-LOCAL-BINDING! ',(canonicalize-name name) ,@tail)))

(syntax-table-define edwin-syntax-table 'DEFINE-MAJOR-MODE
  (lambda (name super-mode-name description . initialization)
    (let ((vname (mode-name->variable name)))
      `(DEFINE ,vname
	 (MAKE-MODE ',name
		    TRUE
		    ,(if super-mode-name
			 `(MODE-COMTABS (NAME->MODE ',super-mode-name))
			 ''())
		    ',description
		    (LAMBDA ()
		      ,@(let ((initialization
			       (if super-mode-name
				   `(((MODE-INITIALIZATION
				       ,(mode-name->variable super-mode-name)))
				     ,@initialization)
				   initialization)))
			  (if (null? initialization)
			      `(',unspecific)
			      initialization))))))))

(syntax-table-define edwin-syntax-table 'DEFINE-MINOR-MODE
  (lambda (name description . initialization)
    (let ((vname (mode-name->variable name)))
      `(DEFINE ,vname
	 (MAKE-MODE ',name
		    FALSE
		    '()
		    ',description
		    (LAMBDA ()
		      ,@(if (null? initialization)
			    `(',unspecific)
			    initialization)))))))

(define-integrable (mode-name->variable name)
  (symbol-append (canonicalize-name name) '-MODE))

(define (canonicalize-name name)
  (cond ((symbol? name) name)
	((string? name) (intern (string-replace name #\Space #\-)))
	(else (error "illegal name" name))))