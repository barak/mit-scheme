;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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
  (make-syntax-table system-global-syntax-table))

(define edwin-macros
  (make-environment

;;; DEFINE-NAMED-STRUCTURE is a simple alternative to DEFSTRUCT,
;;; which defines a vector-based tagged data structure.  The first
;;; argument is a string, which will be stored in the structure's 0th
;;; slot.  The remaining arguments are symbols, which should be the
;;; names of the slots.  Do not use the slot names %TAG or %SIZE.

(syntax-table-define edwin-syntax-table 'DEFINE-NAMED-STRUCTURE
  (macro (name . slots)
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
			   (VECTOR-CONS ,(1+ (length slots)) '())))
		      (VECTOR-SET! ,structure-name 0 ,tag-name)
		      ,structure-name))
		  (DEFINE (,predicate-name OBJECT)
		    (AND (VECTOR? OBJECT)
			 (NOT (ZERO? (VECTOR-LENGTH OBJECT)))
			 (EQ? ,tag-name (VECTOR-REF OBJECT 0))))
		  ,@(slot-loop slot-names 1)
		  ,@(selector-loop selector-names 1)))))))

(syntax-table-define edwin-syntax-table 'DEFINE-INTEGRABLE
  (macro (name . body)
    `(BEGIN (DECLARE (INTEGRATE ,(if (pair? name) (car name) name)))
	    (DEFINE ,name
	      ,@(if (pair? name)
		    `((DECLARE (INTEGRATE ,@(cdr name))))
		    '())
	      ,@body))))

(syntax-table-define edwin-syntax-table 'DEFINE-COMMAND
  (macro (bvl description . body)
    (let ((name (car bvl))
	  (arg-names (map (lambda (arg) (if (pair? arg) (car arg) arg))
			  (cdr bvl)))
	  (arg-inits (map (lambda (arg) (and (pair? arg) (cadr arg)))
			  (cdr bvl))))
      (let ((procedure-name
	     (string->symbol
	      (string-append (canonicalize-name-string name)
			     "-COMMAND"))))
	`(BEGIN (DEFINE (,procedure-name #!OPTIONAL ,@arg-names)
		  ,@(map (lambda (arg-name arg-init)
			   `(IF ,(if (not arg-init)
				     `(UNASSIGNED? ,arg-name)
				     `(OR (UNASSIGNED? ,arg-name)
					  (NOT ,arg-name)))
				(SET! ,arg-name ,arg-init)))
			 arg-names arg-inits)
		  ,@body)
		(MAKE-COMMAND ,name ,description ,procedure-name))))))

(syntax-table-define edwin-syntax-table 'DEFINE-VARIABLE
  (macro (name description #!optional value)
    (let ((variable-name (string->symbol (canonicalize-name-string name))))
      `(BEGIN (DEFINE ,variable-name
		,@(if (unassigned? value)
		      '()
		      `(,value)))
	      (MAKE-VARIABLE ',name ',description ',variable-name)))))

(define (make-conditional-definition name value)
  (make-definition name
    (make-conditional (make-unbound? name)
		      value
		      (make-conditional (make-unassigned? name)
					(make-unassigned-object)
					(make-variable name)))))

(syntax-table-define edwin-syntax-table 'REF-VARIABLE
  (macro (name)
    (string->symbol (canonicalize-name-string name))))

(syntax-table-define edwin-syntax-table 'SET-VARIABLE!
  (macro (name #!optional value)
    `(SET! ,(string->symbol (canonicalize-name-string name))
	   ,@(if (unassigned? value) '() `(,value)))))

(syntax-table-define edwin-syntax-table 'GLOBAL-SET-VARIABLE!
  (macro (name #!optional value)
    (let ((variable-name (string->symbol (canonicalize-name-string name))))
      `(BEGIN (UNMAKE-LOCAL-BINDING! ',variable-name)
	      (SET! ,variable-name
		    ,@(if (unassigned? value) '() `(,value)))))))

(syntax-table-define edwin-syntax-table 'LOCAL-SET-VARIABLE!
  (macro (name #!optional value)
    `(MAKE-LOCAL-BINDING! ',(string->symbol (canonicalize-name-string name))
			  ,@(if (unassigned? value)
				'()
				`(,value)))))

(syntax-table-define edwin-syntax-table 'DEFINE-MAJOR-MODE
  (macro (name super-mode-name description . initialization)
    (let ((vname
	   (string->symbol
	    (string-append (canonicalize-name-string name)
			   "-MODE"))))
      `(DEFINE ,vname
	 (MAKE-MODE ,name TRUE
		    ,(if super-mode-name
			 `(MODE-COMTABS (NAME->MODE ,super-mode-name))
			 ''())
		    ,description
		    (LAMBDA () ,@initialization))))))

(syntax-table-define edwin-syntax-table 'DEFINE-MINOR-MODE
  (macro (name description . initialization)
    (let ((vname
	   (string->symbol
	    (string-append (canonicalize-name-string name)
			   "-MODE"))))
      `(DEFINE ,vname
	 (MAKE-MODE ,name false '()
		    ,description
		    (LAMBDA () ,@initialization))))))

(define (canonicalize-name-string name)
  (let ((name (string-upcase name)))
    (string-replace! name #\Space #\-)
    name))

;;; end EDWIN-MACROS package.
))

;;; Edwin Variables:
;;; Scheme Environment: edwin-macros
;;; End:
