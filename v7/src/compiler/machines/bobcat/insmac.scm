#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/insmac.scm,v 1.119 1987/07/08 22:05:47 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; 68000 Instruction Set Macros

(declare (usual-integrations))

;;;; Effective addressing

(define ea-database-name 'ea-database)

(syntax-table-define assembler-syntax-table 'DEFINE-EA-DATABASE
  (macro rules
    `(define ,ea-database-name
       ,(compile-database rules
	 (lambda (pattern actions)
	   (let ((keyword (car pattern))
		 (categories (car actions))
		 (mode (cadr actions))
		 (register (caddr actions))
		 (extension (cdddr actions)))
	     ;;(declare (integrate keyword categories mode register extension))
	     `(MAKE-EFFECTIVE-ADDRESS
	       ',keyword
	       ,(integer-syntaxer mode 'UNSIGNED 3)
	       ,(integer-syntaxer register 'UNSIGNED 3)
	       (LAMBDA (IMMEDIATE-SIZE INSTRUCTION-TAIL)
		 ,(if (null? extension)
		      'INSTRUCTION-TAIL
		      `(CONS-SYNTAX ,(car extension) INSTRUCTION-TAIL)))
	       ',categories)))))))

(syntax-table-define assembler-syntax-table 'EXTENSION-WORD
  (macro descriptors
    (expand-descriptors descriptors
      (lambda (instruction size source destination)
	(if (or source destination)
	    (error "Source or destination used" 'EXTENSION-WORD)
	    (if (zero? (remainder size 16))
		(optimize-group-syntax instruction false)
		(error "EXTENSION-WORD: Extensions must be 16 bit multiples"
		       size)))))))

;;;; Transformers

(syntax-table-define assembler-syntax-table 'DEFINE-EA-TRANSFORMER
  (macro (name #!optional categories keywords)
    (define (filter special generator extraction)
      (define (multiple rem)
	(if (null? rem)
	    `()
	    `(,(generator (car rem) 'temp)
	      ,@(multiple (cdr rem)))))

      (cond ((null? special)
	     `())
	    ((null? (cdr special))
	     `(,(generator (car special) extraction)))
	    (else
	     `((let ((temp ,extraction))
		 (and ,@(multiple special)))))))

    `(define (,name expression)
       (let ((match-result (pattern-lookup ,ea-database-name expression)))
	 (and match-result
	      ,(if (unassigned? categories)
		    `(match-result)
		    `(let ((ea (match-result)))
		       (and ,@(filter categories
				      (lambda (cat exp) `(memq ',cat ,exp))
				      `(ea-categories ea))
			    ,@(if (unassigned? keywords)
				  `()
				  (filter keywords
					  (lambda (key exp) `(not (eq? ',key ,exp)))
					  `(ea-keyword ea)))
			    ea))))))))

(syntax-table-define assembler-syntax-table 'DEFINE-SYMBOL-TRANSFORMER
  (macro (name . alist)
    `(begin
       (declare (integrate-operator ,name))
       (define (,name symbol)
	 (declare (integrate symbol))
	 (let ((place (assq symbol ',alist)))
	   (if (null? place)
	       #F
	       (cdr place)))))))

(syntax-table-define assembler-syntax-table 'DEFINE-REG-LIST-TRANSFORMER
  (macro (name . alist)
    `(begin
       (declare (integrate-operator ,name))
       (define (,name reg-list)
	 (declare (integrate reg-list))
	 (encode-register-list reg-list ',alist)))))

;;;; Utility procedures

(define (parse-word expression tail #!optional early?)
  (define (kernel)
    (expand-descriptors (cdr expression)
     (lambda (instruction size src dst)
       (if (zero? (remainder size 16))
	   (let ((code
		  (let ((code
			 (let ((code (if dst `(,@dst '()) '())))
			   (if src
			       `(,@src ,code)
			       code))))
		    (if (null? tail)
			code
			`(,(if (null? code) 'CONS 'CONS-SYNTAX)
			  ,(car tail)
			  ,code)))))
	     `(,(if (null? code) 'CONS 'CONS-SYNTAX)
	       ,(optimize-group-syntax instruction
				       (if (unassigned? early?) false early?))
	       ,code))
	   (error "PARSE-WORD: Instructions must be 16 bit multiples" size)))))
  (if (or (unassigned? early?) (not early?))
      (kernel)
      (with-early-selectors kernel)))     

(define (expand-descriptors descriptors receiver)
  (if (null? descriptors)
      (receiver '() 0 false false)
      (expand-descriptors (cdr descriptors)
	(lambda (instruction* size* source* destination*)
	  (expand-descriptor (car descriptors)
	    (lambda (instruction size source destination)
	      (receiver (append! instruction instruction*)
			(+ size size*)
			(if source
			    (if source*
				(error "Multiple source definitions"
				       'EXPAND-DESCRIPTORS)
				source)
			    source*)
			(if destination
			    (if destination*
				(error "Multiple destination definitions"
				       'EXPAND-DESCRIPTORS)
				destination)
			    destination*))))))))

(define ea-keyword-selector 'EA-KEYWORD)
(define ea-categories-selector 'EA-CATEGORIES)
(define ea-mode-selector 'EA-MODE)
(define ea-register-selector 'EA-REGISTER)
(define ea-extension-selector 'EA-EXTENSION)

(define (with-early-selectors handle)
  (fluid-let ((ea-keyword-selector 'EA-KEYWORD-EARLY)
	      (ea-categories-selector 'EA-CATEGORIES-EARLY)
	      (ea-mode-selector 'EA-MODE-EARLY)
	      (ea-register-selector 'EA-REGISTER-EARLY)
	      (ea-extension-selector 'EA-EXTENSION-EARLY))
    (handle)))

(define (expand-descriptor descriptor receiver)
  (let ((size (car descriptor))
	(expression (cadr descriptor))
	(coercion-type
	 (if (null? (cddr descriptor)) 'UNSIGNED (caddr descriptor))))
    (case coercion-type
      ((UNSIGNED SIGNED SHIFT-NUMBER QUICK)
       (receiver `(,(integer-syntaxer expression coercion-type size))
		 size false false))
      ((SHORT-LABEL)
       (receiver `(,(integer-syntaxer
		     ``(- ,,expression (+ *PC* 2))
		     'SHORT-LABEL
		     size))
		 size false false))
      ((SOURCE-EA)
       (receiver `((,ea-mode-selector ,expression)
		   (,ea-register-selector ,expression))
		 size
		 `((,ea-extension-selector ,expression) ,(cadddr descriptor))
		 false))
      ((DESTINATION-EA)
       (receiver `((,ea-mode-selector ,expression)
		   (,ea-register-selector ,expression))
		 size
		 false
		 `((,ea-extension-selector ,expression) '())))
      ((DESTINATION-EA-REVERSED)
       (receiver `((,ea-register-selector ,expression)
		   (,ea-mode-selector ,expression))
		 size
		 false
		 `((,ea-extension-selector ,expression) '())))
      (else
       (error "EXPAND-DESCRIPTOR: Badly-formed descriptor" descriptor)))))