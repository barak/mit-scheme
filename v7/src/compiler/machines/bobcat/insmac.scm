#| -*-Scheme-*-

$Id: insmac.scm,v 1.128 2001/12/19 21:39:30 cph Exp $

Copyright (c) 1988, 1990, 1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; 68000 Instruction Set Macros

(declare (usual-integrations))

;;;; Effective addressing

(define ea-database-name
  'EA-DATABASE)

(syntax-table/define (->environment '(COMPILER LAP-SYNTAXER))
		     'DEFINE-EA-DATABASE
  (lambda rules
    `(DEFINE ,ea-database-name
       ,(compile-database rules
	 (lambda (pattern actions)
	   (if (null? (cddr actions))
	       (make-position-dependent pattern actions)
	       (make-position-independent pattern actions)))))))

(syntax-table/define (->environment '(COMPILER LAP-SYNTAXER))
		     'EXTENSION-WORD
  (lambda descriptors
    (expand-descriptors descriptors
      (lambda (instruction size source destination)
	(if (or source destination)
	    (error "Source or destination used" 'EXTENSION-WORD)
	    (if (zero? (remainder size 16))
		(optimize-group-syntax instruction false)
		(error "EXTENSION-WORD: Extensions must be 16 bit multiples"
		       size)))))))

(syntax-table/define (->environment '(COMPILER LAP-SYNTAXER))
		     'VARIABLE-EXTENSION
  (lambda (binding . clauses)
    (variable-width-expression-syntaxer
     (car binding)
     (cadr binding)
     (map (lambda (clause)
	    `((LIST ,(caddr clause))
	      ,(cadr clause)
	      ,@(car clause)))
	  clauses))))

(define (make-position-independent pattern actions)
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
	IMMEDIATE-SIZE			;ignore if not referenced
	,(if (null? extension)
	     'INSTRUCTION-TAIL
	     `(CONS-SYNTAX ,(car extension) INSTRUCTION-TAIL)))
      ',categories)))

(define (process-ea-field field)
  (if (exact-integer? field)
      (integer-syntaxer field 'UNSIGNED 3)
      (let ((binding (cadr field))
	    (clauses (cddr field)))
	(variable-width-expression-syntaxer
	 (car binding)
	 (cadr binding)
	 (map (lambda (clause)
		`((LIST ,(integer-syntaxer (cadr clause) 'UNSIGNED 3))
		  3
		  ,@(car clause)))
	      clauses)))))

(define (make-position-dependent pattern actions)
  (let ((keyword (car pattern))
	(categories (car actions))
	(code (cdr (cadr actions))))
    (let ((name (car code))
	  (mode (cadr code))
	  (register (caddr code))
	  (extension (cadddr code)))
      `(LET ((,name (GENERATE-LABEL 'MARK)))
	 (make-effective-address
	  ',keyword
	  ,(process-ea-field mode)
	  ,(process-ea-field register)
	  (LAMBDA (IMMEDIATE-SIZE INSTRUCTION-TAIL)
	    IMMEDIATE-SIZE		;ignore if not referenced
	    ,(if (null? extension)
		 'INSTRUCTION-TAIL
		 `(CONS (LIST 'LABEL ,name)
			(CONS-SYNTAX ,extension INSTRUCTION-TAIL))))
	  ',categories)))))

;;;; Transformers

(syntax-table/define (->environment '(COMPILER LAP-SYNTAXER))
		     'DEFINE-EA-TRANSFORMER
  (lambda (name #!optional categories keywords)
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
	      ,(if (default-object? categories)
		    `(match-result)
		    `(let ((ea (match-result)))
		       (and ,@(filter categories
				      (lambda (cat exp) `(memq ',cat ,exp))
				      `(ea-categories ea))
			    ,@(if (default-object? keywords)
				  `()
				  (filter keywords
					  (lambda (key exp)
					    `(not (eq? ',key ,exp)))
					  `(ea-keyword ea)))
			    ea))))))))

(syntax-table/define (->environment '(COMPILER LAP-SYNTAXER))
		     'DEFINE-SYMBOL-TRANSFORMER
  (lambda (name . alist)
    `(begin
       (declare (integrate-operator ,name))
       (define (,name symbol)
	 (declare (integrate symbol))
	 (let ((place (assq symbol ',alist)))
	   (if (null? place)
	       #F
	       (cdr place)))))))

(syntax-table/define (->environment '(COMPILER LAP-SYNTAXER))
		     'DEFINE-REG-LIST-TRANSFORMER
  (lambda (name . alist)
    `(begin
       (declare (integrate-operator ,name))
       (define (,name reg-list)
	 (declare (integrate reg-list))
	 (encode-register-list reg-list ',alist)))))

;;;; Utility procedures

(define (parse-instruction expression tail early?)
  (define (kernel)
    (case (car expression)
      ((WORD)
       (parse-word expression tail))
      ((GROWING-WORD)
       (parse-growing-word expression tail))
      (else
       (error "PARSE-INSTRUCTION: unknown expression" expression))))
    
  (if (not early?)
      (with-normal-selectors kernel)
      (with-early-selectors kernel)))

;;; Variable width instruction parsing

(define (parse-growing-word expression tail)
  (if (not (null? tail))
      (error "PARSE-GROWING-WORD: non null tail" tail))
  (let ((binding (cadr expression)))
    `(LIST
      ,(variable-width-expression-syntaxer
	(car binding)
	(cadr binding)
	(map (lambda (clause)
	       (if (not (null? (cddr clause)))
		   (error "Extension found in clause" clause))
	       (expand-descriptors
		(cdadr clause)
		(lambda (instruction size src dst)
		  (if (not (zero? (remainder size 16)))
		      (error "Instructions must be 16 bit multiples" size))
		  `(,(collect-word instruction src dst '())
		    ,size
		    ,@(car clause)))))	; Range
	     (cddr expression))))))

;;;; Fixed width instruction parsing

(define (parse-word expression tail)
  (expand-descriptors (cdr expression)
   (lambda (instruction size src dst)
     (if (zero? (remainder size 16))
	 (collect-word instruction src dst tail)
	 (error "PARSE-WORD: Instructions must be 16 bit multiples" size)))))

(define (collect-word instruction src dst tail)
  (let ((code
	 (let ((code
		(let ((code (if dst `(,@dst '()) '())))
		  (if src
		      `(,@src ,code)
		      code))))
	   (cond ((null? tail) code)
		 ((null? (cdr tail))
		  `(,(if (null? code) 'CONS 'CONS-SYNTAX)
		    ,(car tail)
		    ,code))
		 (else
		  (error "PARSE-WORD: multiple tail elements" tail))))))
    (if (not (null? instruction))
	`(,(if (null? code) 'CONS 'CONS-SYNTAX)
	  ,(optimize-group-syntax instruction early-instruction-parsing?)
	  ,code)
	code)))	

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

;;;; Hooks for early instruction processing

(define early-instruction-parsing? false)
(define ea-keyword-selector 'EA-KEYWORD)
(define ea-categories-selector 'EA-CATEGORIES)
(define ea-mode-selector 'EA-MODE)
(define ea-register-selector 'EA-REGISTER)
(define ea-extension-selector 'EA-EXTENSION)

(define (with-normal-selectors handle)
  (fluid-let ((early-instruction-parsing? false)
	      (ea-keyword-selector 'EA-KEYWORD)
	      (ea-categories-selector 'EA-CATEGORIES)
	      (ea-mode-selector 'EA-MODE)
	      (ea-register-selector 'EA-REGISTER)
	      (ea-extension-selector 'EA-EXTENSION))
    (handle)))

(define (with-early-selectors handle)
  (fluid-let ((early-instruction-parsing? true)
	      (ea-keyword-selector 'EA-KEYWORD-EARLY)
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
      ((UNSIGNED SIGNED SHIFT-NUMBER QUICK BFWIDTH SCALE-FACTOR)
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