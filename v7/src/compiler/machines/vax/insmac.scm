#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/insmac.scm,v 1.8 1987/08/22 22:44:15 jinx Exp $

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

;;;; VAX Instruction Set Macros

(declare (usual-integrations))

;;;; Effective addressing

(syntax-table-define assembler-syntax-table 'DEFINE-EA-DATABASE
  (macro rules
    `(DEFINE EA-DATABASE
       ,(compile-database
	 rules
	 (lambda (pattern actions)
	   (let ((keyword (car pattern))
		 (categories (car actions))
		 (value (cdr actions)))
	     (declare (integrate keyword categories value))
	     `(MAKE-EFFECTIVE-ADDRESS
	       ',keyword
	       ',categories
	       ,(process-fields value false))))))))

(syntax-table-define assembler-syntax-table 'DEFINE-EA-TRANSFORMER
  (macro (name category type)
    `(define (,name expression)
       (let ((ea (process-ea expression ',type)))
	 (and ea
	      (memq ',category (ea-categories ea))
	      ea)))))

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

(syntax-table-define assembler-syntax-table 'DEFINE-TRANSFORMER
  (macro (name value)
    `(define ,name ,value)))

(define (parse-instruction opcode tail early?)
  (process-fields (cons opcode tail) early?))

(define (process-fields fields early?)
  (if (and (null? (cdr fields))
	   (eq? (caar fields) 'VARIABLE-WIDTH))
      (expand-variable-width (car fields)
			     (if early? 'EA-VALUE-EARLY 'EA-VALUE))
      (expand-fields fields
		     (if early? 'EA-VALUE-EARLY 'EA-VALUE)
		     (lambda (code size)
		       (if (not (zero? (remainder size 8)))
			   (error "process-fields: bad syllable size" size))
		       code))))

(define (expand-variable-width field ea-value-operator)
  (let ((binding (cadr field))
	(clauses (cddr field)))
    `(LIST
      ,(variable-width-expression-syntaxer
	(car binding)			; name
	(cadr binding)			; expression
	(map (lambda (clause)
	       (expand-fields
		(cdr clause)
		ea-value-operator
		(lambda (code size)
		  (if (not (zero? (remainder size 8)))
		      (error "expand-variable-width: bad clause size" size))
		  `(,code ,size ,@(car clause)))))
	     clauses)))))

(define (expand-fields fields ea-value-operator receiver)
  (if (null? fields)
      (receiver ''() 0)
      (expand-fields (cdr fields) ea-value-operator
       (lambda (tail tail-size)
	 (case (caar fields)
	   ((BYTE)
	    (collect-byte (cdar fields)
			  tail
			  (lambda (code size)
			    (receiver code (+ size tail-size)))))
	   ((OPERAND)
	    (receiver `(APPEND-SYNTAX! (,ea-value-operator ,(caddar fields))
				       ,tail)
		      tail-size))
	   ((DISPLACEMENT)
	    (let ((desc (cadar fields)))
	      (let ((expression (cadr desc))
		    (size (car desc)))
		(receiver
		 `(CONS-SYNTAX
		   ,(displacement-syntaxer expression size)
		   ,tail)
		 (+ size tail-size)))))
	   ((IMMEDIATE)
	    (receiver
	     `(CONS-SYNTAX
	       (COERCE-TO-TYPE ,(cadar fields) *IMMEDIATE-TYPE*)
	       ,tail)
	     tail-size))
	   (else
	    (error "expand-fields: Unknown field kind" (caar fields))))))))

(define (displacement-syntaxer expression size)
  (cond ((not (pair? expression))
	 `(SYNTAX-DISPLACEMENT ,expression
			       ,(make-coercion-name 'SIGNED size)))
	((eq? '@PCO (car expression))
	 (integer-syntaxer (cadr expression) 'SIGNED size))
	((eq? '@PCR (car expression))
	 (nteger-syntaxer `(- ,(cadr expression)
			      (+ *PC* ,(/ size 8))) 'SIGNED size))
	(else
	 `(SYNTAX-DISPLACEMENT ,expression
			       ,(make-coercion-name 'SIGNED size)))))

(define (syntax-displacement expression coercion)
  (cond ((not (pair? expression))
	 (error "syntax-displacement: bad displacement specifier"
		expression))
	((eq? (car expression) '@PCO)
	 (syntax-evaluation (cadr expression) coercion))
	((eq? (car expression) '@PCR)
	 (syntax-evaluation `(- ,(cadr expression)
				(+ *PC* ,(/ (coercion-size coercion) 8)))
			    coercion))
	(else
	 (error "syntax-displacement: bad displacement specifier"
		expression))))

(define (collect-byte components tail receiver)
  (define (inner components receiver)
    (if (null? components)
	(receiver tail 0)
	(inner (cdr components)
	       (lambda (byte-tail byte-size)
		 (let ((size (caar components))
		       (expression (cadar components))
		       (type (if (null? (cddar components))
				 'UNSIGNED
				 (caddar components))))
		   (receiver
		    `(CONS-SYNTAX
		      ,(integer-syntaxer expression type size)
		      ,byte-tail)
		    (+ size byte-size)))))))
  (inner components receiver))
		 
     

