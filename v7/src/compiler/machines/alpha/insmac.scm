#| -*-Scheme-*-

$Id: insmac.scm,v 1.1 1992/08/29 13:51:23 jinx Exp $

Copyright (c) 1992 Digital Equipment Corporation (D.E.C.)

This software was developed at the Digital Equipment Corporation
Cambridge Research Laboratory.  Permission to copy this software, to
redistribute it, and to use it for any purpose is granted, subject to
the following restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to both the Digital Equipment Corporation Cambridge Research
Lab (CRL) and the MIT Scheme project any improvements or extensions
that they make, so that these may be included in future releases; and
(b) to inform CRL and MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. D.E.C. has made no warrantee or representation that the operation
of this software will be error-free, and D.E.C. is under no obligation
to provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Digital Equipment Corporation
nor of any adaptation thereof in any advertising, promotional, or
sales literature without prior written consent from D.E.C. in each
case.

|#

;;;; Alpha Instruction Set Macros
;;; Package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Definition macros

(syntax-table-define assembler-syntax-table 'DEFINE-SYMBOL-TRANSFORMER
  (macro (name . alist)
    `(BEGIN
       (DECLARE (INTEGRATE-OPERATOR ,name))
       (DEFINE (,name SYMBOL)
	 (DECLARE (INTEGRATE SYMBOL))
	 (LET ((PLACE (ASSQ SYMBOL ',alist)))
	   (IF (NULL? PLACE)
	       #F
	       (CDR PLACE)))))))

(syntax-table-define assembler-syntax-table 'DEFINE-TRANSFORMER
  (macro (name value)
    `(DEFINE ,name ,value)))

;;;; Fixed width instruction parsing

(define (parse-instruction first-word tail early?)
  (if (not (null? tail))
      (error "parse-instruction: Unknown format" (cons first-word tail)))
  (let loop ((first-word first-word))
    (case (car first-word)
      ((LONG)
       (process-fields (cdr first-word) early?))
      ((VARIABLE-WIDTH)
       (process-variable-width first-word early?))
      ((IF)
       `(IF ,(cadr first-word)
	    ,(loop (caddr first-word))
	    ,(loop (cadddr first-word))))
      (else
       (error "parse-instruction: Unknown format" first-word)))))

(define (process-variable-width descriptor early?)
  (let ((binding (cadr descriptor))
	(clauses (cddr descriptor)))
    `(LIST
      ,(variable-width-expression-syntaxer
	(car binding)			; name
	(cadr binding)			; expression
	(map (lambda (clause)
	       (expand-fields
		(cdadr clause)
		early?
		(lambda (code size)
		  (if (not (zero? (remainder size 32)))
		      (error "process-variable-width: bad clause size" size))
		  `((LIST ,(optimize-group-syntax code early?))
		    ,size
		    ,@(car clause)))))
	     clauses)))))

(define (process-fields fields early?)
  (expand-fields fields
		 early?
		 (lambda (code size)
		   (if (not (zero? (remainder size 32)))
		       (error "process-fields: bad syllable size" size))
		   `(LIST ,(optimize-group-syntax code early?)))))

(define (expand-fields fields early? receiver)
  (define (expand first-word word-size fields receiver)
    (if (null? fields)
	(receiver '() 0)
	(expand-field
	 (car fields) early?
	 (lambda (car-field car-size)
	   (if (= 32 (+ word-size car-size))
	       (expand '() 0 (cdr fields)
		       (lambda (tail tail-size)
			 (receiver
			  (append (cons car-field first-word) tail)
			  (+ car-size tail-size))))
	       (expand (cons car-field first-word)
		       (+ car-size word-size)
		       (cdr fields)
		       (lambda (tail tail-size)
			 (receiver
			  (if (zero? car-size)
			      (cons car-field tail)
			      tail)
			  (+ car-size tail-size)))))))))
  (expand '() 0 fields receiver))

(define (expand-field field early? receiver)
  early?				; ignored for now
  (let ((size (car field))
	(expression (cadr field)))

    (define (default type)
      (receiver (integer-syntaxer expression type size)
		size))

    (if (null? (cddr field))
	(default 'UNSIGNED)
	(case (caddr field)
	  ((PC-REL)
	   (receiver
	    (integer-syntaxer ``(- ,,expression (+ *PC* 4))
			      (cadddr field)
			      size)
	    size))
	  ((BLOCK-OFFSET)
	   (receiver (list 'list ''BLOCK-OFFSET expression)
		     size))
	  (else
	   (default (caddr field)))))))