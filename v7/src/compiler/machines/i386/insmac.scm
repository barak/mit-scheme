#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/insmac.scm,v 1.3 1992/02/13 02:54:37 jinx Exp $
$Vax-Header: insmac.scm,v 1.12 89/05/17 20:29:15 GMT jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; Intel 386 Instruction Set Macros

(declare (usual-integrations))

;;;; Effective addressing

(define ea-database-name
  'EA-DATABASE)

(syntax-table-define assembler-syntax-table 'DEFINE-EA-DATABASE
  (macro rules
    `(DEFINE ,ea-database-name
       ,(compile-database rules
			  (lambda (pattern actions)
			    (let ((keyword (car pattern))
				  (categories (car actions))
				  (mode (cadr actions))
				  (register (caddr actions))
				  (extension (cdddr actions)))
			      (declare (integrate keyword value))
			      `(MAKE-EFFECTIVE-ADDRESS
				',keyword
				',categories
 				,(integer-syntaxer mode 'UNSIGNED 2)
				,(integer-syntaxer register 'UNSIGNED 3)
				,(process-fields value false))))))))

;; This one is necessary to distinguish between r/mW mW, etc.

(syntax-table-define assembler-syntax-table 'DEFINE-EA-TRANSFORMER
  (macro (name #!optional restriction)
    (if (default-object? restriction)
	`(define (,name expression)
	   (let ((match-result (pattern-lookup ,ea-database-name expression)))
	     (and match-result
		  (match-result))))
	`(define (,name expression)
	   (let ((match-result (pattern-lookup ,ea-database-name expression)))
	     (and match-result
		  (let ((ea (match-result)))
		    (and (memq ',restriction (ea/categories ea))
			 ea))))))))

(define (parse-instruction opcode tail early?)
  (process-fields (cons opcode tail) early?))

(define (process-fields fields early?)
  (if (and (null? (cdr fields))
	   (eq? (caar fields) 'VARIABLE-WIDTH))
      (expand-variable-width (car fields) early?)
      (expand-fields fields
		     early?
		     (lambda (code size)
		       (if (not (zero? (remainder size 8)))
			   (error "process-fields: bad syllable size" size))
		       code))))

(define (expand-variable-width field early?)
  (let ((binding (cadr field))
	(clauses (cddr field)))
    `(LIST
      ,(variable-width-expression-syntaxer
	(car binding)			; name
	(cadr binding)			; expression
	(map (lambda (clause)
	       (expand-fields
		(cdr clause)
		early?
		(lambda (code size)
		  (if (not (zero? (remainder size 8)))
		      (error "expand-variable-width: bad clause size" size))
		  `(,code ,size ,@(car clause)))))
	     clauses)))))

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

(define (expand-fields fields early? receiver)
  (if (null? fields)
      (receiver ''() 0)
      (expand-fields (cdr fields) early?
       (lambda (tail tail-size)
	 (case (caar fields)
	   ;; For opcodes and fixed fields of the instruction
	   ((BYTE)
	    ;; (BYTE (8 #xff))
	    ;; (BYTE (16 (+ foo #x23) SIGNED))
	    (collect-byte (cdar fields)
			  tail
			  (lambda (code size)
			    (receiver code (+ size tail-size)))))
	   ((ModR/M)
	    ;; (ModR/M 2 source)	= /2 r/m(source)
	    ;; (ModR/M r target)	= /r r/m(target)
	    (if early?
		(error "No early support for ModR/M -- Fix i386/insmac.scm")
		(let ((field (car fields)))
		  (let ((digit-or-reg (cadr field))
			(r/m (caddr field)))
		    (collect-byte `((2 (EA/MODE ,r/m))
				    (3 ,digit-or-reg)
				    (3 (EA/REGISTER ,r/m)))
				  `(APPEND-SYNTAX! (EA/EXTRA ,r/m) ,tail)
				  (lambda (code byte-size)
				    (receiver code
					      (+ byte-size tail-size))))))))
	   ;; For immediate operands whose size depends on the operand
	   ;; size for the instruction (halfword vs. longword)
	   ((IMMEDIATE)
	    (receiver
	     (let ((field (car fields)))
	       (let ((value (cadr field))
		     (mode (if (null? (cddr field))
			       'OPERAND
			       (caddr field)))
		     (domain (if (or (null? (cddr field))
				     (null? (cdddr field)))
				 'SIGNED
				 (cadddr field))))
		 `(CONS-SYNTAX
		   (COERCE-TO-TYPE ,value
				   ,(case mode
				      ((OPERAND)
				       `*OPERAND-SIZE*)
				      ((ADDRESS)
				       `*ADDRESS-SIZE*)
				      (else
				       (error "Unknown IMMEDIATE mode" mode)))
				   ,domain)
		   ,tail)))
	     tail-size))
	   (else
	    (error "expand-fields: Unknown field kind" (caar fields))))))))