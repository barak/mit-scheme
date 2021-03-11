#| -*-Scheme-*-

$Vax-Header: insmac.scm,v 1.12 89/05/17 20:29:15 GMT jinx Exp $

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

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
				  (tail (cdddr actions)))
			      (declare (integrate keyword value))
			      `(MAKE-EFFECTIVE-ADDRESS
				',keyword
				',categories
 				,(integer-syntaxer mode 'UNSIGNED 2)
				,(integer-syntaxer register 'UNSIGNED 3)
				,(process-tail tail false))))))))

(define (process-tail tail early?)
  (if (null? tail)
      `()
      (process-fields tail early?)))

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

;; *** We can't really handle switching these right now. ***

(define-integrable *ADDRESS-SIZE* 32)
(define-integrable *OPERAND-SIZE* 32)

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
		    (receiver
		     `(CONS-SYNTAX
		       (EA/REGISTER ,r/m)
		       (CONS-SYNTAX
			,(integer-syntaxer digit-or-reg 'UNSIGNED 3)
			(CONS-SYNTAX
			 (EA/MODE ,r/m)
			 (APPEND-SYNTAX! (EA/EXTRA ,r/m)
					 ,tail))))
		     (+ 8 tail-size))))))
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
		   #|
		   (COERCE-TO-TYPE ,value
				   ,(case mode
				      ((OPERAND)
				       `*OPERAND-SIZE*)
				      ((ADDRESS)
				       `*ADDRESS-SIZE*)
				      (else
				       (error "Unknown IMMEDIATE mode" mode)))
				   ,domain)
		   |#
		   ,(integer-syntaxer
		     value
		     domain
		     (case mode
		       ((OPERAND)
			*operand-size*)
		       ((ADDRESS)
			*address-size*)
		       (else
			(error "Unknown IMMEDIATE mode" mode))))
		   ,tail)))
	     tail-size))
	   (else
	    (error "expand-fields: Unknown field kind" (caar fields))))))))