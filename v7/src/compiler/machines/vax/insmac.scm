#| -*-Scheme-*-

$Id: insmac.scm,v 1.14 2001/12/19 21:39:30 cph Exp $

Copyright (c) 1987, 1989, 1999, 2001 Massachusetts Institute of Technology

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

;;;; VAX Instruction Set Macros

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
	   (let ((keyword (car pattern))
		 (categories (car actions))
		 (value (cdr actions)))
	     (declare (integrate keyword categories value))
	     `(MAKE-EFFECTIVE-ADDRESS
	       ',keyword
	       ',categories
	       ,(process-fields value false))))))))

(syntax-table/define (->environment '(COMPILER LAP-SYNTAXER))
		     'DEFINE-EA-TRANSFORMER
  (lambda (name category type)
    `(define (,name expression)
       (let ((ea (process-ea expression ',type)))
	 (and ea
	      (memq ',category (ea-categories ea))
	      ea)))))

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
		     'DEFINE-TRANSFORMER
  (lambda (name value)
    `(define ,name ,value)))

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

(define (expand-fields fields early? receiver)
  (if (null? fields)
      (receiver ''() 0)
      (expand-fields (cdr fields) early?
       (lambda (tail tail-size)
	 (case (caar fields)
	   ((BYTE)
	    (collect-byte (cdar fields)
			  tail
			  (lambda (code size)
			    (receiver code (+ size tail-size)))))
	   ((OPERAND)
	    (receiver
	     `(APPEND-SYNTAX!
	       ,(if early?
		    `(EA-VALUE-EARLY ',(cadar fields) ,(caddar fields))
		    `(EA-VALUE ,(caddar fields)))
	       ,tail)
	     tail-size))
	   ;; Displacements are like signed bytes.  They are a different
	   ;; keyword to allow the disassembler to do its thing correctly.
	   ((DISPLACEMENT)
	    (let* ((desc (cadar fields))
		   (size (car desc)))
	      (receiver
	       `(CONS-SYNTAX ,(integer-syntaxer (cadr desc) 'SIGNED size)
			     ,tail)
	       (+ size tail-size))))
	   ((IMMEDIATE)
	    (receiver
	     `(CONS-SYNTAX
	       (COERCE-TO-TYPE ,(cadar fields)
			       *IMMEDIATE-TYPE*
			       ,(and (cddar fields)
				     (eq? (caddar fields)
					 'UNSIGNED)))
	       ,tail)
	     tail-size))
	   (else
	    (error "expand-fields: Unknown field kind" (caar fields))))))))

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
		 
     

