#| -*-Scheme-*-

$Id$

Copyright (c) 1988, 1989, 1990, 1999 Massachusetts Institute of Technology

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

;;;; Spectrum Instruction Set Macros

(declare (usual-integrations))

;;;; Definition macros

(syntax-table-define assembler-syntax-table 'DEFINE-SYMBOL-TRANSFORMER
  (macro (name . alist)
    `(begin
       (declare (integrate-operator ,name))
       (define (,name symbol)
	 (declare (integrate symbol))
	 (let ((place (assq symbol ',alist)))
	   (if (not place)
	       #F
	       (cdr place)))))))

(syntax-table-define assembler-syntax-table 'DEFINE-TRANSFORMER
  (macro (name value)
    `(define ,name ,value)))

;;;; Fixed width instruction parsing

(define (parse-instruction first-word tail early?)
  (cond ((not (null? tail))
	 (error "parse-instruction: Unknown format" (cons first-word tail)))
	((eq? (car first-word) 'LONG)
	 (process-fields (cdr first-word) early?))
	((eq? (car first-word) 'VARIABLE-WIDTH)
	 (process-variable-width first-word early?))
	(else
	 (error "parse-instruction: Unknown format" first-word))))

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
	   (if (and (eq? endianness 'LITTLE)
		    (= 32 (+ word-size car-size)))
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
			  (if (or (zero? car-size)
				  (not (eq? endianness 'LITTLE)))
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
	    (integer-syntaxer ``(- ,,expression (+ *PC* 8))
			      (cadddr field)
			      size)
	    size))
	  ((BLOCK-OFFSET)
	   (receiver (list 'list ''BLOCK-OFFSET expression)
		     size))
	  (else
	   (default (caddr field)))))))