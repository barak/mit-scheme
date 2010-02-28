#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; SCode Interface

(declare (usual-integrations))

(define (scode/make-constant value) value)
(define (scode/constant-value constant) constant)

(define (scode/quotation-components quot recvr)
  (recvr (scode/quotation-expression quot)))

(define comment-tag:directive
  (intern "#[(compiler)comment-tag:directive]"))

(define (scode/make-directive code directive original-code)
  (scode/make-comment
   (list comment-tag:directive
	 directive
	 (scode/original-expression original-code))
   code))

(define (scode/original-expression scode)
  (if (and (scode/comment? scode)
	   (scode/comment-directive? (scode/comment-text scode)))
      (caddr (scode/comment-text scode))
      scode))

(define (scode/comment-directive? text . kinds)
  (and (pair? text)
       (eq? (car text) comment-tag:directive)
       (or (null? kinds)
	   (memq (caadr text) kinds))))

(define (scode/make-let names values . body)
  (scan-defines (scode/make-sequence body)
    (lambda (auxiliary declarations body)
      (scode/make-combination
       (scode/make-lambda lambda-tag:let names '() false
			  auxiliary declarations body)
       values))))

;;;; Absolute variables and combinations

(define (scode/make-absolute-reference variable-name)
  (scode/make-access system-global-environment variable-name))

(define (scode/absolute-reference? object)
  (and (scode/access? object)
       (eq? (scode/access-environment object) system-global-environment)))

(define (scode/absolute-reference-name reference)
  (scode/access-name reference))

(define (scode/make-absolute-combination name operands)
  (scode/make-combination (scode/make-absolute-reference name) operands))

(define (scode/absolute-combination? object)
  (and (scode/combination? object)
       (scode/absolute-reference? (scode/combination-operator object))))

(define (scode/absolute-combination-name combination)
  (scode/absolute-reference-name (scode/combination-operator combination)))

(define (scode/absolute-combination-operands combination)
  (scode/combination-operands combination))

(define (scode/absolute-combination-components combination receiver)
  (receiver (scode/absolute-combination-name combination)
	    (scode/absolute-combination-operands combination)))

(define (scode/error-combination? object)
  (or (and (scode/combination? object)
	   (eq? (scode/combination-operator object)
		(ucode-primitive error-procedure)))
      (and (scode/absolute-combination? object)
	   (eq? (scode/absolute-combination-name object) 'ERROR-PROCEDURE))))

(define (scode/error-combination-components combination receiver)
  (scode/combination-components combination
    (lambda (operator operands)
      operator
      (receiver
       (car operands)
       (let loop ((irritants (cadr operands)))
	 (cond ((null? irritants) '())
	       ((and (scode/absolute-combination? irritants)
		     (eq? (scode/absolute-combination-name irritants) 'LIST))
		(scode/absolute-combination-operands irritants))
	       ((and (scode/combination? irritants)
		     (eq? (scode/combination-operator irritants)
			  (ucode-primitive cons)))
		(let ((operands (scode/combination-operands irritants)))
		  (cons (car operands)
			(loop (cadr operands)))))
	       (else
		(cadr operands))))))))

(define (scode/make-error-combination message operand)
  (scode/make-absolute-combination
   'ERROR-PROCEDURE
   (list message
	 (scode/make-combination (ucode-primitive cons)
				 (list operand '()))
	 (scode/make-the-environment))))