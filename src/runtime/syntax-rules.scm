#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

;;;; Rule-based Syntactic Expanders

;;; See "Syntactic Extensions in the Programming Language Lisp", by
;;; Eugene Kohlbecker, Ph.D. dissertation, Indiana University, 1986.
;;; See also "Macros That Work", by William Clinger and Jonathan Rees
;;; (reference? POPL?).  This implementation is derived from an
;;; implementation by Kent Dybvig, and includes some ideas from
;;; another implementation by Jonathan Rees.

(declare (usual-integrations))

(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (form rename compare)
     (syntax-check '(KEYWORD (* IDENTIFIER) * ((IDENTIFIER . DATUM) EXPRESSION))
		   form)
     (let ((keywords (cadr form))
	   (clauses (cddr form)))
       (if (let loop ((keywords keywords))
	     (and (pair? keywords)
		  (or (memq (car keywords) (cdr keywords))
		      (loop (cdr keywords)))))
	   (syntax-error "Keywords list contains duplicates:" keywords)
	   (let ((r-form (rename 'FORM))
		 (r-rename (rename 'RENAME))
		 (r-compare (rename 'COMPARE)))
	     `(,(rename 'ER-MACRO-TRANSFORMER)
	       (,(rename 'LAMBDA)
		(,r-form ,r-rename ,r-compare)
		(,(rename 'DECLARE) (IGNORABLE ,r-rename ,r-compare))
		,(let loop ((clauses clauses))
		   (if (pair? clauses)
		       (let ((pattern (caar clauses)))
			 (let ((sids
				(parse-pattern rename compare keywords
					       pattern r-form)))
			   `(,(rename 'IF)
			     ,(generate-match rename compare keywords
					      r-rename r-compare
					      pattern r-form)
			     ,(generate-output rename compare r-rename
					       sids (cadar clauses))
			     ,(loop (cdr clauses)))))
		       `(,(rename 'BEGIN)
			 (,(rename 'ILL-FORMED-SYNTAX) ,r-form))))))))))))

(define (parse-pattern rename compare keywords pattern expression)
  (let loop
      ((pattern pattern)
       (expression expression)
       (sids '())
       (control #f))
    (cond ((identifier? pattern)
	   (if (memq pattern keywords)
	       sids
	       (cons (make-sid pattern expression control) sids)))
	  ((and (or (zero-or-more? pattern rename compare)
		    (at-least-one? pattern rename compare))
		(null? (cddr pattern)))
	   (let ((variable ((make-name-generator) 'CONTROL)))
	     (loop (car pattern)
		   variable
		   sids
		   (make-sid variable expression control))))
	  ((pair? pattern)
	   (loop (car pattern)
		 `(,(rename 'CAR) ,expression)
		 (loop (cdr pattern)
		       `(,(rename 'CDR) ,expression)
		       sids
		       control)
		 control))
	  (else sids))))

(define (generate-match rename compare keywords r-rename r-compare
			pattern expression)
  (letrec
      ((loop
	(lambda (pattern expression)
	  (cond ((identifier? pattern)
		 (if (memq pattern keywords)
		     (let ((temp (rename 'TEMP)))
		       `((,(rename 'LAMBDA)
			  (,temp)
			  (,(rename 'IF)
			   (,(rename 'IDENTIFIER?) ,temp)
			   (,r-compare ,temp
				       (,r-rename ,(syntax-quote pattern)))
			   #f))
			 ,expression))
		     `#t))
		((and (zero-or-more? pattern rename compare)
		      (null? (cddr pattern)))
		 (do-list (car pattern) expression))
		((and (at-least-one? pattern rename compare)
		      (null? (cddr pattern)))
		 `(,(rename 'IF) (,(rename 'NULL?) ,expression)
				 #F
				 ,(do-list (car pattern) expression)))
		((pair? pattern)
		 (let ((generate-pair
			(lambda (expression)
			  (conjunction
			   `(,(rename 'PAIR?) ,expression)
			   (conjunction
			    (loop (car pattern)
				  `(,(rename 'CAR) ,expression))
			    (loop (cdr pattern)
				  `(,(rename 'CDR) ,expression)))))))
		   (if (identifier? expression)
		       (generate-pair expression)
		       (let ((temp (rename 'TEMP)))
			 `((,(rename 'LAMBDA) (,temp) ,(generate-pair temp))
			   ,expression)))))
		((null? pattern)
		 `(,(rename 'NULL?) ,expression))
		(else
		 `(,(rename 'EQUAL?) ,expression
				     (,(rename 'QUOTE) ,pattern))))))
       (do-list
	(lambda (pattern expression)
	  (let ((r-loop (rename 'LOOP))
		(r-l (rename 'L))
		(r-lambda (rename 'LAMBDA)))
	    `(((,r-lambda
		()
		(,(rename 'DEFINE)
		 ,r-loop
		 (,r-lambda
		  (,r-l)
		  (,(rename 'IF)
		   (,(rename 'NULL?) ,r-l)
		   #T
		   ,(conjunction
		     `(,(rename 'PAIR?) ,r-l)
		     (conjunction (loop pattern `(,(rename 'CAR) ,r-l))
				  `(,r-loop (,(rename 'CDR) ,r-l)))))))
		,r-loop))
	      ,expression))))
       (conjunction
	(lambda (predicate consequent)
	  (cond ((eq? predicate #T) consequent)
		((eq? consequent #T) predicate)
		(else `(,(rename 'IF) ,predicate ,consequent #F))))))
    (loop pattern expression)))

(define (generate-output rename compare r-rename sids template)
  (let loop ((template template) (ellipses '()))
    (cond ((identifier? template)
	   (let ((sid
		  (let loop ((sids sids))
		    (and (pair? sids)
			 (if (eq? (sid-name (car sids)) template)
			     (car sids)
			     (loop (cdr sids)))))))
	     (if sid
		 (begin
		   (add-control! sid ellipses)
		   (sid-expression sid))
		 `(,r-rename ,(syntax-quote template)))))
	  ((or (zero-or-more? template rename compare)
	       (at-least-one? template rename compare))
	   (optimized-append rename compare
			     (let ((ellipsis (make-ellipsis '())))
			       (generate-ellipsis rename
						  ellipsis
						  (loop (car template)
							(cons ellipsis
							      ellipses))))
			     (loop (cddr template) ellipses)))
	  ((pair? template)
	   (optimized-cons rename compare
			   (loop (car template) ellipses)
			   (loop (cdr template) ellipses)))
	  (else
	   `(,(rename 'QUOTE) ,template)))))

(define (add-control! sid ellipses)
  (let loop ((sid sid) (ellipses ellipses))
    (let ((control (sid-control sid)))
      (cond (control
	     (if (pair? ellipses)
		 (let ((sids (ellipsis-sids (car ellipses))))
		   (cond ((not (memq control sids))
			  (set-ellipsis-sids! (car ellipses)
					      (cons control sids)))
			 ((not (eq? control (car sids)))
			  (error "illegal control/ellipsis combination"
				 control sids))))
		 (syntax-error "Missing ellipsis in expansion." #f))
	     (loop control (cdr ellipses)))))))

(define (generate-ellipsis rename ellipsis body)
  (let ((sids (ellipsis-sids ellipsis)))
    (if (pair? sids)
	(let ((name (sid-name (car sids)))
	      (expression (sid-expression (car sids))))
	  (cond ((and (null? (cdr sids))
		      (eq? body name))
		 expression)
		((and (null? (cdr sids))
		      (pair? body)
		      (pair? (cdr body))
		      (eq? (cadr body) name)
		      (null? (cddr body)))
		 `(,(rename 'MAP) ,(car body) ,expression))
		(else
		 `(,(rename 'MAP) (,(rename 'LAMBDA) ,(map sid-name sids)
						     ,body)
				  ,@(map sid-expression sids)))))
	(syntax-error "Missing ellipsis in expansion." #f))))

(define (zero-or-more? pattern rename compare)
  (and (pair? pattern)
       (pair? (cdr pattern))
       (identifier? (cadr pattern))
       (compare (cadr pattern) (rename '...))))

(define (at-least-one? pattern rename compare)
;;;  (and (pair? pattern)
;;;       (pair? (cdr pattern))
;;;       (identifier? (cadr pattern))
;;;       (compare (cadr pattern) (rename '+)))
  pattern rename compare		;ignore
  #f)

(define (syntax-quote expression)
  `(,(compiler->keyword
      (lambda (form environment)
	environment			;ignore
	(syntax-check '(KEYWORD DATUM) form)
	(output/constant (cadr form))))
    ,expression))

(define (optimized-cons rename compare a d)
  (cond ((and (pair? d)
	      (compare (car d) (rename 'QUOTE))
	      (pair? (cdr d))
	      (null? (cadr d))
	      (null? (cddr d)))
	 `(,(rename 'LIST) ,a))
	((and (pair? d)
	      (compare (car d) (rename 'LIST))
	      (list? (cdr d)))
	 `(,(car d) ,a ,@(cdr d)))
	(else
	 `(,(rename 'CONS) ,a ,d))))

(define (optimized-append rename compare x y)
  (if (and (pair? y)
	   (compare (car y) (rename 'QUOTE))
	   (pair? (cdr y))
	   (null? (cadr y))
	   (null? (cddr y)))
      x
      `(,(rename 'APPEND) ,x ,y)))

(define-record-type <sid>
    (make-sid name expression control)
    sid?
  (name sid-name)
  (expression sid-expression)
  (control sid-control)
  (output-expression sid-output-expression set-sid-output-expression!))

(define-record-type <ellipsis>
    (make-ellipsis sids)
    ellipsis?
  (sids ellipsis-sids set-ellipsis-sids!))