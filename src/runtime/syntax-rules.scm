#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

     (define (process ellipsis keywords clauses)
       (if (any-duplicates? keywords eq?)
	   (syntax-error "Keywords list contains duplicates:" keywords))
       (let ((ellipsis
	      (if (memq ellipsis keywords)
		  #f
		  ellipsis))
	     (r-form (new-identifier 'form))
	     (rr-rename (new-identifier 'rename))
	     (r-rename (new-identifier 'rename))
	     (r-compare (new-identifier 'compare)))
	 `(,(rename 'er-macro-transformer)
	   (,(rename 'lambda)
	    (,r-form ,rr-rename ,r-compare)
	    (,(rename 'let)
	     ((,r-rename
	       (,(rename 'lambda) (id)
		(,rr-rename (,(rename 'identifier->symbol) id)))))
	     (,(rename 'declare) (ignorable ,r-rename ,r-compare))
	     ,(let loop ((clauses clauses))
		(if (pair? clauses)
		    (let ((pattern (caar clauses)))
		      (let ((sids
			     (parse-pattern rename compare ellipsis keywords
					    pattern r-form)))
			`(,(rename 'if)
			  ,(generate-match rename compare ellipsis keywords
					   r-rename r-compare
					   pattern r-form)
			  ,(generate-output rename compare ellipsis
					    r-rename sids (cadar clauses))
			  ,(loop (cdr clauses)))))
		    `(,(rename 'ill-formed-syntax) ,r-form))))))))

     (cond ((syntax-match? '((* identifier)
			     * ((identifier . datum) expression))
			   (cdr form))
	    (process '... (cadr form) (cddr form)))
	   ((syntax-match? '(identifier
			     (* identifier)
			     * ((identifier . datum) expression))
			   (cdr form))
	    (process (cadr form) (caddr form) (cdddr form)))
	   (else
	    (ill-formed-syntax form))))))

(define (parse-pattern rename compare ellipsis keywords pattern expression)
  (let loop
      ((pattern pattern)
       (expression expression)
       (sids '())
       (control #f)
       (ellipsis ellipsis))
    (cond ((identifier? pattern)
	   (if (and ellipsis (compare pattern (rename ellipsis)))
	       (syntax-error "Misplaced ellipsis:" pattern))
	   (if (memq pattern keywords)
	       sids
	       (cons (make-sid pattern expression control) sids)))
	  ((ellipsis-quote? rename compare ellipsis pattern)
	   (loop (cadr pattern) expression sids control #f))
	  ((zero-or-more? rename compare ellipsis pattern)
	   (if (not (null? (cddr pattern)))
	       (syntax-error "Misplaced ellipsis:" pattern))
	   (let ((variable (new-identifier 'control)))
	     (loop (car pattern)
		   variable
		   sids
		   (make-sid variable expression control)
		   ellipsis)))
	  ((pair? pattern)
	   (loop (car pattern)
		 `(,(rename 'car) ,expression)
		 (loop (cdr pattern)
		       `(,(rename 'cdr) ,expression)
		       sids
		       control
		       ellipsis)
		 control
		 ellipsis))
	  (else sids))))

(define-record-type <sid>
    (make-sid name expression control)
    sid?
  (name sid-name)
  (expression sid-expression)
  (control sid-control))

(define (generate-match rename compare ellipsis keywords r-rename r-compare
			pattern expression)
  (letrec
      ((loop
	(lambda (pattern expression ellipsis)
	  (cond ((identifier? pattern)
		 (if (memq pattern keywords)
		     (let-ify rename expression
		       (lambda (expr)
			 `(,(rename 'and)
			   (,(rename 'identifier?) ,expr)
			   (,r-compare ,expr
				       (,r-rename ,(syntax-quote pattern))))))
		     `#t))
		((ellipsis-quote? rename compare ellipsis pattern)
		 (loop (cadr pattern) expression #f))
		((zero-or-more? rename compare ellipsis pattern)
		 ;; (cddr pattern) guaranteed null by parser above.
		 (do-list (car pattern) expression ellipsis))
		((pair? pattern)
		 (let-ify rename expression
		   (lambda (expr)
		     `(,(rename 'and)
		       (,(rename 'pair?) ,expr)
		       ,(loop (car pattern)
			      `(,(rename 'car) ,expr)
			      ellipsis)
		       ,(loop (cdr pattern)
			      `(,(rename 'cdr) ,expr)
			      ellipsis)))))
		((null? pattern)
		 `(,(rename 'null?) ,expression))
		(else
		 `(,(rename 'equal?) ,expression
				     (,(rename 'quote) ,pattern))))))
       (do-list
	(lambda (pattern expression ellipsis)
	  (let ((r-loop (new-identifier 'loop))
		(r-l (new-identifier 'l)))
	    `((,(rename 'let)
	       ()
	       (,(rename 'define)
		(,r-loop ,r-l)
		(,(rename 'if)
		 (,(rename 'null?) ,r-l)
		 #t
		 (,(rename 'and)
		  (,(rename 'pair?) ,r-l)
		  ,(loop pattern `(,(rename 'car) ,r-l) ellipsis)
		  (,r-loop (,(rename 'cdr) ,r-l)))))
	       ,r-loop)
	      ,expression)))))
    (loop pattern expression ellipsis)))

(define (let-ify rename expression generate-body)
  (if (identifier? expression)
      (generate-body expression)
      (let ((temp (new-identifier 'temp)))
	`(,(rename 'let) ((,temp ,expression)) ,(generate-body temp)))))

(define (generate-output rename compare ellipsis r-rename sids template)
  (let loop ((template template) (ellipses '()) (ellipsis* ellipsis))
    (cond ((identifier? template)
	   (let ((sid
		  (find (lambda (sid)
			  (eq? (sid-name sid) template))
			sids)))
	     (if sid
		 (begin
		   (add-control! sid ellipses)
		   (sid-expression sid))
		 `(,r-rename ,(syntax-quote template)))))
	  ((ellipsis-quote? rename compare ellipsis* template)
	   (loop (cadr template) ellipses #f))
	  ((zero-or-more? rename compare ellipsis* template)
	   (optimized-append rename compare
			     (let ((ellipsis (make-ellipsis '())))
			       (generate-ellipsis rename
						  ellipsis
						  (loop (car template)
							(cons ellipsis ellipses)
							ellipsis*)))
			     (loop (cddr template) ellipses ellipsis*)))
	  ((pair? template)
	   (optimized-cons rename compare
			   (loop (car template) ellipses ellipsis*)
			   (loop (cdr template) ellipses ellipsis*)))
	  (else
	   `(,(rename 'quote) ,template)))))

(define (add-control! sid ellipses)
  (let ((control (sid-control sid)))
    (if control
	(begin
	  (if (not (pair? ellipses))
	      (syntax-error "Missing ellipsis in expansion."))
	  (let ((sids (ellipsis-sids (car ellipses))))
	    (if (memq control sids)
		(if (not (eq? control (car sids)))
		    (error "illegal control/ellipsis combination:"
			   control sids))
		(set-ellipsis-sids! (car ellipses) (cons control sids))))
	  (add-control! control (cdr ellipses))))))

(define (generate-ellipsis rename ellipsis body)
  ;; Generation of body will have filled in the sids:
  (let ((sids (ellipsis-sids ellipsis)))
    (if (not (pair? sids))
	(syntax-error "Missing ellipsis in expansion."))
    ;; Optimize trivial case:
    (if (and (eq? body (sid-name (car sids)))
	     (null? (cdr sids)))
	(sid-expression (car sids))
	`(,(rename 'map) (,(rename 'lambda) ,(map sid-name sids) ,body)
			 ,@(map sid-expression sids)))))

(define-record-type <ellipsis>
    (make-ellipsis sids)
    ellipsis?
  (sids ellipsis-sids set-ellipsis-sids!))

(define (optimized-append rename compare x y)
  (cond ((constant-null? rename compare x) y)
	((constant-null? rename compare y) x)
	(else `(,(rename 'append) ,x ,y))))

(define (optimized-cons rename compare a d)
  (cond ((and (constant? rename compare a)
	      (constant? rename compare d))
	 `(,(rename 'quote)
	   ,(cons (constant->datum rename compare a)
		  (constant->datum rename compare d))))
	((constant-null? rename compare d)
	 `(,(rename 'list) ,a))
	((and (pair? d)
	      (compare (car d) (rename 'list))
	      (list? (cdr d)))
	 `(,(rename 'list) ,a ,@(cdr d)))
	(else
	 `(,(rename 'cons) ,a ,d))))

(define (ellipsis-quote? rename compare ellipsis pattern)
  (and ellipsis
       (pair? pattern)
       (identifier? (car pattern))
       (compare (car pattern) (rename ellipsis))
       (pair? (cdr pattern))
       (null? (cddr pattern))))

(define (zero-or-more? rename compare ellipsis pattern)
  (and ellipsis
       (pair? pattern)
       (pair? (cdr pattern))
       (identifier? (cadr pattern))
       (compare (cadr pattern) (rename ellipsis))))

(define (syntax-quote expression)
  `(,(classifier->keyword
      (lambda (form senv hist)
	(scheck '(_ datum) form senv hist)
	(constant-item (serror-ctx form senv hist) (cadr form))))
    ,expression))

(define (constant-null? rename compare expr)
  (and (quoted? rename compare expr)
       (eqv? '() (quoted-datum expr))))

(define (constant? rename compare expr)
  (or (quoted? rename compare expr)
      (boolean? expr)
      (bytevector? expr)
      (char? expr)
      (number? expr)
      (string? expr)
      (vector? expr)))

(define (constant->datum rename compare expr)
  (if (quoted? rename compare expr)
      (quoted-datum expr)
      expr))

(define (quoted-datum expr)
  (cadr expr))

(define (quoted? rename compare expr)
  (and (pair? expr)
       (compare (car expr) (rename 'quote))
       (pair? (cdr expr))
       (null? (cddr expr))))