#| -*-Scheme-*-

$Id: synutl.scm,v 1.2 1995/04/07 04:55:36 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

;;;; ??
;;; package: (compiler midend)

(declare (usual-integrations))

;;; Syntax-time utilities

(define (%matchup lambda-list prefix expr)
  (if (null? lambda-list)
      (values '() prefix)
      (let ((var* (generate-uninterned-symbol "SUBFORM")))
	(let loop ((ll lambda-list)
		   (names '())
		   (args '())
		   (path var*))
	  (cond ((null? ll)
		 (values (reverse names)
			 `(let ((,var* ,expr))
			    (,@prefix ,@(reverse args)))))
		((eq? (car ll) '#!rest)
		 (loop '()
		       (cons (cadr ll) names)
		       (cons path args)
		       false))
		(else
		 (loop (cdr ll)
		       (cons (car ll) names)
		       (cons `(car ,path) args)
		       `(cdr ,path))))))))

;; (match expr (pattern qualifier ... => action ...) ...)
;; qualifier is a scheme expression
;;
;; Patterns
;; (? name)				matches anything & binds name
;; (? name pred)			if `true', binds to result of xform
;; ,expr				matches equal? to expression: ,'? ,'_
;; _                                    wildcard like (? G1278)
;; (pattern . pattern)			a pair
;; 
;;
;; (let (( (? framevar) (CALL ',%mmm _ '(? frame-vector)))) _)
;;
;; (CALL ',+ '#F '(? a) '(? b)) => ',(+ a b))
;; (CALL ',+ '#F '?a '?b) => ',(+ ?a ?b))
;; (CALL ',+ '#F '?a '?b) (number? a) => ',(+ a b))
;; (CALL ',+ '#F ?a ?b)  => ',(+ ?a ?b))

(define (%compile-match-expression expr all-clauses)
  (define (compile-clauses subject clauses)
    (cond ((null? clauses)
	   `(BEGIN (ERROR "Pattern match failed" ,subject)
		   ,unspecific))
	  ((or (not (pair? clauses)) (not (pair? (car clauses))))
	   (error "Bad clause list" all-clauses))
	  (else
	   (parse-clause
	    (car clauses)
	    (lambda (pattern qualifiers actions)
	      (compile-match subject pattern qualifiers actions
			     (compile-clauses subject (cdr clauses))))))))

  (define (compile-match subject pattern qualifiers actions alternate)
    (compile-pattern subject pattern '()
		     (lambda (predicate selectors)
		       (if (null? qualifiers)
			   (ifify predicate
				  (letify (reverse selectors)
					  actions)
				  alternate)
			   (let ((alt (generate-uninterned-symbol)))
			     `(LET ((,alt (LAMBDA () ,alternate)))
				,(ifify predicate
					(letify (reverse selectors)
						(list
						 (ifify (andify qualifiers)
							(beginify actions)
							`(,alt))))
					`(,alt))))))))

  (define (compile-pattern subject pattern selectors receiver)
    (cond ((null? pattern) (receiver `(NULL? ,subject) selectors))
	  ((eq? pattern '_) (receiver `#T selectors))
	  ((symbol? pattern)
	   (let* ((name (symbol-name pattern))
		  (slen (string-length name)))
	     (if (and (> slen 0)
		      (char=? (string-ref name 0) #\?))
		 (compile-pattern subject
				  `(? ,(string->symbol (substring name 1 slen)))
				  selectors receiver)
		 (receiver `(EQ? ,subject ',pattern) selectors))))
	  ((number? pattern) (receiver `(EQV? ,subject ,pattern) selectors))
	  ((and (pair? pattern)
		(eq? (car pattern) '?))
	   (cond ((assq (cadr pattern) selectors)
		  => (lambda (place)
		       (receiver `(EQ? ,subject ,(cadr place)) selectors)))
		 (else
		  (receiver #T (cons `(,(cadr pattern) ,subject) selectors)))))
	  ((and (pair? pattern)
		(eq? (car pattern) 'unquote))
	   (receiver `(EQ? ,subject ,(second pattern)) selectors))
	  ((pair? pattern)
	   (compile-pattern
	    `(CAR ,subject) (car pattern) selectors
	    (lambda (predicate selectors)
	      (compile-pattern
	       `(CDR ,subject) (cdr pattern) selectors
	       (lambda (predicate* selectors)
		 (receiver (andify (list `(PAIR? ,subject)
					 predicate
					 predicate*))
			   selectors))))))
	  (else
	   (error "Illegal MATCH pattern syntax:" pattern))))
	  
  (define (andify preds)
    (define (and-flatten preds)
      (cond ((null? preds) '())
	    ((eq? #T (car preds)) (and-flatten (cdr preds)))
	    ((and (pair? (car preds)) (eq? 'and (caar preds)))
	     (append (and-flatten (cdar preds)) (and-flatten (cdr preds))))
	    (else (cons (car preds) (and-flatten (cdr preds))))))
    (let ((preds (and-flatten preds)))
      (cond ((null? preds) #T)
	    ((null? (cdr preds)) (car preds))
	    (else `(AND ,@preds)))))

  (define (ifify pred conseq alt)
    (cond ((eq? pred #T)  conseq)
	  ((eq? pred #F)  alt)
	  (else `(IF ,pred ,conseq ,alt))))

  (define (letify bindings body)
    (if (null? bindings)
	(beginify body)
	`(LET ,bindings ,@body)))

  (define (beginify actions)
    (if (and (pair? actions) (null? (cdr actions)))
	(car actions)
	`(BEGIN ,@actions)))

  (define (parse-clause clause receiver)
    (let ((pat  (car clause)))
      (let loop ((actions (cdr clause)) (quals '()))
	(cond ((null? actions)
	       (error "Illegal clause" clause))
	      ((eq? (car actions) '=>)
	       (receiver pat (reverse! quals) (cdr actions)))
	      (else
	       (loop (cdr actions) (cons (car actions) quals)))))))

  (if (symbol? expr)
      (compile-clauses expr all-clauses)
      (let ((subject  (generate-uninterned-symbol)))
	`(LET ((,subject ,expr))
	   ,(compile-clauses subject all-clauses)))))

(define-macro (match expr . clauses)
  (%compile-match-expression expr clauses))