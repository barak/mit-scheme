;;; -*-Scheme-*-
;;;
;;; $Id: parser.scm,v 1.20 2001/10/16 04:59:21 cph Exp $
;;;
;;; Copyright (c) 2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Parser language

;;; A parser is a procedure of one argument, a parser buffer.  It
;;; attempts to parse the contents of the buffer, starting at the
;;; location of the buffer pointer.  If the parse is successful, the
;;; buffer pointer is advanced to the end of the parsed segment, and a
;;; vector of results is returned.  If the parse fails, the buffer
;;; pointer is unchanged, and #F is returned.

(declare (usual-integrations))

;;;; Preprocessor

(define (preprocess-parser-expression expression
				      external-bindings
				      internal-bindings)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression)))
	 (let ((preprocessor (parser-preprocessor (car expression))))
	   (if preprocessor
	       (preprocessor expression external-bindings internal-bindings)
	       (error "Unknown parser expression:" expression))))
	((symbol? expression)
	 (let ((preprocessor (parser-preprocessor expression)))
	   (if preprocessor
	       (preprocessor expression external-bindings internal-bindings)
	       expression)))
	((or (string? expression)
	     (char? expression))
	 (preprocess-parser-expression `(NOISE ,expression)
				       external-bindings
				       internal-bindings))
	(else
	 (error "Unknown parser expression:" expression))))

(define (preprocess-parser-expressions expressions
				       external-bindings
				       internal-bindings)
  (map (lambda (expression)
	 (preprocess-parser-expression expression
				       external-bindings
				       internal-bindings))
       expressions))

(define (define-parser-preprocessor name procedure)
  (if (pair? name)
      (for-each (lambda (name) (define-parser-preprocessor name procedure))
		name)
      (hash-table/put! parser-preprocessors name procedure))
  name)

(syntax-table/define system-global-syntax-table 'DEFINE-*PARSER-MACRO
  (lambda (bvl expression)
    (cond ((symbol? bvl)
	   `(DEFINE-*PARSER-EXPANDER ',bvl
	      (LAMBDA ()
		,expression)))
	  ((named-lambda-bvl? bvl)
	   `(DEFINE-*PARSER-EXPANDER ',(car bvl)
	      (LAMBDA ,(cdr bvl)
		,expression)))
	  (else
	   (error "Malformed bound-variable list:" bvl)))))

(define (define-*parser-expander name procedure)
  (define-parser-macro name
    (lambda (expression external-bindings internal-bindings)
      (preprocess-parser-expression (if (pair? expression)
					(apply procedure (cdr expression))
					(procedure))
				    external-bindings
				    internal-bindings))))

(define (parser-preprocessor name)
  (or (lookup-parser-macro name)
      (hash-table/get parser-preprocessors name #f)))

(define parser-preprocessors
  (make-eq-hash-table))

(define-*parser-expander '+
  (lambda (expression)
    `(SEQ ,expression (* ,expression))))

(define-*parser-expander '?
  (lambda (expression)
    `(ALT ,expression (SEQ))))

(define-*parser-expander 'COMPLETE
  (lambda (expression)
    `(SEQ ,expression (MATCH (END-OF-INPUT)))))

(define-*parser-expander 'TOP-LEVEL
  (lambda (expression)
    `(SEQ ,expression (DISCARD-MATCHED))))

(define-parser-preprocessor '(ALT SEQ)
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,@(flatten-expressions (preprocess-parser-expressions (cdr expression)
							    external-bindings
							    internal-bindings)
			     (car expression)))))

(define-parser-preprocessor '*
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,(preprocess-parser-expression (check-1-arg expression)
				     external-bindings
				     internal-bindings))))

(define-parser-preprocessor '(MATCH NOISE)
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,(preprocess-matcher-expression (check-1-arg expression)
				      external-bindings
				      internal-bindings))))

(define-parser-preprocessor '(TRANSFORM MAP ENCAPSULATE)
  (lambda (expression external-bindings internal-bindings)
    (check-2-args expression)
    `(,(car expression) ,(cadr expression)
			,(preprocess-parser-expression (caddr expression)
						       external-bindings
						       internal-bindings))))

(define-parser-preprocessor 'WITH-POINTER
  (lambda (expression external-bindings internal-bindings)
    (check-2-args expression (lambda (expression) (symbol? (cadr expression))))
    `(,(car expression) ,(cadr expression)
			,(preprocess-parser-expression (caddr expression)
						       external-bindings
						       internal-bindings))))

(define-parser-preprocessor 'SEXP
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (check-1-arg expression)
    expression))

(define-parser-preprocessor 'DISCARD-MATCHED
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (check-0-args expression)
    expression))

(define-parser-preprocessor 'VALUES
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    expression))

;;;; Compiler

(syntax-table/define system-global-syntax-table '*PARSER
  (lambda (expression)
    (optimize-expression (generate-parser-code expression))))

(define (generate-parser-code expression)
  (generate-external-procedure expression
			       preprocess-parser-expression
			       (lambda (expression)
				 `(,(compile-parser-expression expression)
				   (LAMBDA (V KF) KF V)
				   (LAMBDA () #F)))))

(define (compile-parser-expression expression)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression))
	      (hash-table/get parser-compilers (car expression) #f))
	 => (lambda (entry)
	      (let ((arity (car entry))
		    (compiler (cdr entry)))
		(if (and arity (not (= (length (cdr expression)) arity)))
		    (error "Incorrect arity for parser:" expression))
		(apply compiler (cdr expression)))))
	((or (symbol? expression)
	     (and (pair? expression) (eq? (car expression) 'SEXP)))
	 (wrap-external-parser
	  `(,(if (pair? expression) (cadr expression) expression)
	    ,*buffer-name*)))
	(else
	 (error "Malformed parser:" expression))))

(define-macro (define-parser form . compiler-body)
  (let ((name (car form))
	(parameters (cdr form)))
    `(DEFINE-PARSER-COMPILER ',name
       ,(if (symbol? parameters) `#F (length parameters))
       (LAMBDA ,parameters
	 ,@compiler-body))))

(define (define-parser-compiler keyword arity compiler)
  (hash-table/put! parser-compilers keyword (cons arity compiler))
  keyword)

(define parser-compilers
  (make-eq-hash-table))

(define-parser (match expression)
  (wrap-parser
   (lambda (ks kf)
     (call-with-pointer
      (lambda (p)
	`(,(compile-matcher-expression expression)
	  ,(let ((kf2 (make-kf-identifier)))
	     `(LAMBDA (,kf2)
		(,ks (VECTOR (GET-PARSER-BUFFER-TAIL ,*buffer-name* ,p))
		     ,kf2)))
	  ,kf))))))

(define-parser (noise expression)
  (wrap-parser
   (lambda (ks kf)
     `(,(compile-matcher-expression expression)
       ,(let ((kf2 (make-kf-identifier)))
	  `(LAMBDA (,kf2)
	     (,ks '#() ,kf2)))
       ,kf))))

(define-parser (values . expressions)
  (wrap-parser
   (lambda (ks kf)
     `(,ks (VECTOR ,@expressions) ,kf))))

(define-parser (transform transform expression)
  (post-processed-parser expression
    (lambda (ks v kf)
      (handle-parser-value `(,transform ,v) ks kf))))

(define-parser (map transform expression)
  (post-processed-parser expression
    (lambda (ks v kf)
      `(,ks (VECTOR-MAP ,transform ,v) ,kf))))

(define-parser (encapsulate transform expression)
  (post-processed-parser expression
    (lambda (ks v kf)
      `(,ks (VECTOR (,transform ,v)) ,kf))))

(define (post-processed-parser expression procedure)
  (wrap-parser
   (lambda (ks kf)
     `(,(compile-parser-expression expression)
       ,(let ((v (make-value-identifier))
	      (kf2 (make-kf-identifier)))
	  `(LAMBDA (,v ,kf2)
	     ,(procedure ks v kf2)))
       ,kf))))

(define-parser (with-pointer identifier expression)
  `(LET ((,identifier ,(fetch-pointer)))
     ,(compile-parser-expression expression)))

(define-parser (discard-matched)
  (wrap-parser
   (lambda (ks kf)
     `(BEGIN
	(DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
	(,ks '#() ,kf)))))

(define-parser (seq . expressions)
  (if (pair? expressions)
      (if (pair? (cdr expressions))
	  (wrap-parser
	   (lambda (ks kf)
	     (let loop ((expressions expressions) (vs '()) (kf2 kf))
	       (if (pair? (cdr expressions))
		   (call-with-pointer
		    (lambda (p)
		      `(,(compile-parser-expression (car expressions))
			,(let ((v (make-value-identifier))
			       (kf3 (make-kf-identifier)))
			   `(LAMBDA (,v ,kf3)
			      ,(loop (cdr expressions)
				     (cons v vs)
				     `(LAMBDA ()
					,(backtrack-to p)
					(,kf3)))))
			,kf2)))
		   `(,(compile-parser-expression (car expressions))
		     ,(let ((v (make-value-identifier))
			    (kf3 (make-kf-identifier)))
			`(LAMBDA (,v ,kf3)
			   (,ks (VECTOR-APPEND ,@(reverse (cons v vs)))
				,kf3)))
		     ,kf2)))))
	  (compile-parser-expression (car expressions)))
      (wrap-parser (lambda (ks kf) `(,ks '#() ,kf)))))

(define-parser (alt . expressions)
  (if (pair? expressions)
      (if (pair? (cdr expressions))
	  (wrap-parser
	   (lambda (ks kf)
	     (call-with-pointer
	      (lambda (p)
		(let ((ks2 (make-ks-identifier))
		      (v (make-value-identifier))
		      (kf2 (make-kf-identifier)))
		  `(LET ((,ks2
			  (LAMBDA (,v ,kf2)
			    (,ks ,v
				 (LAMBDA ()
				   ,(backtrack-to p)
				   (,kf2))))))
		     ,(let loop ((expressions expressions))
			(if (pair? (cdr expressions))
			    `(,(compile-parser-expression (car expressions))
			      ,ks2
			      (LAMBDA ()
				,(loop (cdr expressions))))
			    `(,(compile-parser-expression (car expressions))
			      ,ks
			      ,kf)))))))))
	  (compile-parser-expression (car expressions)))
      (wrap-parser (lambda (ks kf) ks `(,kf)))))

(define-parser (* expression)
  (wrap-parser
   (lambda (ks kf)
     (let ((ks2 (make-ks-identifier))
	   (v (make-value-identifier))
	   (kf2 (make-kf-identifier)))
       `(LET ,ks2 ((,v '#()) (,kf2 ,kf))
	  ,(call-with-pointer
	    (lambda (p)
	      `(,(compile-parser-expression expression)
		,(let ((v2 (make-value-identifier))
		       (kf3 (make-kf-identifier)))
		   `(LAMBDA (,v2 ,kf3)
		      (,ks2 (VECTOR-APPEND ,v ,v2)
			    (LAMBDA ()
			      ,(backtrack-to p)
			      (,ks ,v ,kf3)))))
		(LAMBDA ()
		  (,ks ,v ,kf2))))))))))