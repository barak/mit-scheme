;;; -*-Scheme-*-
;;;
;;; $Id: matcher.scm,v 1.9 2001/07/02 12:14:29 cph Exp $
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

;;;; Pattern-matcher language

(declare (usual-integrations))

;;; A matcher is a procedure of one argument, a parser buffer.
;;; It performs a match against the contents of the buffer, starting
;;; at the location of the buffer pointer.  If the match is
;;; successful, the buffer pointer is advanced to the end of the
;;; matched segment, and #T is returned.  If the match fails, the
;;; buffer pointer is unchanged, and #F is returned.

;;; The *MATCHER macro provides a concise way to define a broad class
;;; of matchers using a BNF-like syntax.

(syntax-table/define system-global-syntax-table '*MATCHER
  (lambda (expression)
    (optimize-expression (generate-matcher-code expression))))

(define (generate-matcher-code expression)
  (let ((external-bindings (list 'BINDINGS))
	(internal-bindings (list 'BINDINGS)))
    (let ((expression
	   (canonicalize-matcher-expression expression
					    external-bindings
					    internal-bindings)))
      (maybe-make-let (map (lambda (b) (list (cdr b) (car b)))
			   (cdr external-bindings))
	(with-buffer-name
	  (lambda ()
	    (maybe-make-let (map (lambda (b) (list (cdr b) (car b)))
				 (cdr internal-bindings))
	      (call-with-unknown-pointer
	       (lambda (pointer)
		 (compile-matcher-expression expression pointer
		   (simple-backtracking-continuation `#T)
		   (simple-backtracking-continuation `#F)))))))))))

(define (compile-matcher-expression expression pointer if-succeed if-fail)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression))
	      (hash-table/get matcher-compilers (car expression) #f))
	 => (lambda (entry)
	      (let ((arity (car entry))
		    (compiler (cdr entry)))
		(if (and arity (not (= (length (cdr expression)) arity)))
		    (error "Incorrect arity for matcher:" expression))
		(apply compiler pointer if-succeed if-fail
		       (if arity
			   (cdr expression)
			   (list (cdr expression)))))))
	((symbol? expression)
	 (handle-pending-backtracking pointer
	   (lambda (pointer)
	     `(IF (,expression ,*buffer-name*)
		  ,(call-with-unknown-pointer if-succeed)
		  ,(if-fail pointer)))))
	(else
	 (error "Malformed matcher:" expression))))

(syntax-table/define system-global-syntax-table 'DEFINE-*MATCHER-MACRO
  (lambda (bvl expression)
    (cond ((symbol? bvl)
	   `(DEFINE-*MATCHER-MACRO* ',bvl
	      (LAMBDA ()
		,expression)))
	  ((named-lambda-bvl? bvl)
	   `(DEFINE-*MATCHER-MACRO* ',(car bvl)
	      (LAMBDA ,(cdr bvl)
		,expression)))
	  (else
	   (error "Malformed bound-variable list:" bvl)))))

(define (define-*matcher-macro* name procedure)
  (hash-table/put! *matcher-macros name procedure)
  name)

(define (*matcher-expander name)
  (hash-table/get *matcher-macros name #f))

(define *matcher-macros
  (make-eq-hash-table))

;;;; Canonicalization

(define (canonicalize-matcher-expression expression
					 external-bindings internal-bindings)
  (define (do-expression expression)
    (cond ((and (pair? expression)
		(symbol? (car expression))
		(list? (cdr expression)))
	   (case (car expression)
	     ((ALT SEQ)
	      `(,(car expression)
		,@(flatten-expressions (map do-expression (cdr expression))
				       (car expression))))
	     ((*)
	      `(,(car expression)
		,(do-expression (check-1-arg expression))))
	     ((+)
	      (do-expression
	       (let ((expression (check-1-arg expression)))
		 `(SEQ ,expression (* ,expression)))))
	     ((?)
	      (do-expression
	       `(ALT ,(check-1-arg expression) (SEQ))))
	     ((CHAR CHAR-CI NOT-CHAR NOT-CHAR-CI)
	      `(,(car expression)
		,(handle-complex-expression (check-1-arg expression)
					    internal-bindings)))
	     ((STRING)
	      (let ((string (check-1-arg expression)))
		(if (and (string? string) (fix:= (string-length string) 1))
		    `(CHAR ,(string-ref string 0))
		    `(STRING
		      ,(handle-complex-expression string
						  internal-bindings)))))
	     ((STRING-CI)
	      (let ((string (check-1-arg expression)))
		(if (and (string? string) (fix:= (string-length string) 1))
		    `(CHAR-CI ,(string-ref string 0))
		    `(STRING-CI
		      ,(handle-complex-expression string
						  internal-bindings)))))
	     ((ALPHABET)
	      `(,(car expression)
		,(let ((arg (check-1-arg expression)))
		   (if (string? arg)
		       (handle-complex-expression
			(if (string-prefix? "^" arg)
			    `(RE-COMPILE-CHAR-SET ,(string-tail arg 1) #T)
			    `(RE-COMPILE-CHAR-SET ,arg #F))
			external-bindings)
		       (handle-complex-expression arg internal-bindings)))))
	     ((WITH-POINTER)
	      (check-2-args expression
			    (lambda (expression) (symbol? (cadr expression))))
	      `(,(car expression)
		,(cadr expression)
		,(do-expression (caddr expression))))
	     ((SEXP)
	      (handle-complex-expression (check-1-arg expression)
					 internal-bindings))
	     (else
	      (let ((expander (*matcher-expander (car expression))))
		(if expander
		    (do-expression (apply expander (cdr expression)))
		    (error "Unknown matcher expression:" expression))))))
	  ((symbol? expression)
	   (let ((expander (*matcher-expander expression)))
	     (if expander
		 (do-expression (expander))
		 expression)))
	  (else
	   (error "Unknown matcher expression:" expression))))
  (do-expression expression))

;;;; Matchers

(define-macro (define-matcher form . compiler-body)
  (let ((name (car form))
	(parameters (cdr form)))
    (if (symbol? parameters)
	`(DEFINE-MATCHER-COMPILER ',name #F
	   (LAMBDA (POINTER IF-SUCCEED IF-FAIL ,parameters)
	     ,@compiler-body))
	`(DEFINE-MATCHER-COMPILER ',name ,(length parameters)
	   (LAMBDA (POINTER IF-SUCCEED IF-FAIL ,@parameters)
	     ,@compiler-body)))))

(define (define-matcher-compiler keyword arity compiler)
  (hash-table/put! matcher-compilers keyword (cons arity compiler))
  keyword)

(define matcher-compilers
  (make-eq-hash-table))

(define-macro (define-atomic-matcher form test-expression)
  `(DEFINE-MATCHER ,form
     (HANDLE-PENDING-BACKTRACKING POINTER
       (LAMBDA (POINTER)
	 `(IF ,,test-expression
	      ,(CALL-WITH-UNKNOWN-POINTER IF-SUCCEED)
	      ,(IF-FAIL POINTER))))))

(define-atomic-matcher (char char)
  `(MATCH-PARSER-BUFFER-CHAR ,*buffer-name* ,char))

(define-atomic-matcher (char-ci char)
  `(MATCH-PARSER-BUFFER-CHAR-CI ,*buffer-name* ,char))

(define-atomic-matcher (not-char char)
  `(MATCH-PARSER-BUFFER-NOT-CHAR ,*buffer-name* ,char))

(define-atomic-matcher (not-char-ci char)
  `(MATCH-PARSER-BUFFER-NOT-CHAR-CI ,*buffer-name* ,char))

(define-atomic-matcher (alphabet alphabet)
  `(MATCH-PARSER-BUFFER-CHAR-IN-SET ,*buffer-name* ,alphabet))

(define-atomic-matcher (string string)
  `(MATCH-PARSER-BUFFER-STRING ,*buffer-name* ,string))

(define-atomic-matcher (string-ci string)
  `(MATCH-PARSER-BUFFER-STRING-CI ,*buffer-name* ,string))

(define-matcher (with-pointer identifier expression)
  `(LET ((,identifier ,(pointer-reference pointer)))
     ,(compile-matcher-expression expression pointer if-succeed if-fail)))

(define-matcher (* expression)
  if-fail
  (handle-pending-backtracking pointer
    (lambda (pointer)
      pointer
      (call-with-unknown-pointer
       (lambda (pointer)
	 (let ((v (generate-uninterned-symbol)))
	   `(BEGIN
	      (LET ,v ()
		,(compile-matcher-expression expression pointer
		   (simple-backtracking-continuation `(,v))
		   (simple-backtracking-continuation `UNSPECIFIC)))
	      ,(if-succeed pointer))))))))

(define-matcher (seq . expressions)
  (let loop ((expressions expressions) (pointer* pointer))
    (if (pair? expressions)
	(compile-matcher-expression (car expressions) pointer*
	  (lambda (pointer*)
	    (loop (cdr expressions) pointer*))
	  (lambda (pointer*)
	    (if-fail (backtrack-to pointer pointer*))))
	(if-succeed pointer*))))

(define-matcher (alt . expressions)
  (cond ((not (pair? expressions))
	 (if-fail pointer))
	((not (pair? (cdr expressions)))
	 (compile-matcher-expression expression pointer if-succeed if-fail))
	(else
	 (handle-pending-backtracking pointer
	   (lambda (pointer)
	     `(IF (OR ,@(map (let ((s (simple-backtracking-continuation '#T))
				   (f (simple-backtracking-continuation '#F)))
			       (lambda (expression)
				 (compile-matcher-expression expression pointer
				   s f)))
			     expressions))
		  ,(call-with-unknown-pointer if-succeed)
		  ,(if-fail pointer)))))))

;;; Edwin Variables:
;;; Eval: (scheme-indent-method 'handle-pending-backtracking 1)
;;; Eval: (scheme-indent-method 'define-matcher-optimizer 2)
;;; Eval: (scheme-indent-method 'with-buffer-name 0)
;;; Eval: (scheme-indent-method 'compile-matcher-expression 2)
;;; End:
