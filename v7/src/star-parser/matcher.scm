;;; -*-Scheme-*-
;;;
;;; $Id: matcher.scm,v 1.4 2001/06/26 21:16:44 cph Exp $
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
  (with-canonical-matcher-expression expression
    (lambda (expression)
      (compile-matcher-expression
       expression
       (no-pointers)
       (simple-backtracking-continuation `#T)
       (simple-backtracking-continuation `#F)))))

(define (compile-matcher-expression expression pointers if-succeed if-fail)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression))
	      (hash-table/get matcher-compilers (car expression) #f))
	 => (lambda (entry)
	      (let ((arity (car entry))
		    (compiler (cdr entry)))
		(if (and arity (not (= (length (cdr expression)) arity)))
		    (error "Incorrect arity for matcher:" expression))
		(apply compiler pointers if-succeed if-fail
		       (if arity
			   (cdr expression)
			   (list (cdr expression)))))))
	((symbol? expression)
	 (handle-pending-backtracking pointers
	   (lambda (pointers)
	     `(IF (,expression ,*buffer-name*)
		  ,(if-succeed (unknown-location pointers))
		  ,(if-fail pointers)))))
	(else
	 (error "Malformed matcher:" expression))))

;;;; Canonicalization

(define (with-canonical-matcher-expression expression receiver)
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
	      (receiver expression))))))))

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
	     ((SEXP)
	      (handle-complex-expression (check-1-arg expression)
					 internal-bindings))
	     (else
	      (error "Unknown matcher expression:" expression))))
	  ((symbol? expression)
	   expression)
	  (else
	   (error "Unknown matcher expression:" expression))))
  (do-expression expression))

;;;; Matchers

(define-macro (define-matcher form compiler-body)
  (let ((name (car form))
	(parameters (cdr form)))
    (if (symbol? parameters)
	`(DEFINE-MATCHER-COMPILER ',name #F
	   (LAMBDA (POINTERS IF-SUCCEED IF-FAIL ,parameters)
	     ,compiler-body))
	`(DEFINE-MATCHER-COMPILER ',name ,(length parameters)
	   (LAMBDA (POINTERS IF-SUCCEED IF-FAIL ,@parameters)
	     ,compiler-body)))))

(define (define-matcher-compiler keyword arity compiler)
  (hash-table/put! matcher-compilers keyword (cons arity compiler))
  keyword)

(define matcher-compilers
  (make-eq-hash-table))

(define-macro (define-atomic-matcher form test-expression)
  `(DEFINE-MATCHER ,form
     (HANDLE-PENDING-BACKTRACKING POINTERS
       (LAMBDA (POINTERS)
	 `(IF ,,test-expression
	      ,(IF-SUCCEED (UNKNOWN-LOCATION POINTERS))
	      ,(IF-FAIL POINTERS))))))

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

(define-matcher (* expression)
  (handle-pending-backtracking pointers
    (lambda (pointers)
      (let ((v (generate-uninterned-symbol)))
	`(BEGIN
	   (LET ,v ()
	     ,(compile-matcher-expression expression (no-pointers)
		(simple-backtracking-continuation `(,v))
		(simple-backtracking-continuation `UNSPECIFIC)))
	   ,(if-succeed (no-pointers)))))))

(define-matcher (seq . expressions)
  (with-current-pointer pointers
    (lambda (start-pointers)
      (let loop
	  ((expressions expressions)
	   (pointers start-pointers))
	(if (pair? expressions)
	    (compile-matcher-expression (car expressions)
					pointers
					(lambda (pointers)
					  (loop (cdr expressions) pointers))
					(lambda (pointers)
					  (if-fail
					   (new-backtrack-pointer
					    start-pointers pointers))))
	    (if-succeed pointers))))))

(define-matcher (alt . expressions)
  (with-current-pointer pointers
    (lambda (pointers)
      (let loop ((expressions expressions))
	(if (pair? expressions)
	    (let ((predicate
		   (compile-matcher-expression
		    (car expressions)
		    pointers
		    (simple-backtracking-continuation '#T)
		    (simple-backtracking-continuation '#F)))
		  (consequent
		   (lambda () (if-succeed (unknown-location pointers))))
		  (alternative
		   (lambda () (loop (cdr expressions)))))
	      (cond ((eq? predicate '#T) (consequent))
		    ((eq? predicate '#F) (alternative))
		    (else `(IF ,predicate ,(consequent) ,(alternative)))))
	    (if-fail pointers))))))

;;; Edwin Variables:
;;; Eval: (scheme-indent-method 'handle-pending-backtracking 1)
;;; Eval: (scheme-indent-method 'define-matcher-optimizer 2)
;;; Eval: (scheme-indent-method 'with-buffer-name 0)
;;; End:
