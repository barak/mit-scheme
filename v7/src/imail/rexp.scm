;;; -*-Scheme-*-
;;;
;;; $Id: rexp.scm,v 1.1 2000/04/13 15:36:02 cph Exp $
;;;
;;; Copyright (c) 2000 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; List-based Regular Expressions

(declare (usual-integrations))

(define (rexp? object)
  (or (string? rexp)
      (char? rexp)
      (char-set? rexp)
      (and (pair? rexp)
	   (list? (cdr rexp))
	   (let ((one-arg
		  (lambda ()
		    (and (fix:= 1 (length (cdr rexp)))
			 (rexp? (cadr rexp))))))
	     (case (car rexp)
	       ((GROUP ALTERNATIVES)
		(for-all? (cdr rexp) rexp?))
	       ((? * +)
		(and (one-arg)
		     (not (or (and (string? rexp)
				   (string-null? rexp))
			      (and (pair? rexp)
				   (memq (car rexp)
					 nongroupable-rexp-types))))))
	       ((ANY-CHAR LINE-START LINE-END STRING-START STRING-END
			  WORD-EDGE NOT-WORD-EDGE WORD-START WORD-END
			  WORD-CHAR NOT-WORD-CHAR)
		#t)
	       ((SYNTAX-CHAR NOT-SYNTAX-CHAR)
		(and (one-arg)
		     (assq (cadr rexp) syntax-type-alist)))
	       (else #f))))))

(define (rexp-group . rexps) `(GROUP ,@rexps))
(define (rexp-alternatives . rexps) `(ALTERNATIVES ,@rexps))
(define (rexp-optional rexp) `(? ,rexp))
(define (rexp* rexp) `(* ,rexp))
(define (rexp+ rexp) `(+ ,rexp))

(define (rexp-any-char) `(ANY-CHAR))
(define (rexp-line-start) `(LINE-START))
(define (rexp-line-end) `(LINE-END))
(define (rexp-string-start) `(STRING-START))
(define (rexp-string-end) `(STRING-END))
(define (rexp-word-edge) `(WORD-EDGE))
(define (rexp-not-word-edge) `(NOT-WORD-EDGE))
(define (rexp-word-start) `(WORD-START))
(define (rexp-word-end) `(WORD-END))
(define (rexp-word-char) `(WORD-CHAR))
(define (rexp-not-word-char) `(NOT-WORD-CHAR))
(define (rexp-syntax-char type) `(SYNTAX-CHAR ,type))
(define (rexp-not-syntax-char type) `(NOT-SYNTAX-CHAR ,type))

(define (rexp-compile-pattern rexp case-fold?)
  (re-compile-pattern (rexp->regexp rexp) case-fold?))

(define (rexp->regexp rexp)
  (let ((lose (lambda () (error "Malformed rexp:" rexp))))
    (cond ((string? rexp)
	   (re-quote-string rexp))
	  ((char? rexp)
	   (re-quote-string (string rexp)))
	  ((char-set? rexp)
	   (char-set->regexp rexp))
	  ((and (pair? rexp) (list? (cdr rexp)))
	   (let ((alternatives
		  (lambda ()
		    (separated-append (map rexp->regexp (cdr rexp)) "\\|")))
		 (one-arg
		  (lambda ()
		    (if (not (fix:= 1 (length (cdr rexp))))
			(lose))
		    (cadr rexp))))
	     (let ((repeat-arg
		    (lambda ()
		      (rexp->regexp (rexp-groupify (one-arg)))))
		   (syntax-type
		    (lambda ()
		      (let ((entry (assq (one-arg) syntax-type-alist)))
			(if entry
			    (cdr entry)
			    (lose))))))
	       (case (car rexp)
		 ((GROUP) (string-append "\\(" (alternatives) "\\)"))
		 ((ALTERNATIVES) (alternatives))
		 ((?) (string-append (repeat-arg) "?"))
		 ((*) (string-append (repeat-arg) "*"))
		 ((+) (string-append (repeat-arg) "+"))
		 ((ANY-CHAR) ".")
		 ((LINE-START) "^")
		 ((LINE-END) "$")
		 ((STRING-START) "\\`")
		 ((STRING-END) "\\'")
		 ((WORD-EDGE) "\\b")
		 ((NOT-WORD-EDGE) "\\B")
		 ((WORD-START) "\\<")
		 ((WORD-END) "\\>")
		 ((WORD-CHAR) "\\w")
		 ((NOT-WORD-CHAR) "\\W")
		 ((SYNTAX-CHAR) (string-append "\\s" (syntax-type)))
		 ((NOT-SYNTAX-CHAR) (string-append "\\S" (syntax-type)))
		 (else (lose))))))
	  (else (lose)))))

(define syntax-type-alist
  '((WHITESPACE . " ")
    (PUNCTUATION . ".")
    (WORD . "w")
    (SYMBOL . "_")
    (OPEN . "(")
    (CLOSE . ")")
    (QUOTE . "\'")
    (STRING-DELIMITER . "\"")
    (MATH-DELIMITER . "$")
    (ESCAPE . "\\")
    (CHAR-QUOTE . "/")
    (COMMENT-START . "<")
    (COMMENT-END . ">")))

(define (rexp-groupify rexp)
  (let ((lose (lambda () (error "Malformed rexp:" rexp)))
	(no-group (lambda () (error "Expression can't be grouped:" rexp))))
    (cond ((string? rexp)
	   (case (string-length rexp)
	     ((0) (no-group))
	     ((1) rexp)
	     (else (rexp-group rexp))))
	  ((or (char? rexp) (char-set? rexp))
	   rexp)
	  ((pair? rexp)
	   (cond ((memq (car rexp) grouped-rexp-types) rexp)
		 ((memq (car rexp) groupable-rexp-types) (rexp-group rexp))
		 ((memq (car rexp) nongroupable-rexp-types) (no-group))
		 (else (lose))))
	  (else (lose)))))

(define grouped-rexp-types
  '(GROUP ANY-CHAR WORD-CHAR NOT-WORD-CHAR SYNTAX-CHAR NOT-SYNTAX-CHAR))

(define groupable-rexp-types
  '(ALTERNATIVES ? * +))

(define nongroupable-rexp-types
  '(LINE-START LINE-END STRING-START STRING-END WORD-EDGE NOT-WORD-EDGE
	       WORD-START WORD-END))

(define (separated-append tokens separator)
  (cond ((not (pair? tokens)) "")
	((not (pair? (cdr tokens))) (car tokens))
	(else
	 (let ((string
		(make-string
		 (let ((ns (string-length separator)))
		   (do ((tokens (cdr tokens) (cdr tokens))
			(count (string-length (car tokens))
			       (fix:+ count
				      (fix:+ (string-length (car tokens))
					     ns))))
		       ((not (pair? tokens)) count))))))
	   (let loop
	       ((tokens (cdr tokens))
		(index (string-move! (car tokens) string 0)))
	     (if (pair? tokens)
		 (loop (cdr tokens)
		       (string-move! (car tokens)
				     string
				     (string-move! separator string index)))))
	   string))))