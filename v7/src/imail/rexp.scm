;;; -*-Scheme-*-
;;;
;;; $Id: rexp.scm,v 1.7 2000/04/13 16:40:04 cph Exp $
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
      (char-set? rexp)
      (and (pair? rexp)
	   (list? (cdr rexp))
	   (let ((one-arg
		  (lambda ()
		    (and (fix:= 1 (length (cdr rexp)))
			 (rexp? (cadr rexp))))))
	     (case (car rexp)
	       ((ALTERNATIVES SEQUENCE)
		(for-all? (cdr rexp) rexp?))
	       ((GROUP OPTIONAL * +)
		(and (one-arg)
		     (not (and (pair? rexp)
			       (memq (car rexp) nongroupable-rexp-types)))))
	       ((ANY-CHAR LINE-START LINE-END STRING-START STRING-END
			  WORD-EDGE NOT-WORD-EDGE WORD-START WORD-END
			  WORD-CHAR NOT-WORD-CHAR)
		#t)
	       ((SYNTAX-CHAR NOT-SYNTAX-CHAR)
		(and (one-arg)
		     (assq (cadr rexp) syntax-type-alist)))
	       (else #f))))))

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

(define (rexp-alternatives . rexps)
  `(ALTERNATIVES ,@rexps))

(define (rexp-sequence . rexps)
  `(SEQUENCE ,@(map (lambda (rexp)
		      (if (and (pair? rexp)
			       (eq? (car rexp) 'ALTERNATIVES))
			  (rexp-group rexp)
			  rexp))
		    rexps)))

(define (rexp-group rexp) `(GROUP ,rexp))
(define (rexp-optional rexp) `(OPTIONAL ,(rexp-groupify rexp)))
(define (rexp* rexp) `(* ,(rexp-groupify rexp)))
(define (rexp+ rexp) `(+ ,(rexp-groupify rexp)))

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

(define (rexp-case-fold rexp)
  (let ((lose (lambda () (error "Malformed rexp:" rexp))))
    (cond ((string? rexp)
	   `(CASE-FOLD rexp))
	  ((and (pair? rexp)
		(memq (car rexp) '(ALTERNATIVES SEQUENCE GROUP OPTIONAL * +))
		(list? (cdr rexp)))
	   (cons (car rexp)
		 (map rexp-case-fold (cdr rexp))))
	  (else rexp))))

(define (rexp-groupify rexp)
  (let ((lose (lambda () (error "Malformed rexp:" rexp))))
    (cond ((string? rexp)
	   (if (fix:= 1 (string-length rexp))
	       rexp
	       (rexp-group rexp)))
	  ((char-set? rexp)
	   rexp)
	  ((pair? rexp)
	   (cond ((memq (car rexp) grouped-rexp-types)
		  rexp)
		 ((memq (car rexp) groupable-rexp-types)
		  (rexp-group rexp))
		 ((memq (car rexp) nongroupable-rexp-types)
		  (error "Expression can't be grouped:" rexp))
		 (else
		  (lose))))
	  (else (lose)))))

(define grouped-rexp-types
  '(GROUP ANY-CHAR WORD-CHAR NOT-WORD-CHAR SYNTAX-CHAR NOT-SYNTAX-CHAR))

(define groupable-rexp-types
  '(ALTERNATIVES SEQUENCE OPTIONAL * +))

(define nongroupable-rexp-types
  '(LINE-START LINE-END STRING-START STRING-END WORD-EDGE NOT-WORD-EDGE
	       WORD-START WORD-END))

(define (rexp-compile rexp)
  (re-compile-pattern (rexp->regexp rexp) #f))

(define (rexp->regexp rexp)
  (let ((lose (lambda () (error "Malformed rexp:" rexp))))
    (cond ((string? rexp)
	   (re-quote-string rexp))
	  ((char-set? rexp)
	   (char-set->regexp rexp))
	  ((and (pair? rexp) (list? (cdr rexp)))
	   (let ((one-arg
		  (lambda ()
		    (if (fix:= 1 (length (cdr rexp)))
			(cadr rexp)
			(lose))))
		 (rexp-args (lambda () (map rexp->regexp (cdr rexp)))))
	     (let ((rexp-arg (lambda () (rexp->regexp (one-arg))))
		   (syntax-type
		    (lambda ()
		      (let ((entry (assq (one-arg) syntax-type-alist)))
			(if entry
			    (cdr entry)
			    (lose))))))
	       (case (car rexp)
		 ((ALTERNATIVES) (separated-append (rexp-args) "\\|"))
		 ((SEQUENCE) (apply string-append (rexp-args)))
		 ((GROUP) (string-append "\\(" (rexp-arg) "\\)"))
		 ((OPTIONAL) (string-append (rexp-arg) "?"))
		 ((*) (string-append (rexp-arg) "*"))
		 ((+) (string-append (rexp-arg) "+"))
		 ((CASE-FOLD)
		  (let ((arg (one-arg)))
		    (if (string? arg)
			(case-fold-string arg)
			(lose))))
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

(define (case-fold-string s)
  (let ((end (string-length s)))
    (let loop ((start 0) (parts '()))
      (let ((index
	     (substring-find-next-char-in-set s start end
					      char-set:alphabetic)))
	(if index
	    (loop (fix:+ index 1)
		  (cons* (let ((char (string-ref s index)))
			   (string-append "["
					  (string (char-upcase char))
					  (string (char-downcase char))
					  "]"))
			 (re-quote-string
			  (substring s start index))
			 parts))
	    (apply string-append (reverse! parts)))))))

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