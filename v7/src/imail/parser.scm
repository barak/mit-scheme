;;; -*-Scheme-*-
;;;
;;; $Id: parser.scm,v 1.4 2000/06/01 20:06:38 cph Exp $
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

;;;; Parsing support

(declare (usual-integrations))

;;;; Parser language

;;; A parser is a procedure that accepts a substring as three
;;; arguments and returns one of two values.  If the parser
;;; successfully parses the substring, it returns a pair whose car is
;;; an index into the substring indicating how much of the substring
;;; was parsed, and whose cdr is an alist of keyword/token pairs.  If
;;; the parser fails, it returns #F.

(define (parse-string parser string)
  (parse-substring parser string 0 (string-length string)))

(define (parse-substring parser string start end)
  (let ((pv (parser string start end)))
    (and pv
	 (fix:= (car pv) end)
	 pv)))

(define (parser-token parser-value keyword)
  (let ((entry (assq keyword (cdr parser-value))))
    (and entry
	 (cdr entry))))

(define (parse-never string start end)
  string start end
  #f)

(define (parse-always string start end)
  string end
  (list start))

(define (noise-parser match)
  (lambda (string start end)
    (let ((i (match string start end)))
      (and i
	   (list i)))))

(define (simple-parser match keyword)
  (lambda (string start end)
    (let ((i (match string start end)))
      (and i
	   (list i (cons keyword (substring string start i)))))))

(define (decoding-parser match-encoded decode parse-decoded)
  (lambda (string start end)
    (let ((i (match-encoded string start end)))
      (and i
	   (let ((string (decode string start i)))
	     (let ((end (string-length string)))
	       (let ((pv (parse-substring parse-decoded string 0 end)))
		 (and pv
		      (cons i (cdr pv))))))))))

(define (encapsulating-parser parser transformer keyword)
  (lambda (string start end)
    (let ((pv (parser string start end)))
      (and pv
	   (list (car pv) (cons keyword (transformer pv)))))))

(define (predicated-parser parser predicate)
  (lambda (string start end)
    (let ((pv (parser string start end)))
      (and pv
	   (predicate pv)
	   pv))))

(define (list-parser match-element match-delimiter keyword)
  (lambda (string start end)
    (let ((index (match-element string start end)))
      (if index
	  (let loop
	      ((start index)
	       (elements (list (substring string start index))))
	    (let ((index (match-delimiter string start end)))
	      (if index
		  (let ((index* (match-element string index end)))
		    (if index*
			(loop index*
			      (cons (substring string index index*) elements))
			(list start (cons keyword (reverse! elements)))))
		  (list start (cons keyword (reverse! elements))))))
	  (list start (list keyword))))))

(define (optional-parser . parsers)
  (let ((parse (apply sequence-parser parsers)))
    (lambda (string start end)
      (or (parse string start end)
	  (list start)))))

(define (sequence-parser . parsers)
  (if (pair? parsers)
      (if (pair? (cdr parsers))
	  (lambda (string start end)
	    (let loop ((parsers parsers) (start start))
	      (let ((pv1 ((car parsers) string start end)))
		(and pv1
		     (if (pair? (cdr parsers))
			 (let ((pv2 (loop (cdr parsers) (car pv1))))
			   (and pv2
				(cons (car pv2) (append (cdr pv1) (cdr pv2)))))
			 pv1)))))
	  (car parsers))
      parse-always))

(define (alternatives-parser . parsers)
  (if (pair? parsers)
      (if (pair? (cdr parsers))
	  (lambda (string start end)
	    (let loop ((parsers parsers))
	      (or ((car parsers) string start end)
		  (and (pair? (cdr parsers))
		       (loop (cdr parsers))))))
	  (car parsers))
      parse-never))

;;;; Matcher language

;;; A matcher is a procedure that accepts a substring as three
;;; arguments and returns one of two values.  If the matcher
;;; successfully matches the substring, it returns an index into the
;;; substring indicating how much of the substring was matched.  If
;;; the matcher fails, it returns #F.

(define (match-never string start end)
  string start end
  #f)

(define (match-always string start end)
  string end
  start)

(define (rexp-matcher pattern)
  (let ((pattern (rexp-compile pattern)))
    (lambda (string start end)
      (let ((regs (re-substring-match pattern string start end)))
	(and regs
	     (re-match-end-index 0 regs))))))

(define (string-matcher pattern)
  (let ((pl (string-length pattern)))
    (lambda (string start end)
      (and (substring-prefix? pattern 0 pl string start end)
	   (fix:+ start pl)))))

(define (ci-string-matcher pattern)
  (let ((pl (string-length pattern)))
    (lambda (string start end)
      (and (substring-prefix-ci? pattern 0 pl string start end)
	   (fix:+ start pl)))))

(define (optional-matcher . matchers)
  (let ((matcher (apply sequence-matcher matchers)))
    (lambda (string start end)
      (or (matcher string start end)
	  start))))

(define (alternatives-matcher . matchers)
  (if (pair? matchers)
      (if (pair? (cdr matchers))
	  (lambda (string start end)
	    (let loop ((matchers matchers))
	      (or ((car matchers) string start end)
		  (and (pair? (cdr matchers))
		       (loop (cdr matchers))))))
	  (car matchers))
      match-never))

(define (sequence-matcher . matchers)
  (if (pair? matchers)
      (if (pair? (cdr matchers))
	  (lambda (string start end)
	    (let loop ((matchers matchers) (start start))
	      (let ((i ((car matchers) string start end)))
		(and i
		     (if (pair? (cdr matchers))
			 (loop (cdr matchers) i)
			 i)))))
	  (car matchers))
      match-always))

(define (*-matcher . matchers)
  (let ((matcher (apply sequence-matcher matchers)))
    (lambda (string start end)
      (let loop ((start start))
	(let ((i (matcher string start end)))
	  (if i
	      (loop i)
	      start))))))

(define (+-matcher . matchers)
  (let ((matcher (apply sequence-matcher matchers)))
    (sequence-matcher matcher (*-matcher matcher))))