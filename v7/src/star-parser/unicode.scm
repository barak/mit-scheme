;;; -*-Scheme-*-
;;;
;;; $Id: unicode.scm,v 1.3 2001/07/12 03:53:02 cph Exp $
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

;;;; Unicode support

;;; UTF-8 encoding:
;;;
;;;  max code  encoding
;;; ---------- -----------------------------------------------------
;;; #x00000080 0xxxxxxx
;;; #x00000800 110xxxxx 10xxxxxx
;;; #x00010000 1110xxxx 10xxxxxx 10xxxxxx
;;; #x00200000 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
;;; #x04000000 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
;;; #x80000000 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx

(declare (usual-integrations))

(define-structure (alphabet (type-descriptor alphabet-rtd))
  (low #f read-only #t)
  (high1 #f read-only #t)
  (high2 #f read-only #t))

(define-integrable (make-alphabet-low)
  (make-string #x100 (integer->char 0)))

(define-integrable (alphabet-low-ref low code-point)
  (not (fix:= (fix:and (vector-8b-ref low (fix:lsh code-point -3))
		       (fix:lsh 1 (fix:and code-point 7)))
	      0)))

(define-integrable (alphabet-low-set! low code-point)
  (vector-8b-set! low
		  (fix:lsh code-point -3)
		  (fix:or (vector-8b-ref low (fix:lsh code-point -3))
			  (fix:lsh 1 (fix:and code-point 7)))))

(define null-alphabet
  (make-alphabet (make-alphabet-low) '#() '#()))

(define (unicode-code-point? n)
  (and (exact-nonnegative-integer? n)
       (< n #x80000000)))

(define (code-point-in-alphabet? n alphabet)
  (if (not (unicode-code-point? n))
      (error:wrong-type-argument n "unicode code point"
				 'CODE-POINT-IN-ALPHABET?))
  (if (not (alphabet? alphabet))
      (error:wrong-type-argument alphabet "unicode alphabet"
				 'CODE-POINT-IN-ALPHABET?))
  (if (< n #x800)
      (alphabet-low-ref (alphabet-low alphabet) n)
      (let ((high1 (alphabet-high1 alphabet))
	    (high2 (alphabet-high2 alphabet)))
	(let loop ((lower 0) (upper (vector-length high1)))
	  (and (fix:< lower upper)
	       (let ((index (fix:quotient (fix:+ lower upper) 2)))
		 (cond ((< n (vector-ref high1 index))
			(loop lower index))
		       ((< (vector-ref high2 index) n)
			(loop (fix:+ index 1) upper))
		       (else #t))))))))

(define (char-in-alphabet? char alphabet)
  (code-point-in-alphabet? (char-code char) alphabet))

(define (code-points->alphabet items)
  (if (not (well-formed-code-points-list? items))
      (error:wrong-type-argument items "code-points list"
				 'CODE-POINTS->ALPHABET))
  (call-with-values (lambda () (split-list items #x800))
    (lambda (low-items high-items)
      (let ((low (make-alphabet-low)))
	(for-each (lambda (item)
		    (if (pair? item)
			(do ((i (car item) (fix:+ i 1)))
			    ((fix:> i (cdr item)))
			  (alphabet-low-set! low i))
			(alphabet-low-set! low item)))
		  low-items)
	(let ((n-high (length high-items)))
	  (let ((high1 (make-vector n-high))
		(high2 (make-vector n-high)))
	    (do ((items high-items (cdr items))
		 (i 0 (fix:+ i 1)))
		((not (pair? items)))
	      (if (pair? (car items))
		  (begin
		    (vector-set! high1 i (caar items))
		    (vector-set! high2 i (cdar items)))
		  (begin
		    (vector-set! high1 i (car items))
		    (vector-set! high2 i (car items)))))
	    (make-alphabet low high1 high2)))))))

(define (split-list items limit)
  (let loop ((items items) (low '()))
    (if (pair? items)
	(let ((item (car items)))
	  (cond ((not (pair? item))
		 (if (< item limit)
		     (loop (cdr items) (cons item low))
		     (values low items)))
		((< (cdr item) limit)
		 (loop (cdr items) (cons item low)))
		((<= limit (car item))
		 (values low items))
		(else
		 (values (cons (cons (car item) (- limit 1)) low)
			 (cons (cons limit (cdr item)) items)))))
	(values low '()))))

(define (well-formed-code-points-list? items)
  (or (not (pair? items))
      (and (well-formed-item? (car items))
	   (let loop ((a (car items)) (items (cdr items)))
	     (or (not (pair? items))
		 (let ((b (car items))
		       (items (cdr items)))
		   (and (well-formed-item? b)
			(< (if (pair? a) (cdr a) a)
			   (if (pair? b) (car b) b))
			(loop b items))))))))

(define (well-formed-item? item)
  (if (pair? item)
      (and (unicode-code-point? (car item))
	   (unicode-code-point? (cdr item))
	   (< (car item) (cdr item)))
      (unicode-code-point? item)))

(define (char-set->alphabet char-set)
  (let ((low (make-alphabet-low)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i #x100))
      (if (char-set-member? char-set (integer->char i))
	  (alphabet-low-set! low i)))
    (make-alphabet low '#() '#())))

(define (alphabet->char-set alphabet)
  (predicate->char-set (lambda (char) (char-in-alphabet? char alphabet))))

(define (string->alphabet string)
  (if (not (string? string))
      (error:wrong-type-argument string "string" 'STRING->ALPHABET))
  (let ((n (string-length string))
	(low (make-alphabet-low)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n))
      (alphabet-low-set! low (vector-8b-ref string i)))
    (make-alphabet low '#() '#())))

(define (alphabet->string alphabet)
  (let loop ((i 0) (chars '()))
    (if (fix:< i #x100)
	(loop (fix:+ i 1)
	      (if (code-point-in-alphabet? i alphabet)
		  (cons (integer->char i) chars)
		  chars))
	(apply string (reverse! chars)))))

(define (8-bit-alphabet? alphabet)
  (and (fix:= (vector-length (alphabet-high1 alphabet)) 0)
       (let ((low (alphabet-low alphabet)))
	 (let loop ((i #x20))
	   (or (fix:= i #x100)
	       (and (fix:= (vector-8b-ref low i) 0)
		    (loop (fix:+ i 1))))))))

(define (alphabet->code-points alphabet)
  (append! (alphabet-low->code-points (alphabet-low alphabet))
	   (alphabet-high->code-points (alphabet-high1 alphabet)
				       (alphabet-high2 alphabet))))

(define (alphabet-low->code-points low)
  (let find-lower ((i 0) (result '()))
    (if (fix:< i #x800)
	(if (alphabet-low-ref low i)
	    (let ((lower i))
	      (let find-upper ((i (fix:+ i 1)))
		(if (fix:< i #x800)
		    (if (alphabet-low-ref low i)
			(find-upper (fix:+ i 1))
			(find-lower i
				    (cons (if (fix:= lower (fix:- i 1))
					      lower
					      (cons lower (fix:- i 1)))
					  result)))
		    (reverse!
		     (cons (if (fix:= lower (fix:- i 1))
			       lower
			       (cons lower (fix:- i 1)))
			   result)))))
	    (find-lower (fix:+ i 1) result))
	(reverse! result))))

(define (alphabet-high->code-points lower upper)
  (let ((n (vector-length lower)))
    (let loop ((i 0) (result '()))
      (if (fix:< i n)
	  (loop (fix:+ i 1)
		(cons (cons (vector-ref lower i) (vector-ref upper i))
		      result))
	  (reverse! result)))))

(define (alphabet+ . alphabets)
  (for-each (lambda (alphabet)
	      (if (not (alphabet? alphabet))
		  (error:wrong-type-argument alphabet "unicode alphabet"
					     'ALPHABET+)))
	    alphabets)
  (reduce alphabet+2 null-alphabet alphabets))

(define (alphabet+2 a1 a2)
  (call-with-values
      (lambda ()
	(alphabet-high+2 (alphabet-high1 a1)
			 (alphabet-high2 a1)
			 (alphabet-high1 a2)
			 (alphabet-high2 a2)))
    (lambda (high1 high2)
      (make-alphabet (alphabet-low+2 (alphabet-low a1) (alphabet-low a2))
		     high1
		     high2))))

(define (alphabet-low+2 low1 low2)
  (let ((low (make-alphabet-low)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i #x100))
      (vector-8b-set! low i
		      (fix:or (vector-8b-ref low1 i)
			      (vector-8b-ref low2 i))))
    low))

(define (alphabet-high+2 lower1 upper1 lower2 upper2)
  (let ((n1 (vector-length lower1))
	(n2 (vector-length lower2)))
    (let ((lower (make-vector (fix:+ n1 n2)))
	  (upper (make-vector (fix:+ n1 n2))))
      (let ((n
	     (let loop ((i1 0) (i2 0) (i 0))
	       (cond ((fix:= i1 n1)
		      (subvector-move-left! lower2 i2 n2 lower i)
		      (subvector-move-left! upper2 i2 n2 upper i)
		      (fix:+ i (fix:- n2 i2)))
		     ((fix:= i2 n2)
		      (subvector-move-left! lower1 i1 n1 lower i)
		      (subvector-move-left! upper1 i1 n1 upper i)
		      (fix:+ i (fix:- n1 i1)))
		     ((< (vector-ref upper1 i1) (vector-ref lower2 i2))
		      (vector-set! lower i (vector-ref lower1 i1))
		      (vector-set! upper i (vector-ref upper1 i1))
		      (loop (fix:+ i1 1) i2 (fix:+ i 1)))
		     ((< (vector-ref upper2 i2) (vector-ref lower1 i1))
		      (vector-set! lower i (vector-ref lower2 i2))
		      (vector-set! upper i (vector-ref upper2 i2))
		      (loop i1 (fix:+ i2 1) (fix:+ i 1)))
		     (else
		      (vector-set! lower i
				   (min (vector-ref lower1 i1)
					(vector-ref lower2 i2)))
		      (vector-set! upper i
				   (max (vector-ref upper1 i1)
					(vector-ref upper2 i2)))
		      (loop (fix:+ i1 1) (fix:+ i2 1) (fix:+ i 1)))))))
	(if (fix:< n (vector-length lower))
	    (values (vector-head lower n) (vector-head upper n))
	    (values lower upper))))))

(define (alphabet- a1 a2)
  (call-with-values
      (lambda ()
	(alphabet-high- (alphabet-high1 a1)
			(alphabet-high2 a1)
			(alphabet-high1 a2)
			(alphabet-high2 a2)))
    (lambda (high1 high2)
      (make-alphabet (alphabet-low- (alphabet-low a1) (alphabet-low a2))
		     high1
		     high2))))

(define (alphabet-low- low1 low2)
  (let ((low (make-alphabet-low)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i #x100))
      (vector-8b-set! low i
		      (fix:and (vector-8b-ref low1 i)
			       (fix:not (vector-8b-ref low2 i)))))
    low))

(define (alphabet-high- lower1 upper1 lower2 upper2)
  (let ((n1 (vector-length lower1))
	(n2 (vector-length lower2)))
    (let ((lower (make-vector (fix:* n1 2)))
	  (upper (make-vector (fix:* n1 2))))
      (let ((n
	     (let loop ((i1 0) (i2 0) (i 0))
	       (cond ((fix:= i1 n1)
		      i)
		     ((fix:= i2 n2)
		      (subvector-move-left! lower1 i1 n1 lower i)
		      (subvector-move-left! upper1 i1 n1 upper i)
		      (fix:+ i (fix:- n1 i1)))
		     ((< (vector-ref upper1 i1) (vector-ref lower2 i2))
		      (vector-set! lower i (vector-ref lower1 i1))
		      (vector-set! upper i (vector-ref upper1 i1))
		      (loop (fix:+ i1 1) i2 (fix:+ i 1)))
		     ((< (vector-ref upper2 i2) (vector-ref lower1 i1))
		      (loop i1 (fix:+ i2 1) i))
		     ((< (vector-ref lower1 i1) (vector-ref lower2 i2))
		      (vector-set! lower i (vector-ref lower1 i1))
		      (vector-set! upper i (- (vector-ref lower2 i2) 1))
		      (if (<= (vector-ref upper1 i1) (vector-ref upper2 i2))
			  (loop (fix:+ i1 1) (fix:+ i2 1) (fix:+ i 1))
			  (begin
			    (vector-set! lower (fix:+ i 1)
					 (+ (vector-ref upper2 i2) 1))
			    (vector-set! upper (fix:+ i 1)
					 (vector-ref upper1 i1))
			    (loop (fix:+ i1 1) (fix:+ i2 1) (fix:+ i 2)))))
		     ((<= (vector-ref upper1 i1) (vector-ref upper2 i2))
		      (loop (fix:+ i1 1) (fix:+ i2 1) i))
		     (else
		      (vector-set! lower i (+ (vector-ref upper2 i2) 1))
		      (vector-set! upper i (vector-ref upper1 i1))
		      (loop (fix:+ i1 1) (fix:+ i2 1) (fix:+ i 1)))))))
	(if (fix:< n (vector-length lower))
	    (values (vector-head lower n) (vector-head upper n))
	    (values lower upper))))))

(define (code-point->utf8-string n)

  (define-integrable (initial-char n-bits offset)
    (fix:or (fix:and (fix:lsh #xFF (fix:+ n-bits 1)) #xFF)
	    (fix:lsh n (fix:- 0 offset))))

  (define-integrable (subsequent-char offset)
    (fix:or #x80
	    (fix:and (fix:lsh n (fix:- 0 offset)) #x3F)))

  (if (not (unicode-code-point? n))
      (error:wrong-type-argument n "unicode code point"
				 'CODE-POINT->UTF8-STRING))
  (cond ((< n #x00000080)
	 (let ((s (make-string 1)))
	   (vector-8b-set! s 0 n)
	   s))
	((< n #x00000800)
	 (let ((s (make-string 2)))
	   (vector-8b-set! s 0 (initial-char 5 6))
	   (vector-8b-set! s 1 (subsequent-char 0))
	   s))
	((< n #x00010000)
	 (let ((s (make-string 3)))
	   (vector-8b-set! s 0 (initial-char 4 12))
	   (vector-8b-set! s 1 (subsequent-char 6))
	   (vector-8b-set! s 2 (subsequent-char 0))
	   s))
	((< n #x00200000)
	 (let ((s (make-string 4)))
	   (vector-8b-set! s 0 (initial-char 3 18))
	   (vector-8b-set! s 1 (subsequent-char 12))
	   (vector-8b-set! s 2 (subsequent-char 6))
	   (vector-8b-set! s 3 (subsequent-char 0))
	   s))
	((< n #x04000000)
	 (let ((s (make-string 5)))
	   (vector-8b-set! s 0 (initial-char 2 24))
	   (vector-8b-set! s 1 (subsequent-char 18))
	   (vector-8b-set! s 2 (subsequent-char 12))
	   (vector-8b-set! s 3 (subsequent-char 6))
	   (vector-8b-set! s 4 (subsequent-char 0))
	   s))
	(else
	 (let ((s (make-string 6)))
	   (vector-8b-set! s 0 (initial-char 1 30))
	   (vector-8b-set! s 1 (subsequent-char 24))
	   (vector-8b-set! s 2 (subsequent-char 18))
	   (vector-8b-set! s 3 (subsequent-char 12))
	   (vector-8b-set! s 4 (subsequent-char 6))
	   (vector-8b-set! s 5 (subsequent-char 0))
	   s))))

(define (utf8-string->code-point string)

  (define-integrable (test2 index)
    (and (fix:<= #x80 (vector-8b-ref string index))
	 (fix:< (vector-8b-ref string index) #xC0)))

  (define-integrable (get2 index)
    (fix:and (vector-8b-ref string index) #x3F))

  (or (cond ((fix:= (string-length string) 0)
	     #f)
	    ((fix:< (vector-8b-ref string 0) #x80)
	     (and (fix:= (string-length string) 1)
		  (vector-8b-ref string 0)))
	    ((fix:< (vector-8b-ref string 0) #xE0)
	     (and (fix:= (string-length string) 2)
		  (test2 1)
		  (fix:or (fix:lsh (fix:and (vector-8b-ref string 0) #x1F) 6)
			  (get2 1))))
	    ((fix:< (vector-8b-ref string 0) #xF0)
	     (and (fix:= (string-length string) 3)
		  (test2 1)
		  (test2 2)
		  (fix:or (fix:lsh (fix:and (vector-8b-ref string 0) #x0F) 12)
			  (fix:or (fix:lsh (get2 1) 6)
				  (get2 2)))))
	    ((fix:< (vector-8b-ref string 0) #xF8)
	     (and (fix:= (string-length string) 4)
		  (test2 1)
		  (test2 2)
		  (test2 3)
		  (fix:or (fix:lsh (fix:and (vector-8b-ref string 0) #x07) 18)
			  (fix:or (fix:lsh (get2 1) 12)
				  (fix:or (fix:lsh (get2 2) 6)
					  (get2 3))))))
	    ((fix:< (vector-8b-ref string 0) #xFC)
	     (and (fix:= (string-length string) 5)
		  (test2 1)
		  (test2 2)
		  (test2 3)
		  (test2 4)
		  (+ (* (fix:and (vector-8b-ref string 0) #x03) #x01000000)
		     (fix:or (fix:lsh (get2 1) 18)
			     (fix:lsh (get2 2) 12))
		     (fix:or (fix:lsh (get2 3) 6)
			     (get2 4)))))
	    ((fix:< (vector-8b-ref string 0) #xFE)
	     (and (fix:= (string-length string) 6)
		  (test2 1)
		  (test2 2)
		  (test2 3)
		  (test2 4)
		  (test2 5)
		  (+ (* (fix:and (vector-8b-ref string 0) #x01) #x40000000)
		     (* (get2 1) #x01000000)
		     (fix:or (fix:lsh (get2 2) 18)
			     (fix:lsh (get2 3) 12))
		     (fix:or (fix:lsh (get2 4) 6)
			     (get2 5)))))
	    (else #f))
      (error:wrong-type-argument string "UTF-8 character"
				 'UTF8-STRING->CODE-POINT)))

(define (match-utf8-char-in-alphabet buffer alphabet)
  (let ((p (get-parser-buffer-pointer buffer)))
    (let ((n (read-utf8-code-point buffer p)))
      (and n
	   (if (code-point-in-alphabet? n alphabet)
	       #t
	       (begin
		 (set-parser-buffer-pointer! buffer p)
		 #f))))))

(define (read-utf8-code-point buffer p)
  (let ((c0 (read-parser-buffer-char buffer))
	(get-next
	 (lambda ()
	   (let ((c (read-parser-buffer-char buffer)))
	     (if (and c
		      (fix:<= #x80 (char->integer c))
		      (fix:< (char->integer c) #xC0))
		 (fix:and (char->integer c) #x3F)
		 (begin
		   (set-parser-buffer-pointer! buffer p)
		   #f))))))
    (and c0
	 (cond ((fix:< (char->integer c0) #x80)
		(char->integer c0))
	       ((fix:< (char->integer c0) #xE0)
		(let ((n1 (get-next)))
		  (and n1
		       (fix:or (fix:lsh (fix:and (char->integer c0) #x1F) 6)
			       n1))))
	       ((fix:< (char->integer c0) #xF0)
		(let* ((n1 (get-next))
		       (n2 (get-next)))
		  (and n1 n2
		       (fix:or (fix:lsh (fix:and (char->integer c0) #x0F) 12)
			       (fix:or (fix:lsh n1 6)
				       n2)))))
	       ((fix:< (char->integer c0) #xF8)
		(let* ((n1 (get-next))
		       (n2 (get-next))
		       (n3 (get-next)))
		  (and n1 n2 n3
		       (fix:or (fix:lsh (fix:and (char->integer c0) #x07) 18)
			       (fix:or (fix:lsh n1 12)
				       (fix:or (fix:lsh n2 6)
					       n3))))))
	       ((fix:< (char->integer c0) #xFC)
		(let* ((n1 (get-next))
		       (n2 (get-next))
		       (n3 (get-next))
		       (n4 (get-next)))
		  (and n1 n2 n3 n4
		       (+ (* (fix:and (char->integer c0) #x03) #x01000000)
			  (fix:or (fix:lsh n1 18)
				  (fix:lsh n2 12))
			  (fix:or (fix:lsh n3 6)
				  n4)))))
	       ((fix:< (char->integer c0) #xFE)
		(let* ((n1 (get-next))
		       (n2 (get-next))
		       (n3 (get-next))
		       (n4 (get-next))
		       (n5 (get-next)))
		  (and n1 n2 n3 n4 n5
		       (+ (* (fix:and (char->integer c0) #x01) #x40000000)
			  (* n1 #x01000000)
			  (fix:or (fix:lsh n2 18)
				  (fix:lsh n3 12))
			  (fix:or (fix:lsh n4 6)
				  n5)))))
	       (else
		(set-parser-buffer-pointer! buffer p)
		#f)))))