;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/image.scm,v 1.128 1991/04/01 10:07:13 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Display Imaging

(declare (usual-integrations))

(define (string-line-columns string column tab-width)
  (substring-line-columns string 0 (string-length string) column tab-width))

(define (substring-line-columns string start end column tab-width)
  (if tab-width
      (let loop ((index start) (column column))
	(if (fix:= index end)
	    (cons index column)
	    (let ((ascii (vector-8b-ref string index)))
	      (if (fix:= ascii (char->integer #\newline))
		  (cons index column)
		  (loop (fix:+ index 1)
			(fix:+ column
			       (if (fix:= ascii (char->integer #\tab))
				   (fix:- tab-width
					  (fix:remainder column tab-width))
				   (vector-ref char-image-lengths ascii))))))))
      (let loop ((index start) (column column))
	(if (fix:= index end)
	    (cons index column)
	    (let ((ascii (vector-8b-ref string index)))
	      (if (fix:= ascii (char->integer #\newline))
		  (cons index column)
		  (loop (fix:+ index 1)
			(fix:+ column
			       (vector-ref char-image-lengths ascii)))))))))

(define (string-columns string column tab-width)
  (substring-columns string 0 (string-length string) column tab-width))

(define (substring-columns string start end column tab-width)
  (if tab-width
      (do ((index start (fix:+ index 1))
	   (column column
		   (fix:+ column
			  (let ((ascii (vector-8b-ref string index)))
			    (if (fix:= ascii (char->integer #\tab))
				(fix:- tab-width
				       (fix:remainder column tab-width))
				(vector-ref char-image-lengths ascii))))))
	  ((fix:= index end) column))
      (do ((index start (fix:+ index 1))
	   (column column
		   (fix:+ column
			  (vector-ref char-image-lengths
				      (vector-8b-ref string index)))))
	  ((fix:= index end) column))))

(define-integrable (substring-column->index string start end start-column
					    tab-width column)
  (car (%substring-column->index string start end start-column tab-width
				 column)))

(define (%substring-column->index string start end start-column tab-width
				  column)
  ;; If COLUMN falls in the middle of a multi-column character, the
  ;; index returned is that of the character.  Thinking of the index
  ;; as a pointer between characters, the value is the pointer to the
  ;; left of the multi-column character.  Only if COLUMN reaches
  ;; across the character will the right-hand pointer be returned.
  ;; Various things depend on this.
  (if tab-width
      (let loop ((index start) (c start-column))
	(if (or (fix:= c column) (fix:= index end))
	    (cons index c)
	    (let ((c
		   (fix:+ c
			  (let ((ascii (vector-8b-ref string index)))
			    (if (fix:= ascii (char->integer #\tab))
				(fix:- tab-width (fix:remainder c tab-width))
				(vector-ref char-image-lengths ascii))))))
	      (if (fix:> c column)
		  (cons index c)
		  (loop (fix:+ index 1) c)))))
      (let loop ((index start) (c start-column))
	(if (or (fix:= c column) (fix:= index end))
	    (cons index c)
	    (let ((c
		   (fix:+ c
			  (vector-ref char-image-lengths
				      (vector-8b-ref string index)))))
	      (if (fix:> c column)
		  (cons index c)
		  (loop (fix:+ index 1) c)))))))

(define-integrable char-image-lengths
  '#(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2
     4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
     4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
     4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
     4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4))

(define (group-line-columns group start end column tab-width)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-end (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-line-columns text start end column tab-width))
	  ((fix:<= gap-start start)
	   (let ((i&c
		  (substring-line-columns text
					  (fix:+ start gap-length)
					  (fix:+ end gap-length)
					  column
					  tab-width)))
	     (cons (fix:- (car i&c) gap-length) (cdr i&c))))
	  (else
	   (let ((i&c
		  (substring-line-columns text start gap-start
					  column tab-width)))
	     (if (fix:< (car i&c) gap-start)
		 i&c
		 (let ((i&c
			(substring-line-columns text
						gap-end
						(fix:+ end gap-length)
						(cdr i&c)
						tab-width)))
		   (cons (fix:- (car i&c) gap-length) (cdr i&c)))))))))

(define (group-columns group start end column tab-width)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-end (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-columns text start end column tab-width))
	  ((fix:<= gap-start start)
	   (substring-columns text
			      (fix:+ start gap-length)
			      (fix:+ end gap-length)
			      column
			      tab-width))
	  (else
	   (substring-columns text
			      gap-end
			      (fix:+ end gap-length)
			      (substring-columns text start gap-start
						 column tab-width)
			      tab-width)))))

(define (group-column->index group start end start-column column tab-width)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-end (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-column->index text start end start-column tab-width
				    column))
	  ((fix:<= gap-start start)
	   (fix:- (substring-column->index text
					   (fix:+ start gap-length)
					   (fix:+ end gap-length)
					   start-column
					   tab-width
					   column)
		  gap-length))
	  (else
	   (let ((i&c
		  (%substring-column->index text start gap-start
					    start-column tab-width column)))
	     (if (fix:< (cdr i&c) column)
		 (fix:- (substring-column->index text gap-end
						 (fix:+ end gap-length)
						 (cdr i&c) tab-width column)
			gap-length)
		 (car i&c)))))))

(define (substring-image! string string-start string-end
			  image image-start image-end
			  tab-width column-offset results)
  (let loop ((string-index string-start) (image-index image-start))
    (if (or (fix:= image-index image-end)
	    (fix:= string-index string-end))
	(begin
	  (vector-set! results 0 string-index)
	  (vector-set! results 1 image-index)
	  (vector-set! results 2 0))
	(let ((ascii (vector-8b-ref string string-index))
	      (partial
	       (lambda (partial)
		 (vector-set! results 0 string-index)
		 (vector-set! results 1 image-end)
		 (vector-set! results 2 partial))))
	  (cond ((fix:< ascii #o040)
		 (if (and (fix:= ascii (char->integer #\tab)) tab-width)
		     (let ((n
			    (fix:- tab-width
				   (fix:remainder (fix:+ column-offset
							 image-index)
						  tab-width))))
		       (let ((end (fix:+ image-index n)))
			 (if (fix:<= end image-end)
			     (begin
			       (do ((image-index image-index
						 (fix:+ image-index 1)))
				   ((fix:= image-index end))
				 (string-set! image image-index #\space))
			       (loop (fix:+ string-index 1) end))
			     (begin
			       (do ((image-index image-index
						 (fix:+ image-index 1)))
				   ((fix:= image-index image-end))
				 (string-set! image image-index #\space))
			       (partial (fix:- end image-end))))))
		     (begin
		       (string-set! image image-index #\^)
		       (if (fix:= (fix:+ image-index 1) image-end)
			   (partial 1)
			   (begin
			     (vector-8b-set! image
					     (fix:+ image-index 1)
					     (fix:+ ascii #o100))
			     (loop (fix:+ string-index 1)
				   (fix:+ image-index 2)))))))
		((fix:< ascii #o177)
		 (vector-8b-set! image image-index ascii)
		 (loop (fix:+ string-index 1) (fix:+ image-index 1)))
		((fix:= ascii #o177)
		 (string-set! image image-index #\^)
		 (if (fix:= (fix:+ image-index 1) image-end)
		     (partial 1)
		     (begin
		       (string-set! image (fix:+ image-index 1) #\?)
		       (loop (fix:+ string-index 1) (fix:+ image-index 2)))))
		(else
		 (string-set! image image-index #\\)
		 (let ((q (fix:quotient ascii 8)))
		   (let ((d1 (fix:+ (fix:quotient q 8) (char->integer #\0)))
			 (d2 (fix:+ (fix:remainder q 8) (char->integer #\0)))
			 (d3
			  (fix:+ (fix:remainder ascii 8) (char->integer #\0))))
		     (cond ((fix:<= (fix:+ image-index 4) image-end)
			    (vector-8b-set! image (fix:+ image-index 1) d1)
			    (vector-8b-set! image (fix:+ image-index 2) d2)
			    (vector-8b-set! image (fix:+ image-index 3) d3)
			    (loop (fix:+ string-index 1)
				  (fix:+ image-index 4)))
			   ((fix:= (fix:+ image-index 1) image-end)
			    (partial 3))
			   ((fix:= (fix:+ image-index 2) image-end)
			    (vector-8b-set! image (fix:+ image-index 1) d1)
			    (partial 2))
			   (else
			    (vector-8b-set! image (fix:+ image-index 1) d1)
			    (vector-8b-set! image (fix:+ image-index 2) d2)
			    (partial 1)))))))))))

(define (string-image string start-column tab-width)
  (substring-image string 0 (string-length string) start-column tab-width))

(define (substring-image string start end start-column tab-width)
  (let ((columns
	 (fix:- (substring-columns string start end start-column tab-width)
		start-column)))
    (let ((image (make-string columns)))
      (substring-image! string start end
			image 0 columns
			tab-width start-column substring-image-results)
      image)))

(define substring-image-results
  (make-vector 3))

(define (group-image! group start end
		      image image-start image-end
		      tab-width column-offset results)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-end (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-image! text start end
			     image image-start image-end
			     tab-width column-offset results))
	  ((fix:<= gap-start start)
	   (substring-image! text
			     (fix:+ start gap-length) (fix:+ end gap-length)
			     image image-start image-end
			     tab-width column-offset results)
	   (vector-set! results 0 (fix:- (vector-ref results 0) gap-length)))
	  (else
	   (substring-image! text start gap-start
			     image image-start image-end
			     tab-width column-offset results)
	   (if (fix:< (vector-ref results 1) image-end)
	       (begin
		 (substring-image! text gap-end (fix:+ end gap-length)
				   image (vector-ref results 1) image-end
				   tab-width column-offset results)
		 (vector-set! results 0
			      (fix:- (vector-ref results 0) gap-length))))))))

(define (partial-image! char n image image-start image-end tab-width)
  ;; Assume that (< IMAGE-START IMAGE-END) and that N is less than the
  ;; total width of the image for the character.
  (let ((ascii (char->integer char)))
    (cond ((fix:< ascii #o040)
	   (if (and (fix:= ascii (char->integer #\tab)) tab-width)
	       (let ((end
		      (let ((end (fix:+ image-start n)))
			(if (fix:< end image-end) end image-end))))
		 (do ((image-index image-start (fix:+ image-index 1)))
		     ((fix:= image-index end))
		   (string-set! image image-index #\space)))
	       (vector-8b-set! image image-start (fix:+ ascii #o100))))
	  ((fix:= ascii #o177)
	   (string-set! image image-start #\?))
	  (else
	   (let ((q (fix:quotient ascii 8)))
	     (let ((d1 (fix:+ (fix:quotient q 8) (char->integer #\0)))
		   (d2 (fix:+ (fix:remainder q 8) (char->integer #\0)))
		   (d3 (fix:+ (fix:remainder ascii 8) (char->integer #\0))))
	       (case n
		 ((1)
		  (vector-8b-set! image image-start d3))
		 ((2)
		  (vector-8b-set! image image-start d2)
		  (if (fix:< (fix:+ image-start 1) image-end)
		      (vector-8b-set! image (fix:+ image-start 1) d3)))
		 (else
		  (vector-8b-set! image image-start d1)
		  (if (fix:< (fix:+ image-start 1) image-end)
		      (vector-8b-set! image (fix:+ image-start 1) d2))
		  (if (fix:< (fix:+ image-start 2) image-end)
		      (vector-8b-set! image (fix:+ image-start 2) d3))))))))))