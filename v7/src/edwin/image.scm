;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/image.scm,v 1.127 1991/03/22 00:31:53 cph Exp $
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

(define (string-line-image string column tab-width)
  (substring-line-image string 0 (string-length string) column tab-width))

(define (substring-line-image string start end column tab-width)
  (let ((i&c (substring-line-columns string start end column tab-width)))
    (let ((end (car i&c)))
      (let ((image (make-string (fix:- (cdr i&c) column))))
	(%substring-image string start end column tab-width image 0)
	(cons image end)))))

(define (string-image string column tab-width)
  (substring-image string 0 (string-length string) column tab-width))

(define (substring-image string start end column tab-width)
  (let ((image
	 (make-string
	  (fix:- (substring-columns string start end column tab-width)
		 column))))
    (%substring-image string start end column tab-width image 0)
    image))

(define (%substring-image string start end column tab-width image start-image)
  (let loop ((string-index start) (image-index start-image))
    (if (not (fix:= string-index end))
	(loop
	 (fix:+ string-index 1)
	 (let ((ascii (vector-8b-ref string string-index)))
	   (cond ((fix:< ascii #o040)
		  (if (and tab-width (fix:= ascii (char->integer #\tab)))
		      (let ((n
			     (fix:- tab-width
				    (fix:remainder (fix:+ image-index column)
						   tab-width))))
			(let ((end (fix:+ image-index n)))
			  (do ((image-index image-index
					    (fix:+ image-index 1)))
			      ((fix:= image-index end) image-index)
			    (string-set! image image-index #\space))))
		      (begin
			(string-set! image image-index #\^)
			(vector-8b-set! image
					(fix:+ image-index 1)
					(fix:+ ascii #o100))
			(fix:+ image-index 2))))
		 ((fix:< ascii #o177)
		  (vector-8b-set! image image-index ascii)
		  (fix:+ image-index 1))
		 ((fix:= ascii #o177)
		  (string-set! image image-index #\^)
		  (string-set! image image-index #\?)
		  (fix:+ image-index 2))
		 (else
		  (string-set! image image-index #\\)
		  (let ((q (fix:quotient ascii 8)))
		    (vector-8b-set! image
				    (fix:+ image-index 1)
				    (fix:+ (fix:quotient q 8)
					   (char->integer #\0)))
		    (vector-8b-set! image
				    (fix:+ image-index 2)
				    (fix:+ (fix:remainder q 8)
					   (char->integer #\0))))
		  (vector-8b-set! image
				  (fix:+ image-index 3)
				  (fix:+ (fix:remainder ascii 8)
					 (char->integer #\0)))
		  (fix:+ image-index 4))))))))

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

(define (group-line-image group start end column tab-width)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-end (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-line-image text start end column tab-width))
	  ((fix:<= gap-start start)
	   (let ((image&index
		  (substring-line-image text
					(fix:+ start gap-length)
					(fix:+ end gap-length)
					column
					tab-width)))
	     (cons (car image&index) (fix:- (cdr image&index) gap-length))))
	  (else
	   (let ((index&column
		  (substring-line-columns text start gap-start
					  column tab-width)))
	     (let ((end-1 (car index&column))
		   (column-1 (cdr index&column)))
	       (if (fix:= end-1 gap-start)
		   (let ((index&column
			  (substring-line-columns text
						  gap-end
						  (fix:+ end gap-length)
						  column-1
						  tab-width)))
		     (let ((end-2 (car index&column))
			   (column-2 (cdr index&column)))
		       (let ((image (make-string (fix:- column-2 column))))
			 (%substring-image text start end-1
					   column tab-width
					   image 0)
			 (%substring-image text gap-end end-2
					   column tab-width
					   image (fix:- column-1 column))
			 (cons image (fix:- end-2 gap-length)))))
		   (let ((image (make-string (fix:- column-1 column))))
		     (%substring-image text start end-1
				       column tab-width
				       image 0)
		     (cons image end-1)))))))))

(define (group-image group start end column tab-width)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-end (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-image text start end column tab-width))
	  ((fix:<= gap-start start)
	   (substring-image text
			    (fix:+ start gap-length)
			    (fix:+ end gap-length)
			    column
			    tab-width))
	  (else
	   (let ((column-1
		  (substring-columns text start gap-start
				     column tab-width))
		 (end (fix:+ end gap-length)))
	     (let ((image
		    (make-string
		     (fix:- (substring-columns text gap-end end
					       column-1 tab-width)
			    column))))
	       (%substring-image text start gap-start column tab-width
				 image 0)
	       (%substring-image text gap-end end column tab-width
				 image (fix:- column-1 column))
	       image))))))