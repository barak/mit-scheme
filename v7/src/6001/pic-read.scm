#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/6001/pic-read.scm,v 1.3 1992/07/10 21:56:55 cph Exp $

Copyright (c) 1991-92 Massachusetts Institute of Technology

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

;;;; Procedures to read a file in raw pgm format into a picture

(declare (usual-integrations))

(define (pgm-file->picture filename)
  (call-with-input-file (standard-pathname filename "pgm")
    (lambda (port)
      (get-body port (get-header port)))))

(define (standard-pathname filename type)
  (let ((pathname (->pathname filename)))
    (if (or (pathname-type pathname)
	    (file-exists? pathname))
	pathname
	(pathname-new-type pathname type))))

(define (get-header port)
  (let* ((type (get-line port))
	 (dims (get-line port))
	 (no-of-greys (string->number (get-line port)))
	 (spc-index (string-find-next-char dims #\space)))
    (if (not (equal? type "P5"))  ; P5 is the magic number for raw PGM format
	(error "Unrecognized format (Convert to raw PGM)."))
    (vector type
	    (string->number (string-head dims spc-index))
	    (string->number (string-tail dims (+ spc-index 1)))
	    no-of-greys)))

(define get-line
  (let ((delimiters (char-set #\newline)))
    (lambda (port)
      (let loop ()
	(let ((line (read-string delimiters port)))
	  (if (eof-object? line)
	      (error "EOF encountered when parsing line."))
	  ;; ignore comments
	  (if (and (not (string-null? line))
		   (char=? #\# (string-ref line 0)))
	      (loop)
	      line))))))

(define (get-body port attributes)
  (let* ((length (vector-ref attributes 1))
	 (width (vector-ref attributes 2))
	 (pic (make-picture length width))
	 (data
	  (make-initialized-vector
	   width
	   (lambda (index)
	     index			; ignored
	     (make-floating-vector length 0.))))) ;initialize to blank
    (side-effecting-iter
     width
     (lambda (n)
       (let ((nth-row (vector-ref data (- width n 1))))
	 (side-effecting-iter
	  length
	  (lambda (m)
	    (floating-vector-set!
	     nth-row
	     m
	     (exact->inexact (char->ascii (read-char port)))))))))
    (picture-set-data! pic data)
    pic))

;;; Procedure to read in a picture that was previously saved using
;;; picture-write.

(define (picture-read filename)
  (let ((pic-mimic (fasload (standard-pathname filename "pic"))))
    (if (not (record? pic-mimic))
	(error "Object loaded is not a record:" pic-mimic))
    (let ((mimic-type (record-type-descriptor pic-mimic)))
      (if (not (equal? (record-type-field-names mimic-type)
		       (record-type-field-names picture-type)))
	  (error "Object loaded is not a picture:" pic-mimic))
      (let ((new-pic
	     (make-picture ((record-accessor mimic-type 'width) pic-mimic)
			   ((record-accessor mimic-type 'height) pic-mimic))))
	(picture-set-data! new-pic
			   ((record-accessor mimic-type 'data) pic-mimic))
	(%picture-set-min! new-pic
			   ((record-accessor mimic-type 'min) pic-mimic))
	(%picture-set-max! new-pic
			   ((record-accessor mimic-type 'max) pic-mimic))
	new-pic))))