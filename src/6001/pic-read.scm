#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

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
	  (read-char port)
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
	     (flo:make-vector length 0.))))) ;initialize to blank
    (side-effecting-iter
     width
     (lambda (n)
       (let ((nth-row (vector-ref data (- width n 1))))
	 (side-effecting-iter
	  length
	  (lambda (m)
	    (flo:vector-set!
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
		       (record-type-field-names <picture>)))
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