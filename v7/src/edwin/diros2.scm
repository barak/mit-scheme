;;; -*-Scheme-*-
;;;
;;;	$Id: diros2.scm,v 1.2 1995/10/31 08:08:33 cph Exp $
;;;
;;;	Copyright (c) 1992-95 Massachusetts Institute of Technology
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

;;;; Directory Editor (OS/2 Customizations)
;;; package: (edwin dired)

(declare (usual-integrations))

(define-key 'dired #\Z 'dired-do-compress)
(define-key 'dired #\S 'dired-hidden-toggle)
(define-key 'dired #\M 'dired-chmod)

(define-command dired-do-compress
  "Compress or uncompress marked (or next ARG) files.
The files are compressed or uncompressed using gzip."
  "P"
  (lambda (argument)
    (let ((n
	   (dired-change-files "compress" argument
	     (let ((gzip (os/find-program "gzip" #f))
		   (directory (buffer-default-directory (current-buffer))))
	       (lambda (pathname lstart)
		 (let ((type (pathname-type pathname))
		       (namestring (->namestring pathname)))
		   (let ((decompress? (equal? type "gz")))
		     (message (if decompress? "Unc" "C")
			      "ompressing file `" namestring "'...")
		     (run-synchronous-process #f #f directory #f
					      gzip
					      (if decompress? "-d" "")
					      namestring)
		     (dired-redisplay
		      (pathname-new-type
		       pathname
		       (and (not decompress?)
			    (if (string? type)
				(string-append type ".gz")
				"gz")))
		      lstart))))))))
      (if (positive? n)
	  (message "Compressed or uncompressed " n " files.")))))

(define-command dired-hidden-toggle
  "Toggle display of hidden/system files on and off."
  ()
  (lambda () (dired-toggle-switch #\a)))

(define-command dired-chmod
  "Change mode of this file."
  "sChange to Mode\nP"
  (lambda (spec argument)
    (call-with-values (lambda () (os2/parse-attributes-spec spec))
      (lambda (plus minus)
	(dired-change-files "change attributes of" argument
	  (lambda (pathname lstart)
	    (set-file-modes! pathname
			     (fix:or (fix:andc (file-modes pathname)
					       minus)
				     plus))
	    (dired-redisplay pathname lstart)))))))

(define (os2/parse-attributes-spec spec)
  (let ((end (string-length spec))
	(plus '())
	(minus '()))
    (let loop ((index 0) (state #f))
      (if (< index end)
	  (let ((char (char-downcase (string-ref spec index)))
		(index (+ index 1)))
	    (case char
	      ((#\+ #\-)
	       (loop index char))
	      ((#\a #\h #\r #\s)
	       (set! plus (delv! char plus))
	       (set! minus (delv! char minus))
	       (case state
		 ((#\+)
		  (set! plus (cons char plus))
		  (loop index state))
		 ((#\-)
		  (set! minus (cons char minus))
		  (loop index state))
		 (else #f)))
	      (else #f)))
	  (values (os2/attribute-letters-to-mask plus)
		  (os2/attribute-letters-to-mask minus))))))

(define (os2/attribute-letters-to-mask letters)
  (let ((mask 0))
    (for-each (lambda (letter)
		(set! mask
		      (fix:or (case letter
				((#\a) os2-file-mode/archived)
				((#\d) os2-file-mode/directory)
				((#\h) os2-file-mode/hidden)
				((#\r) os2-file-mode/read-only)
				((#\s) os2-file-mode/system)
				(else (error "Unknown mode letter:" letter)))
			      mask))
		unspecific)
	      letters)
    mask))