;;; -*-Scheme-*-
;;;
;;;	$Id: dirunx.scm,v 1.9 1995/01/31 21:38:17 cph Exp $
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

;;;; Directory Editor
;; package: (edwin dired)

(declare (usual-integrations))

(define (dired-change-inode program)
  (lambda (attribute argument)
    (dired-change-files (string-append "change" attribute "of") argument
      (let ((program (os/find-program program #f))
	    (directory (buffer-default-directory (current-buffer))))
	(lambda (pathname lstart)
	  (run-synchronous-process #f #f directory #f
				   program attribute (->namestring pathname))
	  (dired-redisplay pathname lstart))))))

(define-key 'dired #\M 'dired-chmod)
(define-command dired-chmod
  "Change mode of this file."
  "sChange to Mode\nP"
  (dired-change-inode "chmod"))

(define-key 'dired #\G 'dired-chgrp)
(define-command dired-chgrp
  "Change group of this file."
  "sChange to Group\nP"
  (dired-change-inode "chgrp"))

(define-key 'dired #\O 'dired-chown)
(define-command dired-chown
  "Change owner of this file."
  "sChange to Owner\nP"
  (dired-change-inode "chown"))

(define-key 'dired #\Z 'dired-do-compress)
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
		   (let ((decompress? (member type '("gz" "z" "Z"))))
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

(define (dired-change-files verb argument procedure)
  (let ((filenames
	 (if argument
	     (dired-next-files (command-argument-value argument))
	     (let ((files (dired-marked-files)))
	       (if (null? files)
		   (dired-next-files 1)
		   files)))))
    (if (null? filenames)
	(message "No files to " verb ".")
	(begin
	  (for-each (lambda (filename)
		      (set-cdr! filename
				(mark-right-inserting-copy (cdr filename))))
		    filenames)
	  (for-each (lambda (filename)
		      (procedure (car filename) (cdr filename))
		      (mark-temporary! (cdr filename)))
		    filenames)))
    (length filenames)))