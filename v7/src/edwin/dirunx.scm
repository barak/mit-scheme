;;; -*-Scheme-*-
;;;
;;; $Id: dirunx.scm,v 1.13 2001/06/02 22:28:49 cph Exp $
;;;
;;; Copyright (c) 1992-1999 Massachusetts Institute of Technology
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

;;;; Directory Editor (Unix Customizations)
;;; package: (edwin dired)

(declare (usual-integrations))

(define (dired-change-inode program)
  (lambda (attribute argument)
    (dired-change-files (string-append "change" attribute "of") argument
      (let ((program (os/find-program program #f (ref-variable exec-path)))
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
	     (let ((gzip (os/find-program "gzip" #f (ref-variable exec-path)))
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

#|
(define-command dired-do-symlink
  "Make symbolic links to current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new symbolic links are made in that directory
with the same names that the files currently have."
  "P"
  (lambda (argument)
    ))

(define-command dired-do-hardlink
  "Add names (hard links) current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new hard links are made in that directory
with the same names that the files currently have."
  "P"
  (lambda (argument)
    ))
|#