#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/system.scm,v 14.8 1991/11/04 20:30:06 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

;;;; Systems
;;; package: (runtime system)

(declare (usual-integrations))

(define (add-identification! name version modification)
  (add-system! (make-system name version modification '())))

(define-structure (system
		   (constructor
		    make-system
		    (name version modification files-lists))
		   (conc-name system/))
  (name false read-only true)
  (version false)
  (modification false)
  (files-lists false read-only true)
  (files false))

(define known-systems '())

(define (add-system! system)
  (set! known-systems (append! known-systems (list system)))
  unspecific)

(define (for-each-system! procedure)
  (for-each procedure known-systems))

(define (system/identification-string system)
  (string-append
   (system/name system)
   (let ((version
	  (string-append
	   (version->string (system/version system))
	   (let ((modification (version->string (system/modification system))))
	     (if (string-null? modification)
		 ""
		 (string-append "." modification))))))
     (if (string-null? version)
	 ""
	 (string-append " " version)))))

(define (version->string version)
  (cond ((string? version) version)
	((exact-nonnegative-integer? version) (number->string version))
	((null? version) "")
	((list? version)
	 (let loop ((version version))
	   (if (null? (cdr version))
	       (version->string (car version))
	       (string-append (version->string (car version))
			      "."
			      (loop (cdr version))))))
	(else
	 (error "Illegal system version" version))))

;;; Load the given system.

;;; SYSTEM/FILES will be assigned the list of filenames actually
;;; loaded.

;;; SYSTEM/FILES-LISTS should contain a list of pairs, the car of each
;;; pair being an environment, and the cdr a list of filenames.  The
;;; files are loaded in the order specified, into the environments
;;; specified.  COMPILED?, if false, means change all of the file
;;; types to "BIN".

(define (load-system! system #!optional compiled?)
  (let ((files
	 (format-files-list (system/files-lists system)
			    (if (default-object? compiled?)
				(prompt-for-confirmation "Load compiled")
				compiled?))))
    (set-system/files! system
		       (map (lambda (file) (->namestring (car file))) files))
    (for-each (lambda (file scode)
		(newline) (write-string "Eval ")
		(write (->namestring (car file)))
		(scode-eval scode (cdr file)))
	      files
	      (let loop ((files (map car files)))
		(if (null? files)
		    '()
		    (split-list files 20
		      (lambda (head tail)
			(let ((expressions (map fasload head)))
			  (newline)
			  (write-string "Purify")
			  (purify (list->vector expressions) true)
			  (append! expressions (loop tail))))))))
    (newline)
    (write-string "Done"))
  (add-system! system)
  unspecific)

(define (split-list list n receiver)
  (if (or (not (pair? list)) (zero? n))
      (receiver '() list)
      (split-list (cdr list) (-1+ n)
	(lambda (head tail)
	  (receiver (cons (car list) head) tail)))))

(define (format-files-list files-lists compiled?)
  (append-map! (lambda (files-list)
		 (map (lambda (filename)
			(let ((pathname (->pathname filename)))
			  (cons (if (and (not compiled?)
					 (equal? "com"
						 (pathname-type pathname)))
				    (pathname-new-type pathname "bin")
				    pathname)
				(car files-list))))
		      (cdr files-list)))
	       files-lists))