#| -*-Scheme-*-

$Id: option.scm,v 14.36 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Option Loader
;;; package: (runtime options)

(declare (usual-integrations))

(define *initial-options-file* #F)

(define (initial-load-options)
  (or *initial-options-file*
      (get-environment-variable "MITSCHEME_LOAD_OPTIONS")
      (local-load-options)))

(define (local-load-options)
  (or (library-file? "optiondb")
      (standard-load-options)))

(define (standard-load-options)
  (or (library-file? "options/optiondb")
      (error "Cannot locate a load-option database")
      "optiondb"))

(define (define-load-option name . loaders)
  (set! *options* (cons (cons name loaders) *options*))
  unspecific)

(define (further-load-options place)
  (set! *parent* place)
  unspecific)

(define (load-option name #!optional no-error?)
  (let ((no-error? (and (not (default-object? no-error?)) no-error?)))

    (define (find-option)
      (cond ((assq name *options*) => load-entry)
	    ((force* *parent*)     => search-parent)
	    ((not no-error?)
	     (error "Unknown option name:" name)
	     #F)
	    (else  #F)))

    (define (load-entry entry)
      (for-each (lambda (thunk) (thunk)) (cdr entry))
      (set! loaded-options (cons name loaded-options))
      name)

    (define (search-parent file)
      (fluid-let ((*options* '())
		  (*parent*  #F))
	(fluid-let ((load/suppress-loading-message? #T))
	  (load-latest (merge-pathnames file (library-directory-pathname ""))
		       (make-load-environment)
		       system-global-syntax-table
		       #F))
	(find-option)))

    (define (make-load-environment)
      (eval '(LET () (THE-ENVIRONMENT)) system-global-environment))

    (fluid-let ((*parser-canonicalize-symbols?* #t))
      (if (not (memq name loaded-options))
	  (find-option)
	  name))))

(define loaded-options  '())
(define *options* '())			; Current options.
(define *parent*  initial-load-options)	; A thunk or a pathname/string or #F.

(define (library-file? library-internal-path)
  (let* ((library    (library-directory-pathname ""))
	 (pathname   (merge-pathnames library-internal-path library)))
    (let loop ((file-types load/default-types))
      (if (null? file-types)
	  #F
	  (let ((full-pathname (pathname-new-type pathname (caar file-types))))
	    (if (file-exists? full-pathname)
		pathname;; not full-pathname to allow load-latest
		(loop (cdr file-types))))))))

(define (force* value)
  (cond	((procedure? value)  (force* (value)))
	((promise? value)    (force* (force value)))
	(else value)))

(define (standard-option-loader package-name init-expression . files)
  (lambda ()
    (let ((environment     (package/environment (find-package package-name)))
	  (library-options (delay (library-directory-pathname "options"))))
      (for-each
	  (lambda (file)
	    (let ((file  (force* file)))
	      (cond 
	       (((ucode-primitive initialize-c-compiled-block 1)
		 (string-append "runtime_" file))
		=> (lambda (obj)
		     (purify obj)
		     (scode-eval obj environment)))
	       (else
		(let ((path  (merge-pathnames file (force library-options))))
		  (with-working-directory-pathname
		   (directory-pathname path)
		   (lambda ()
		     (load path
			   environment
			   syntax-table/system-internal
			   true))))))))
	files)
      (flush-purification-queue!)
      (eval init-expression environment))))

(define (library-directory-pathname name)
  (or (system-library-directory-pathname name)
      (library-directory-pathname
       (error:file-operation name
			     "find"
			     "directory"
			     "no such directory in system library path"
			     library-directory-pathname
			     (list name)))))

(define (declare-shared-library shared-library thunk)
  (let ((thunk-valid?
	 (lambda (thunk)
	   (not (condition? (ignore-errors thunk))))))
    (add-event-receiver!
     event:after-restore
     (lambda ()
       (if (not (thunk-valid? thunk))
	   (fluid-let ((load/suppress-loading-message? true))
	     (load (merge-pathnames
		    (library-directory-pathname "shared")
		    shared-library))))))))