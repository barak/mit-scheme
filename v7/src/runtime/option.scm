#| -*-Scheme-*-

$Id: option.scm,v 14.42 2002/11/20 19:46:21 cph Exp $

Copyright (c) 1988-2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Option Loader
;;; package: (runtime options)

(declare (usual-integrations))

(define (load-option name #!optional no-error?)
  (let ((no-error? (and (not (default-object? no-error?)) no-error?)))

    (define (find-option options parent)
      (cond ((assq name options) => load-entry)
	    ((force* parent) => search-parent)
	    ((not no-error?) (error "Unknown option name:" name))
	    (else #f)))

    (define (load-entry entry)
      (for-each (lambda (thunk) (thunk)) (cdr entry))
      (set! loaded-options (cons name loaded-options))
      name)

    (define (search-parent file)
      (call-with-values
	  (lambda ()
	    (fluid-let ((*options* '())
			(*parent* #f))
	      (fluid-let ((load/suppress-loading-message? #t))
		(load-latest (merge-pathnames file
					      (library-directory-pathname ""))
			     (make-load-environment)
			     'DEFAULT
			     #f))
	      (values *options* *parent*)))
	find-option))

    (define (make-load-environment)
      (extend-top-level-environment system-global-environment))

    (fluid-let ((*parser-canonicalize-symbols?* #t))
      (if (memq name loaded-options)
	  name
	  (find-option *options* *parent*)))))

(define (define-load-option name . loaders)
  (set! *options* (cons (cons name loaders) *options*))
  unspecific)

(define (further-load-options place)
  (set! *parent* place)
  unspecific)

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

(define (library-file? library-internal-path)
  (let* ((library (library-directory-pathname ""))
	 (pathname (merge-pathnames library-internal-path library)))
    (let loop ((file-types load/default-types))
      (and (not (null? file-types))
	   (let ((full-pathname
		  (pathname-new-type pathname (caar file-types))))
	     (if (file-exists? full-pathname)
		 ;; not full-pathname to allow load-latest
		 pathname		
		 (loop (cdr file-types))))))))

(define loaded-options '())
(define *options* '())			; Current options.
(define *parent* initial-load-options)	; A thunk or a pathname/string or #f.
(define *initial-options-file* #f)

(define (dummy-option-loader)
  unspecific)

(define (standard-option-loader package-name init-expression . files)
  (lambda ()
    (let ((environment (package/environment (find-package package-name)))
	  (runtime (pathname-as-directory "runtime")))
      (for-each (lambda (file)
		  (let ((file (force* file)))
		    (cond 
		     (((ucode-primitive initialize-c-compiled-block 1)
		       (string-append "runtime_" file))
		      => (lambda (obj)
			   (purify obj)
			   (scode-eval obj environment)))
		     (else
		      (let* ((options (library-directory-pathname "options"))
			     (pathname (merge-pathnames file options)))
			(with-directory-rewriting-rule options runtime
			  (lambda ()
			    (with-working-directory-pathname
				(directory-pathname pathname)
			      (lambda ()
				(load pathname
				      environment
				      'DEFAULT
				      #t))))))))))
		files)
      (flush-purification-queue!)
      (eval init-expression environment))))

(define (declare-shared-library shared-library thunk)
  (let ((thunk-valid?
	 (lambda (thunk)
	   (not (condition? (ignore-errors thunk))))))
    (add-event-receiver!
     event:after-restore
     (lambda ()
       (if (not (thunk-valid? thunk))
	   (fluid-let ((load/suppress-loading-message? #t))
	     (load
	      (merge-pathnames shared-library
			       (library-directory-pathname "shared")))))))))

(define (force* value)
  (cond	((procedure? value) (force* (value)))
	((promise? value) (force* (force value)))
	(else value)))

(define (library-directory-pathname name)
  (or (system-library-directory-pathname name)
      (library-directory-pathname
       (error:file-operation name
			     "find"
			     "directory"
			     "no such directory in system library path"
			     library-directory-pathname
			     (list name)))))