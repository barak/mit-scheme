#| -*-Scheme-*-

$Id: option.scm,v 14.33 1995/04/29 14:09:09 adams Exp $

Copyright (c) 1988-1994 Massachusetts Institute of Technology

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

    (if (not (memq name loaded-options))
	(find-option)
	name)))

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