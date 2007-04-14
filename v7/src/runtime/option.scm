#| -*-Scheme-*-

$Id: option.scm,v 14.53 2007/04/14 03:52:51 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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
      (let ((e (extend-top-level-environment system-global-environment)))
	(environment-define e '*PARSER-CANONICALIZE-SYMBOLS?* #t)
	e))

    (if (memq name loaded-options)
	name
	(find-option *options* *parent*))))

(define (define-load-option name . loaders)
  (set! *options* (cons (cons name loaders) *options*))
  unspecific)

(define (further-load-options place)
  (set! *parent* place)
  unspecific)

(define (initial-load-options)
  (or *initial-options-file*
      (let ((s (get-environment-variable "MITSCHEME_LOAD_OPTIONS")))
	(and s
	     (confirm-pathname (merge-pathnames s (user-homedir-pathname)))))
      (local-load-options)))

(define (local-load-options)
  (or (library-file? "optiondb")
      (standard-load-options)))

(define (standard-load-options)
  (or (library-file? "runtime/optiondb") ; for C back end
      (library-file? "options/optiondb")
      (error "Cannot locate a load-option database")
      "optiondb"))

(define (library-file? library-internal-path)
  (confirm-pathname
   (merge-pathnames library-internal-path (library-directory-pathname ""))))

(define (confirm-pathname pathname)
  (receive (pathname* loader)
      (search-types-in-order pathname load/default-types)
    pathname*
    (and loader pathname)))

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
      (for-each
       (lambda (file)
	 (let ((file (force* file)))
	   (cond ((built-in-object-file (merge-pathnames file runtime))
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