#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
  (let ((no-error? (and (not (default-object? no-error?)) no-error?))
	(path library-directory-path))

    (define (find-option options parent)
      (cond ((assq name options) => load-entry)
	    ((force* parent) => search-parent)
	    ((not no-error?) (error "Unknown option name:" name))
	    (else #f)))

    (define (load-entry entry)
      (fluid-let ((package/cross-compiling? #f))
	(for-each (lambda (thunk) (thunk)) (cdr entry)))
      (set! loaded-options (cons name loaded-options))
      name)

    (define (search-parent pathname)
      (call-with-values
	  (lambda ()
	    (parameterize ((*options* '())
			   (*parent* #f)
			   (param:suppress-loading-message? #t))
	      (load pathname (simple-top-level-environment #t))
	      (values (*options*)
		      (let ((parent (*parent*)))
			(if (eq? #t parent)
			    (next-optiondb)
			    parent)))))
	find-option))

    (define (next-optiondb)
      (and (pair? path)
	   (let ((p (merge-pathnames "optiondb" (car path))))
	     (set! path (cdr path))
	     (if (file-loadable? p)
		 p
		 (next-optiondb)))))

    (define (initial-load-options)
      (or *initial-options-file*
	  (let ((s (get-environment-variable "MITSCHEME_LOAD_OPTIONS")))
	    (and s (confirm-pathname
		    (merge-pathnames s (user-homedir-pathname)))))
	  (next-optiondb)))

    (if (memq name loaded-options)
	name
	(find-option (*options*) initial-load-options))))

(define (option-loaded? name)
  (not (eq? #f (memq name loaded-options))))

(define (define-load-option name . loaders)
  (*options* (cons (cons name loaders) (*options*)))
  unspecific)

(define (further-load-options place)
  (*parent* place)
  unspecific)

(define (local-load-options)
  (or (library-file? "optiondb")
      (standard-load-options)))

(define (standard-load-options)
  (or (library-file? "runtime/optiondb")
      (error "Cannot locate a load-option database.")))

(define (library-file? library-internal-path)
  (confirm-pathname
   (pathname-new-type
    (system-library-pathname (pathname-default-type library-internal-path "scm")
			     #f)
    #f)))

(define (confirm-pathname pathname)
  (and (file-loadable? pathname)
       pathname))

(define loaded-options '())
(define *options*)		 ; Current options.
(define *parent*)		 ; A thunk or a pathname/string or #f.
(define *initial-options-file* #f)

(define (initialize-package!)
  (set! *options* (make-settable-parameter '()))
  (set! *parent* (make-settable-parameter #f)))

(define (dummy-option-loader)
  unspecific)

(define (standard-option-loader package-name init-expression . files)
  (lambda ()
    (let ((environment (package/environment (find-package package-name)))
	  (runtime (pathname-as-directory "runtime"))
	  (rundir (system-library-directory-pathname "runtime" #t)))
      (for-each
       (lambda (file)
	 (let ((file (force* file)))
	   (cond ((built-in-object-file (merge-pathnames file runtime))
		  => (lambda (obj)
		       (purify obj)
		       (scode-eval obj environment)))
		 (else
		  (let ((pathname (merge-pathnames file rundir)))
		    (with-directory-rewriting-rule rundir runtime
		      (lambda ()
			(with-working-directory-pathname
			    (directory-pathname pathname)
			  (lambda ()
			    (load pathname
				  environment
				  'default
				  #t))))))))))
       files)
      (flush-purification-queue!)
      (if init-expression
	  (eval init-expression environment)))))

(define (force* value)
  (cond ((procedure? value) (force* (value)))
	((promise? value) (force* (force value)))
	(else value)))

(define (standard-system-loader name)
  (let ((here (merge-pathnames (pathname-as-directory name)
			       (directory-pathname (current-load-pathname)))))
    (lambda ()
      (with-working-directory-pathname here (lambda () (load "make"))))))