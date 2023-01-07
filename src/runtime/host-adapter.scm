#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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

;;;; Adapt host system.
;;; package: (user)

(declare (usual-integrations))

;;; This file is loaded by the cross-syntaxer and cross-compiler to
;;; hack the host e.g. to add bindings that were added to the new
;;; runtime AND used in the new CREF/SF/LIAR.  It is NOT loaded into
;;; the new runtime.  It contains temporary hacks that will be kept
;;; only until the new runtime is released.

(let ((genv (->environment '()))
      (renv (->environment '(runtime))))

  (if (lexical-unreferenceable? genv 'target-bytes-per-object)
      (environment-define genv 'target-bytes-per-object bytes-per-object))
  (if (lexical-unreferenceable? genv 'cref/source-root)
      (environment-define genv 'cref/source-root #!default))
  (if (lexical-unreferenceable? genv 'cref/object-root)
      (environment-define genv 'cref/object-root #!default))

  (if (not (environment-bound? renv 'define-sequenced-procedure))
      (eval '(define-syntax define-sequenced-procedure
	       (er-macro-transformer
		(lambda (form rename compare)
		  (declare (ignore compare))
		  (syntax-check '(_ identifier expression expression) form)
		  (let ((name (cadr form))
			(seq (caddr form))
			(expr (cadddr form))
			(args (new-identifier 'args)))
		    `(,(rename 'begin)
		      (,(rename 'define) ,name
		       (,(rename 'lambda) ,args
			(,seq 'add-action!
			      (,(rename 'lambda) ()
			       (,(rename 'apply) ,name ,args)))))
		      (,seq 'add-action!
			    (,(rename 'lambda) ()
			     (,(rename 'set!) ,name ,expr)
			     ,(rename 'unspecific))))))))
	    renv))

  (if (not (environment-bound? renv 'define-values-deferred))
      (eval '(define-syntax define-values-deferred
	       (er-macro-transformer
		(lambda (form rename compare)
		  (declare (ignore compare))
		  (syntax-check '(_ (* identifier) expression) form)
		  (let ((names (cadr form))
			(expr (caddr form)))
		    `(,(rename 'begin)
		      ,@(map (lambda (name)
			       `(,(rename 'define) ,name))
			     names)
		      (,(rename 'add-boot-init!)
		       (,(rename 'lambda) ()
			,(let ((names* (map new-identifier names)))
			   `(let-values ((,names* ,expr))
			      ,@(map (lambda (name name*)
				       `(,(rename 'set!) ,name ,name*))
				     names
				     names*)
			      ,(rename 'unspecific))))))))))
	    renv))

  (if (not (environment-bound? genv 'hash-bound))
      (begin
	(eval '(define-syntax hash-bound
		 (syntax-rules ()
		   ((_) (select-on-bytes-per-word #x02000000 #x100000000))))
	      renv)
	(link-variables genv 'hash-bound renv 'hash-bound)))

  (if (not (environment-bound? genv 'hash-mask))
      (begin
	(eval '(define-syntax hash-mask
		 (syntax-rules ()
		   ((_) (select-on-bytes-per-word #x01FFFFFF #xFFFFFFFF))))
	      renv)
	(link-variables genv 'hash-mask renv 'hash-mask)))

  (let ((env (->environment '(scode-optimizer expansion))))

    (define (get name)
      (environment-lookup env name))

    (define (put! name value)
      (environment-assign! env name value))

    (define alist
      (get 'usual-integrations/expansion-alist))

    (set! alist
	  (remove! (lambda (p)
		     (let ((name (car p)))
		       (or (eq? name 'call-with-values)
			   (eq? name 'with-values)
			   (eq? name 'values))))
		   alist))
    (put! 'usual-integrations/expansion-alist alist)
    (put! 'usual-integrations/expansion-names (map car alist))
    (put! 'usual-integrations/expansion-values (map cdr alist)))

  )