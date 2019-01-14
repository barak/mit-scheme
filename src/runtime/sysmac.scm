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

;;;; System Internal Syntax
;;; package: (runtime system-macros)

(declare (usual-integrations))

(define-syntax define-primitives
  (sc-macro-transformer
   (lambda (form environment)
     (declare (ignore environment))
     (let ((primitive-definition
	    (lambda (variable-name primitive-args)
	      (let ((primitive
		     (apply make-primitive-procedure primitive-args)))
		(let ((arity (procedure-arity primitive)))
		  (if (eqv? (procedure-arity-min arity)
			    (procedure-arity-max arity))
		      (let ((names
			     (map (lambda (n) (symbol 'a n))
				  (iota (procedure-arity-min arity) 1))))
			`(define-integrable (,variable-name ,@names)
			   (,primitive ,@names)))
		      `(define-integrable ,variable-name
			 ,primitive)))))))
       `(begin ,@(map (lambda (name)
			(cond ((not (pair? name))
			       (primitive-definition name (list name)))
			      ((not (symbol? (cadr name)))
			       (primitive-definition (car name) name))
			      (else
			       (primitive-definition (car name) (cdr name)))))
		      (cdr form)))))))

(define-syntax ucode-type
  (sc-macro-transformer
   (lambda (form environment)
     (declare (ignore environment))
     (apply microcode-type (cdr form)))))

(define-syntax ucode-primitive
  (sc-macro-transformer
   (lambda (form environment)
     (declare (ignore environment))
     (apply make-primitive-procedure (cdr form)))))

(define-syntax ucode-return-address
  (sc-macro-transformer
   (lambda (form environment)
     (declare (ignore environment))
     (make-return-address (apply microcode-return (cdr form))))))

(define-syntax define-guarantee
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(symbol expression) (cdr form))
	 (let ((root (cadr form))
	       (desc (close-syntax (caddr form) environment)))
	   (let ((p-name (symbol root '?))
		 (g-name (symbol 'guarantee- root))
		 (e-name (symbol 'error:not- root)))
	     `(begin
		(define (,g-name object #!optional caller)
		  (declare (integrate caller))
		  (if (not (,(close-syntax p-name environment) object))
		      (,(close-syntax e-name environment) object caller))
		  object)
		(define (,e-name object #!optional caller)
		  (error:wrong-type-argument object ,desc caller)))))
	 (ill-formed-syntax form)))))

(define-syntax define-deferred
  (er-macro-transformer
   (lambda (form rename compare)
     (declare (ignore compare))
     (syntax-check '(_ identifier expression) form)
     (let ((name (cadr form))
	   (value (caddr form)))
       `(,(rename 'begin)
	  (,(rename 'define) ,name)
	  (,(rename 'add-boot-init!)
	   (,(rename 'lambda) ()
	     (,(rename 'set!) ,name ,value)
	     ,(rename 'unspecific))))))))

(define-syntax select-on-bytes-per-word
  (er-macro-transformer
   (lambda (form rename compare)
     (declare (ignore rename compare))
     (syntax-check '(_ expression expression) form)
     (let ((bpo (bytes-per-object)))
       (case bpo
	 ((4) (cadr form))
	 ((8) (caddr form))
	 (else (error "Unsupported bytes-per-object:" bpo)))))))

(define-syntax variable-setter
  (syntax-rules ()
    ((_ identifier)
     (lambda (value)
       (set! identifier value)
       unspecific))))