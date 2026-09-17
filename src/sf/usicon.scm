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

;;;; SCode Optimizer: Usual Integrations: Constants
;;; package: (scode-optimizer)

(declare (usual-integrations)
	 (integrate-external "object"))

(define usual-integrations/constant-names)
(define usual-integrations/constant-values)
(define usual-integrations/constant-alist)
(define usual-integrations/constant-bytes-per-object #f)

;;; The fixnum bounds depend on the word size, so when cross-compiling
;;; to a different one the host's values are wrong for the target.
;;; Assumes 6-bit type codes, as the rest of the system does.

(define (global-constant-value name)
  (let ((fx-width (- (* 8 (target-bytes-per-object)) 6)))
    (case name
      ((fx-width) fx-width)
      ((fx-greatest) (- (expt 2 (- fx-width 1)) 1))
      ((fx-least) (- (expt 2 (- fx-width 1))))
      (else (environment-lookup system-global-environment name)))))

(define (usual-integrations/refresh!)
  (if (not (eqv? (target-bytes-per-object)
		 usual-integrations/constant-bytes-per-object))
      (usual-integrations/cache!)))

(define (usual-integrations/cache!)
  (set! usual-integrations/constant-bytes-per-object (target-bytes-per-object))
  (set! usual-integrations/constant-names
	(list-copy global-constant-objects))
  (set! usual-integrations/constant-values
	(map (lambda (name)
	       (let ((object (global-constant-value name)))
		 (if (not (memq (microcode-type/code->name
				 (object-type object))
				'(bignum
				  character
				  constant
				  false
				  fixnum
				  flonum
				  interned-symbol
				  ratnum
				  recnum
				  uninterned-symbol)))
		     (error "USUAL-INTEGRATIONS: not a constant" name))
		 (constant->integration-info object)))
	     usual-integrations/constant-names))
  (set! usual-integrations/constant-alist
	(map (lambda (name)
	       (cons name
		     (constant/make #f (global-constant-value name))))
	     usual-integrations/constant-names))
  unspecific)