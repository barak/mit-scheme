#| -*-Scheme-*-

$Id: 8a9136d8b95e4c806f5696fe31c94262750dea4a $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

(define (usual-integrations/cache!)
  (set! usual-integrations/constant-names
	(list-copy global-constant-objects))
  (set! usual-integrations/constant-values
	(map (lambda (name)
	       (let ((object
		      (environment-lookup system-global-environment name)))
		 (if (not (memq (microcode-type/code->name
				 (object-type object))
				'(BIGNUM
				  CHARACTER
				  FALSE
				  FIXNUM
				  FLONUM
				  INTERNED-SYMBOL
				  RATNUM
				  RECNUM
				  TRUE
				  UNINTERNED-SYMBOL)))
		     (error "USUAL-INTEGRATIONS: not a constant" name))
		 (constant->integration-info object)))
	     usual-integrations/constant-names))
  (set! usual-integrations/constant-alist
	(map (lambda (name)
	       (cons name
		     (constant/make
		      #f
		      (environment-lookup system-global-environment name))))
	     usual-integrations/constant-names))
  unspecific)