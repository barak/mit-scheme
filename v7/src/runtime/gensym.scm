#| -*-Scheme-*-

$Id: gensym.scm,v 14.7 2002/11/20 19:46:20 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

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

;;;; Symbol Generation
;;; package: (runtime gensym)

(declare (usual-integrations))

(define (generate-uninterned-symbol #!optional argument)
  (let ((prefix
	 (cond ((or (default-object? argument) (not argument))
		name-prefix)
	       ((string? argument)
		argument)
	       ((symbol? argument)
		(symbol-name argument))
	       ((exact-nonnegative-integer? argument)
		(set! name-counter argument)
		name-prefix)
	       (else
		(error:wrong-type-argument argument "symbol or integer"
					   'GENERATE-UNINTERNED-SYMBOL)))))
    (string->uninterned-symbol
     (string-append prefix
		    (number->string
		     (let ((result name-counter))
		       (set! name-counter (1+ name-counter))
		       result))))))

(define name-counter)
(define name-prefix)

(define (initialize-package!)
  (set! name-counter 0)
  (set! name-prefix "G")
  unspecific)