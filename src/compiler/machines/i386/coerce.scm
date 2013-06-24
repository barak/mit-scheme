#| -*-Scheme-*-

$Id: coerce.scm,v 1.5 2003/02/14 18:28:03 cph Exp $
$MC68020-Header: coerce.scm,v 1.10 88/08/31 05:56:37 GMT cph Exp $

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Intel i386 Specific Coercions

(declare (usual-integrations))

;; *** NOTE ***
;; If you add coercions here, remember to also add them in "insmac.scm".

(define make-coercion
  (coercion-maker
   `((UNSIGNED . ,coerce-unsigned-integer)
     (SIGNED . ,coerce-signed-integer))))

(define coerce-2-bit-unsigned (make-coercion 'UNSIGNED 2))
(define coerce-3-bit-unsigned (make-coercion 'UNSIGNED 3))
(define coerce-8-bit-unsigned (make-coercion 'UNSIGNED 8))
(define coerce-16-bit-unsigned (make-coercion 'UNSIGNED 16))
(define coerce-32-bit-unsigned (make-coercion 'UNSIGNED 32))

(define coerce-8-bit-signed (make-coercion 'SIGNED 8))
(define coerce-16-bit-signed (make-coercion 'SIGNED 16))
(define coerce-32-bit-signed (make-coercion 'SIGNED 32))