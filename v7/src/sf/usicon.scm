#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/usicon.scm,v 3.2 1987/05/04 23:50:04 cph Rel $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Usual Integrations: Constants

(declare (usual-integrations))

(define usual-integrations/constant-names)
(define usual-integrations/constant-values)
(define usual-integrations/constant-alist)

(define (usual-integrations/delete-constant! name)
  (set! global-constant-objects (delq! name global-constant-objects))
  (usual-integrations/cache!))

(define (usual-integrations/cache!)
  (set! usual-integrations/constant-names
	(list-copy global-constant-objects))
  (set! usual-integrations/constant-values
	(map (lambda (name)
	       (let ((object
		      (lexical-reference system-global-environment name)))
		 (if (not (scode-constant? object))
		     (error "USUAL-INTEGRATIONS: not a constant" name))
		 (constant->integration-info object)))
	     usual-integrations/constant-names))
  (set! usual-integrations/constant-alist
	(map (lambda (name)
	       (cons name
		     (constant/make
		      (lexical-reference system-global-environment name))))
	     usual-integrations/constant-names))
  'DONE)

(define (constant->integration-info constant)
  (return-2 (constant/make constant) '()))