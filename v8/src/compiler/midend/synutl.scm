#| -*-Scheme-*-

$Id: synutl.scm,v 1.1 1994/11/19 02:04:29 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

;;;; ??
;;; package: (compiler midend)

(declare (usual-integrations))

;;; Syntax-time utilities

(define (%matchup lambda-list prefix expr)
  (if (null? lambda-list)
      (values '() prefix)
      (let ((var* (generate-uninterned-symbol "SUBFORM")))
	(let loop ((ll lambda-list)
		   (names '())
		   (args '())
		   (path var*))
	  (cond ((null? ll)
		 (values (reverse names)
			 `(let ((,var* ,expr))
			    (,@prefix ,@(reverse args)))))
		((eq? (car ll) '#!rest)
		 (loop '()
		       (cons (cadr ll) names)
		       (cons path args)
		       false))
		(else
		 (loop (cdr ll)
		       (cons (car ll) names)
		       (cons `(car ,path) args)
		       `(cdr ,path))))))))