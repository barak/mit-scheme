#| -*-Scheme-*-

$Id: make.scm,v 15.23 1995/02/24 00:38:28 cph Exp $

Copyright (c) 1991-95 Massachusetts Institute of Technology

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

;;;; 6.001: System Construction

(declare (usual-integrations))

(package/system-loader "6001" '() 'QUERY)
(let ((edwin (->environment '(edwin))))
  (load "edextra" edwin)
  (if (eq? 'UNIX microcode-id/operating-system)
      (load "floppy" edwin)))
((access initialize-package! (->environment '(student scode-rewriting))))
(add-system! (make-system "6.001" 15 23 '()))

;;; Customize the runtime system:
(set! repl:allow-restart-notifications? false)
(set! repl:write-result-hash-numbers? false)
(set! *unparse-disambiguate-null-as-itself?* false)
(set! *unparse-disambiguate-null-lambda-list?* true)
(set! *pp-default-as-code?* true)
(set! *pp-named-lambda->define?* 'LAMBDA)
(set! x-graphics:auto-raise? true)
(set! (access write-result:undefined-value-is-special?
	      (->environment '(runtime user-interface)))
      false)
(set! hook/exit (lambda (integer) integer (warn "EXIT has been disabled.")))
(set! hook/quit (lambda () (warn "QUIT has been disabled.")))
(set! user-initial-environment (->environment '(student)))

(in-package (->environment '(edwin))
  (set! student-root-directory
	(merge-pathnames "/users/u6001/" (user-homedir-pathname)))
  (set! student-work-directory
	(merge-pathnames "work/" student-root-directory))
  (set! pset-directory (merge-pathnames "psets/" student-root-directory))
  (set! pset-list-file (merge-pathnames "probsets.scm" pset-directory)))

(in-package (->environment '(student))
  (define u6001-dir
    (let ((homedir (access student-root-directory (->environment '(edwin)))))
      (lambda (filename)
	(->namestring (merge-pathnames filename homedir)))))
  (define nil #f))

(ge '(student))