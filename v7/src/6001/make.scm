#| -*-Scheme-*-

$Id: make.scm,v 15.32 2001/08/18 04:50:22 cph Exp $

Copyright (c) 1991-1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; 6.001: System Construction

(declare (usual-integrations))

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    ((access with-directory-rewriting-rule
	     (->environment '(RUNTIME COMPILER-INFO)))
     (working-directory-pathname)
     (pathname-as-directory "6001")
     (lambda ()
       (load-package-set "6001")
       (if (and (eq? 'UNIX microcode-id/operating-system)
		(string-ci=? "HP-UX" microcode-id/operating-system-variant))
	   (load "floppy" (->environment '(edwin))))))))
(add-identification! "6.001" 15 30)

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
  ;; These defaults will be overridden when the editor is started.
  (set! student-root-directory "~u6001/")
  (set! student-work-directory "~/work/")
  (set! pset-directory "~u6001/psets/")
  (set! pset-list-file "~u6001/psets/probsets.scm"))

(in-package (->environment '(student))
  (define u6001-dir
    (let ((edwin (->environment '(edwin))))
      (lambda (filename)
	(->namestring
	 (merge-pathnames filename (access student-root-directory edwin))))))
  (define nil #f))

(ge '(student))