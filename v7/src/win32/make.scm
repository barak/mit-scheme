#| -*-Scheme-*-

$Id: make.scm,v 1.7 2001/08/17 13:01:32 cph Exp $

Copyright (c) 1993-1999, 2001 Massachusetts Institute of Technology

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

;;;; Win32 subsystem: System Construction

(declare (usual-integrations))

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    ((access with-directory-rewriting-rule
	     (->environment '(RUNTIME COMPILER-INFO)))
     (working-directory-pathname)
     (pathname-as-directory "win32")
     (lambda ()
       (load "ffimacro")
       (load-package-set "win32")))))

;((package/reference (find-package '(WIN32))
;		    'INITIALIZE-PACKAGE!))
(add-identification! "Win32" 1 5)


(define (package-initialize package-name procedure-name mandatory?)
  (define (print-name string)
    (display "\n")
    (display string)
    (display " (")
    (let loop ((name package-name))
      (if (not (null? name))
	  (begin
	    (if (not (eq? name package-name))
		(display " "))
	    (display (system-pair-car (car name)))
	    (loop (cdr name)))))
    (display ")"))

  (define (package-reference name)
    (package/environment (find-package name)))

  (let ((env (package-reference package-name)))
    (cond ((not (lexical-unreferenceable? env procedure-name))
	   (print-name "initialize:")
	   (if (not (eq? procedure-name 'INITIALIZE-PACKAGE!))
	       (begin
		 (display " [")
		 (display (system-pair-car procedure-name))
		 (display "]")))
	   ((lexical-reference env procedure-name)))
	  ((not mandatory?)
	   (print-name "* skipping:"))
	  (else
	   ;; Missing mandatory package! Report it and die.
	   (print-name "Package")
	   (display " is missing initialization procedure ")
	   (display (system-pair-car procedure-name))
	   (fatal-error "Could not initialize a required package.")))))


(package-initialize '(win32) 'initialize-protection-list-package! #t)
(package-initialize '(win32) 'initialize-module-package! #t)
(package-initialize '(win32) 'initialize-package! #t)
(package-initialize '(win32) 'init-wf_user! #t)
(package-initialize '(win32 scheme-graphics) 'initialize-package! #t)
(package-initialize '(win32 dib) 'initialize-package! #t)