#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/option.scm,v 14.10 1991/02/19 22:45:25 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

;;;; Option Loader
;;; package: (runtime options)

(declare (usual-integrations))

(define (load-option name)
  (let ((entry (assq name options))
	(directory
	 (system-library-directory-pathname (string->pathname "options"))))
    (if (not entry)
	(error "Unknown option name" name))
    (for-each
     (lambda (descriptor)
       (let ((environment
	      (package/environment (find-package (car descriptor)))))
	 (for-each (lambda (filename)
		     (load (merge-pathnames (string->pathname filename)
					    directory)
			   environment
			   syntax-table/system-internal
			   true))
		   (cddr descriptor))
	 (eval (cadr descriptor) environment)))
     (cdr entry))
    name))

(define options
  '((ARITHMETIC-INTERFACE ((RUNTIME NUMBER INTERFACE) #F "numint"))
    (FORMAT ((RUNTIME FORMAT) (INITIALIZE-PACKAGE!) "format"))
    (HASH-TABLE ((RUNTIME HASH-TABLE) (INITIALIZE-PACKAGE!) "hashtb"))
    (SUBPROCESS ((RUNTIME SUBPROCESS) (INITIALIZE-PACKAGE!) "process"))))