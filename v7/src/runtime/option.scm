#| -*-Scheme-*-

$Id: option.scm,v 14.22 1993/09/01 03:22:19 ziggy Exp $

Copyright (c) 1988-1993 Massachusetts Institute of Technology

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
  (let ((entry (assq name options)))
    (if (not entry)
	(error "Unknown option name" name))
    (if (not (memq name loaded-options))
	(let ((directory (library-directory-pathname "options")))
	  (for-each
	   (lambda (descriptor)
	     (let ((environment
		    (package/environment (find-package (car descriptor)))))
	       (for-each (lambda (filename)
			   (load (merge-pathnames filename directory)
				 environment
				 syntax-table/system-internal
				 true))
			 (cddr descriptor))
	       (eval (cadr descriptor) environment)))
	   (cdr entry))
	  (set! loaded-options (cons name loaded-options))))
    name))

(define (library-directory-pathname name)
  (or (system-library-directory-pathname name)
      (library-directory-pathname
       (error:file-operation name
			     "find"
			     "directory"
			     "no such directory in system library path"
			     library-directory-pathname
			     (list name)))))

(define options
  '((ARITHMETIC-INTERFACE ((RUNTIME NUMBER INTERFACE) #F "numint"))
    (COMPRESS ((RUNTIME COMPRESS) #F "cpress"))
    (DOSPROCESS (() #F "dosproc"))
    (FORMAT ((RUNTIME FORMAT) (INITIALIZE-PACKAGE!) "format"))
    (HASH-TABLE ((RUNTIME HASH-TABLE) (INITIALIZE-PACKAGE!) "hashtb"))
    (KRYPT ((RUNTIME KRYPT) #F "krypt"))
    (SUBPROCESS ((RUNTIME SUBPROCESS) (INITIALIZE-PACKAGE!) "process"))
    (PC-SAMPLE ((PC-SAMPLE INTERRUPT-HANDLER) (INITIALIZE-PACKAGE!) "pcsboot" 
					                            "pcsintrp")
	       ((PC-SAMPLE)                   (INITIALIZE-PACKAGE!) "pcsample" 
					                            "binutl")
	       ((PC-SAMPLE INTERP-PROCS)      (INITIALIZE-PACKAGE!) "pcsiproc")
	       ((PC-SAMPLE  CODE-BLOCKS)      (INITIALIZE-PACKAGE!) "pcscobl")
	       ((PC-SAMPLE DISPLAY)           (INITIALIZE-PACKAGE!) "pcsdisp")
	       )
    ))

(define loaded-options
  '())

