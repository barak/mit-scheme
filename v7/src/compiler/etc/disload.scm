#| -*-Scheme-*-

$Id: disload.scm,v 1.3 1993/07/01 21:47:28 gjr Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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

;;;; Load the disassembler into a Scheme containing the compiler

(declare (usual-integrations))

(define (load-disassembler #!optional directory)
  (with-working-directory-pathname
    (if (default-object? directory)
	"/usr/local/lib/mit-scheme/SRC/compiler/machine"
	directory)
    (lambda ()
      (let* ((parent (or (name->package '(compiler))
			 (find-package '())))
	     (parenv (package/environment parent))
	     (disassembler
	      (package/add-child! parent
				  'disassembler
				  (eval '(make-environment) parenv))))
	(let ((disenv (package/environment disassembler))
	      (global system-global-environment)
	      (compinfo (package/environment
			 (find-package '(runtime compiler-info)))))
	  (define (export name)
	    (environment-link-name global disenv name))
	  (define (import name)
	    (environment-link-name disenv compinfo name))

	  (if (not (environment-bound? parenv 'addressing-granularity))
	      (local-assignment
	       parenv
	       'addressing-granularity
	       ;; The following computation assumes:
	       ;; - The word size is a power of 2.
	       ;; - Character strings always have a null character.
	       (let loop ((ag 1)
			  (vsize (system-vector-length "")))
		 (let* ((ag* (+ ag ag)))
		   (if (= vsize (system-vector-length (make-string (-1+ ag*))))
		       (loop ag* vsize)
		       ag)))))
	  (for-each import
		    '(compiled-code-block/dbg-info
		      dbg-info-vector/blocks-vector
		      dbg-info-vector?
		      dbg-info/labels
		      dbg-label/external?
		      dbg-label/name
		      dbg-labels/find-offset))
	  (if (file-esits? "mips.scm")
	      (load "mips" disenv))
	  (load "dassm1" disenv)
	  (load "dassm2" disenv)
	  (load "dassm3" disenv)
	  (if (file-exists? "dinstr1.scm")
	      (begin
		;; For the vax
		(load "dinstr1")
		(load "dinstr2")
		(load "dinstr3")))
	  (for-each export
		    '(compiler:write-lap-file
		      compiler:disassemble)))))))