#| -*-Scheme-*-

$Id: disload.scm,v 1.12 2002/02/13 01:05:22 cph Exp $

Copyright (c) 1993, 1999, 2001, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Load the disassembler into a Scheme containing the compiler

(declare (usual-integrations))

(define (load-disassembler #!optional directory addressing-granularity)
  (with-working-directory-pathname
    (cond ((not (default-object? directory))
	   directory)
	  ((equal? microcode-id/operating-system-name "unix")
	   "/usr/local/lib/mit-scheme/SRC/compiler/machine")
	  (else
	   "/scheme/compiler/machines/i386"))
    (lambda ()
      (let* ((parent (or (name->package '(compiler))
			 (find-package '())))
	     (parenv (package/environment parent))
	     (disassembler
	      (package/add-child! parent
				  'disassembler
				  (extend-top-level-environment parenv))))
	(let ((disenv (package/environment disassembler))
	      (global system-global-environment)
	      (compinfo (package/environment
			 (find-package '(runtime compiler-info)))))
	  (define (export name)
	    (link-variables global name disenv name))
	  (define (import name)
	    (link-variables disenv name compinfo name))

	  (if (not (environment-bound? parenv 'addressing-granularity))
	      (environment-define
	       parenv
	       'addressing-granularity
	       (if (default-object? addressing-granularity)
		   8
		   addressing-granularity)))
	  (for-each import
		    '(compiled-code-block/dbg-info
		      dbg-info-vector/blocks-vector
		      dbg-info-vector?
		      dbg-info/labels
		      dbg-label/external?
		      dbg-label/name
		      dbg-labels/find-offset))
	  (if (file-exists? "mips.scm")
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