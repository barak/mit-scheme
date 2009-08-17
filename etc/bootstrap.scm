#| -*-Scheme-*-

$Id: 28a4e5c69713f202b40c631b830b1d12188bc67b $

Copyright (c) 1991-1994 Massachusetts Institute of Technology

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

;;;; File to build Scheme binaries from sources

;;; cd to the "src" directory, start scheme with the
;;; "-compiler" option, and load this file.

;;; To make the band "runtime.com":
;;;
;;; cd runtime
;;; scheme -large -fasl make.com
;;; (disk-save "runtime.com")

;;; To make the band "compiler.com":
;;;
;;; scheme -large
;;; (cd "sf")
;;; (load "make")
;;; (cd "../compiler")
;;; (load "make")
;;; (disk-save "compiler.com")

;;; To make the band "edwin.com":
;;;
;;; scheme -large
;;; (cd "edwin")
;;; (load "make")
;;; (disk-save "edwin.com")

;;; Compile the runtime system
(with-working-directory-pathname "runtime"
  (lambda ()
    (fluid-let ((sf/default-syntax-table syntax-table/system-internal))
      (sf-directory "."))
    (compile-directory ".")))

;;; Compile and load the CREF subsystem
(with-working-directory-pathname "cref"
  (lambda ()
    (fluid-let ((sf/default-syntax-table system-global-syntax-table))
      (sf-conditionally "object")
      (sf-directory ".")
      (if (not (file-exists? "cref.bcon"))
	  (sf "triv.con" "cref.bcon"))
      (if (not (file-exists? "cref.bldr"))
	  (sf "triv.ldr" "cref.bldr")))
    (compile-directory ".")
    (if (not (name->package '(CROSS-REFERENCE)))
	(load "make"))))

;;; Generate CREF files for runtime system
(with-working-directory-pathname "runtime"
  (lambda ()
    (if (not (and (file-exists? "runtime.glob")
		  (file-exists? "runtime.con")
		  (file-exists? "runtime.ldr")))
	(begin
	  (cref/generate-constructors "runtime")
	  (sf "runtime.con" "runtime.bcon")
	  (sf "runtime.ldr" "runtime.bldr")))))

;;; Generate CREF files for CREF subsystem
(with-working-directory-pathname "cref"
  (lambda ()
    (if (not (and (file-exists? "cref.glob")
		  (file-exists? "cref.con")
		  (file-exists? "cref.ldr")))
	(begin
	  (cref/generate-constructors "cref")
	  (sf "cref.con" "cref.bcon")
	  (sf "cref.ldr" "cref.bldr")))))

;;; Compile (and generate CREF files for) SF subsystem
(with-working-directory-pathname "sf"
  (lambda ()
    (load "sf.sf")
    (load "sf.cbf")))

;;; Compile (and generate CREF files for) editor
(with-working-directory-pathname "edwin"
  (lambda ()
    (load "edwin.sf")
    (load "edwin.cbf")))

;;; Compile (and generate CREF files for) compiler
(if (file-directory? "compiler")
    (with-working-directory-pathname "compiler"
      (lambda ()
	(if (or (and (file-symbolic-link? "machine")
		     (file-symbolic-link? "compiler.cbf")
		     (file-symbolic-link? "compiler.pkg")
		     (file-symbolic-link? "compiler.sf")
		     (file-symbolic-link? "make.com")
		     (file-symbolic-link? "make.binf"))
		(let ((types
		       '((ALPHA "DEC Alpha Architecture")
			 (HP-PA "HP Precision Architecture")
			 (I386 "Intel i386 and i486, NOT 80286")
			 (MC68030 "Motorola 68030 and 68020")
			 (MC68040 "Motorola 68020 - 68040")
			 (MIPS "all big-endian MIPS (e.g. SGI, Sony)")
			 (PMAX "all little-endian MIPS (e.g. DecStations)")
			 (VAX "DEC Vax Architecture")
			 (OTHER "no compiled-code support"))))
		  (let loop ()
		    (newline)
		    (write-string "Enter one of the following machine types:")
		    (newline)
		    (write-string "Type\t\tNotes")
		    (newline)
		    (write-string "----\t\t-----")
		    (for-each (lambda (type)
				(newline)
				(write (car type))
				(if (cadr type)
				    (begin
				      (write-string "\t\t")
				      (write-string (cadr type)))))
			      types)
		    (newline)
		    (write-string "Enter machine type: ")
		    (let ((type (read)))
		      (cond
		       ((not (assq type types))
			(beep)
			(loop))
		       ((eq? type 'OTHER)
			false)
		       (else
			(let ((directory
			       (case type
				 ((ALPHA) "alpha")
				 ((HP-PA) "spectrum")
				 ((I386) "i386")
				 ((MC68030 MC68040) "bobcat")
				 ((PMAX MIPS) "mips")
				 ((VAX) "vax")))
			      (ln-sf
			       (let ((ln-s
				      (let ((prim (make-primitive-procedure
						   'FILE-LINK-SOFT)))
					(lambda (from to)
					  (prim (->namestring
						 (merge-pathnames from))
						(->namestring
						 (merge-pathnames to)))))))
				 (lambda (from to)
				   (if (file-exists? to)
				       (delete-file to))
				   (ln-s from to)))))
			  (let ((prefix
				 (string-append "machines/" directory)))
			    (with-working-directory-pathname prefix
			      (lambda ()
				(case type
				  ((MC68030)
				   (ln-sf "make020.scm" "make.scm"))
				  ((MC68040)
				   (ln-sf "make040.scm" "make.scm"))
				  ((PMAX)
				   (ln-sf "compiler.sf-little" "compiler.sf")
				   (ln-sf "make.scm-little" "make.scm"))
				  ((MIPS)
				   (ln-sf "compiler.sf-big" "compiler.sf")
				   (ln-sf "make.scm-big" "make.scm")))))
			    (ln-sf prefix "machine")
			    (ln-sf "machine/compiler.cbf" "compiler.cbf")
			    (ln-sf "machine/compiler.pkg" "compiler.pkg")
			    (ln-sf "machine/compiler.sf" "compiler.sf")
			    (ln-sf "machine/make.com" "make.com")
			    (ln-sf "machine/make.binf" "make.binf")))
			true))))))
	    (begin
	      (load "compiler.sf")
	      (load "compiler.cbf"))))))