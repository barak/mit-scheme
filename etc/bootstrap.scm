;;; File to build Scheme 7.1.3 binaries from sources

;;; cd to the "dist-7.1.3/src" directory, start scheme with the
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
    (if (not (and (file-exists? "runtim.glob")
		  (file-exists? "runtim.con")
		  (file-exists? "runtim.ldr")))
	(begin
	  (cref/generate-constructors "runtim")
	  (sf "runtim.con" "runtim.bcon")
	  (sf "runtim.ldr" "runtim.bldr")))))

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

;;; Compile (and generate CREF files for) compiler
(if (file-directory? "compiler")
    (with-working-directory-pathname "compiler"
      (lambda ()
	(if (or (and (file-symbolic-link? "machine")
		     (file-symbolic-link? "comp.cbf")
		     (file-symbolic-link? "comp.pkg")
		     (file-symbolic-link? "comp.sf")
		     (file-symbolic-link? "make.com")
		     (file-symbolic-link? "make.binf"))
		(let ((types
		       '((MC68030 "also 68020")
			 (MC68040 "also 68030, 68020")
			 (VAX #f)
			 (HP-PA "HP Precision Architecture")
			 (PMAX "DECStation 3100 or 5100 = little-endian MIPS")
			 (MIPS "all other MIPS = big-endian")
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
		      (cond ((not (assq type types))
			     (beep)
			     (loop))
			    ((eq? type 'OTHER)
			     false)
			    (else
			     (let ((directory
				    (case type
				      ((MC68030 MC68040) "bobcat")
				      ((VAX) "vax")
				      ((HP-PA) "spectrum")
				      ((PMAX MIPS) "mips")))
				   (ln-sf
				    (let ((ln-s
					   (make-primitive-procedure
					    'FILE-LINK-SOFT)))
				      (lambda (from to)
					(delete-file to)
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
					(ln-sf "comp.sf-little" "comp.sf")
					(ln-sf "make.scm-little" "make.scm"))
				       ((MIPS)
					(ln-sf "comp.sf-big" "comp.sf")
					(ln-sf "make.scm-big" "make.scm")))))
				 (ln-sf prefix "machine")
				 (ln-sf "machine/comp.cbf" "comp.cbf")
				 (ln-sf "machine/comp.pkg" "comp.pkg")
				 (ln-sf "machine/comp.sf" "comp.sf")
				 (ln-sf "machine/make.com" "make.com")
				 (ln-sf "machine/make.binf" "make.binf")))
			     true))))))
	    (begin
	      (load "comp.sf")
	      (load "comp.cbf"))))))

;;; Compile (and generate CREF files for) editor
(with-working-directory-pathname "edwin"
  (lambda ()
    (load "edwin.sf")
    (load "edwin.cbf")))