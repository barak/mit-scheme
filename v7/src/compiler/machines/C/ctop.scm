#| -*-Scheme-*-

$Id: ctop.scm,v 1.10 1993/11/17 05:22:03 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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

;;;; C-output fake assembler and linker
;;; package: (compiler top-level)

(declare (usual-integrations))

;;;; Exports to the compiler

(define compiled-output-extension "c")
(define compiler:invoke-c-compiler? true)
(define compiler:c-compiler-name "cc")
(define compiler:c-compiler-switches 'UNKNOWN)
(define compiler:c-linker-name 'UNKNOWN)
(define compiler:c-linker-switches 'UNKNOWN)
(define compiler:c-linker-output-extension 'UNKNOWN)

(define (compiler-file-output object pathname)
  (let ((pair (vector-ref object 1)))
    (call-with-output-file pathname
      (lambda (port)
	(write-string (cdr pair) port)))
    (if compiler:invoke-c-compiler? (c-compile pathname))))

(define (compiled-scode->procedure compiled-scode environment)
  ;; This could compile to a file, c-compile it, and then load it.
  environment				; ignored
  (error "compiled-scode->procedure: Not yet implemented"
	 compiled-scode))

(define (cross-compile-bin-file input . more)
  input more				; ignored
  (error "cross-compile-bin-file: Meaningless"))

(define (optimize-linear-lap lap-program)
  lap-program)

(define (c-compile pathname)
  ;; Some c compilers do not leave the output file in the same place.
  (with-working-directory-pathname
    (directory-pathname pathname)
    (lambda ()
      (fluid-let ((*call/cc-c-compiler* compiler:c-compiler-name)
		  (*call/cc-warn?* false))
	(let ((source (enough-namestring pathname))
	      (object (enough-namestring (pathname-new-type pathname "o")))
	      (call/cc*
	       (lambda (l)
		 (let ((result (apply call/cc l)))
		   #|
		   ;; Some C compilers always fail
		   (if (not (zero? result))
		       (error "compiler: C compiler/linker failed"))
		   |#
		   result))))
	  (newline)
	  (display ";Compiling ")
	  (display source)
	  (call/cc* (append (c-compiler-switches) (list source)))
	  (set! *call/cc-c-compiler* (c-linker-name))
	  (newline)
	  (display ";Linking ")
	  (display object)
	  (call/cc* (append (list "-o")
			    (list
			     (enough-namestring
			      (pathname-new-type pathname
						 (c-output-extension))))
			    (c-linker-switches)
			    (list object)))
	  (delete-file object))))))

(define (c-output-extension)
  (cond ((not (eq? compiler:c-linker-output-extension 'UNKNOWN))
	 compiler:c-linker-output-extension)
	((assoc microcode-id/operating-system-variant
		c-compiler-switch-table)
	 => (lambda (place)
	      (set! compiler:c-linker-output-extension (cadr place))
	      (cadr place)))
	(else
	 (error "c-output-extension: Unknown OS"
		microcode-id/operating-system-variant))))

(define c-compiler-switch-table
  `(("AIX"
     "so"
     ("-c" "-O" "-D_AIX")
     ,(lambda (dir)
	(list "-bM:SRE"
	      (string-append "-bE:"
			     (->namestring (merge-pathnames dir "liarc.exp")))
	      (string-append "-bI:"
			     (->namestring (merge-pathnames dir "scheme.imp")))
	      "-edload_initialize_file")))
    ("HP-UX"
     "sl"
     ("-c" "+z" "-O" "-Ae" "-D_HPUX")
     ("-b"))
    ("OSF"
     "so"
     ("-c" "-std1")
     ("-shared" "-expect_unresolved" "'*'"))
    ("SunOS"
     "so"
     ("-c" "-pic" "-O" "-Dsun4" "-D_SUNOS4" "-w")
     ())))

(define (c-compiler-switches)
  (if (not (eq? compiler:c-compiler-switches 'UNKNOWN))
      compiler:c-compiler-switches
      (let ((place (assoc microcode-id/operating-system-variant
			  c-compiler-switch-table))
	    (dir (system-library-directory-pathname "include")))
	(cond ((not place)
	       (error 'c-compiler-switches "Unknown OS"
		      microcode-id/operating-system-variant))
	      ((not dir)
	       (error 'c-compiler-switches
		      "Cannot find \"include\" directory"))
	      (else
	       (let ((result
		      (append
		       (caddr place)
		       (list
			(string-append
			 "-I"
			 (->namestring
			  (directory-pathname-as-file dir)))))))
		 (set! compiler:c-compiler-switches result)
		 result))))))

(define (c-linker-name)
  (if (not (eq? compiler:c-linker-name 'UNKNOWN))
      compiler:c-linker-name
      (let ((new (if (string=? "AIX" microcode-id/operating-system-variant)
		     "cc"
		     "ld")))
	(set! compiler:c-linker-name new)
	new)))

(define (c-linker-switches)
  (cond ((not (eq? compiler:c-linker-switches 'UNKNOWN))
	 compiler:c-linker-switches)
	((assoc microcode-id/operating-system-variant c-compiler-switch-table)
	 => (lambda (place)
	      (let ((switches
		     (let ((switches (cadddr place)))
		       (if (not (scode/procedure? switches))
			   switches
			   (let ((dir (system-library-directory-pathname
				       "include")))
			     (if (not dir)
				 (error 'c-linker-switches
					"Cannot find \"include\" directory"))
			     (switches dir))))))
		(set! compiler:c-linker-switches switches)
		switches)))
	(else
	 (error 'c-linker-switches "Unknown OS"
		microcode-id/operating-system-variant))))

(define (recursive-compilation-results)
  (sort *recursive-compilation-results*
	(lambda (x y)
	  (< (vector-ref x 0)
	     (vector-ref y 0)))))

;; Global variables for assembler and linker

(define *recursive-compilation-results*)

;; First set: phase/rtl-generation
;; Last used: phase/link
(define *block-label*)
(define *disambiguator*)

(define *start-label*)

;; First set: phase/lap-generation
;; Last used: phase/info-generation-2
(define *external-labels*)
(define *special-labels*)

;; First set: phase/lap-generation
;; Last used: phase/output-generation ???
(define *invoke-interface*)
(define *used-invoke-primitive*)
(define *use-jump-execute-chache*)
(define *use-pop-return*)
(define *purification-root-object*)

;; First set: phase/assemble
;; Last used: phase/output-generation
(define *C-code-name*)
(define *C-data-name*)
(define *ntags*)
(define *labels*)
(define *code*)

;; First set: phase/output-generation
(define *result*)

(define (assemble&link info-output-pathname)
  (phase/assemble info-output-pathname)
  (if info-output-pathname
      (phase/info-generation-2 *labels* info-output-pathname))
  (phase/output-generation)
  *result*)

(define (wrap-lap entry-label some-lap)
  (set! *start-label* entry-label)
  (LAP ,@(if *procedure-result?*
	     (LAP)
	     (lap:make-entry-point entry-label *block-label*))
       ,@some-lap))

(define (bind-assembler&linker-top-level-variables thunk)
  (fluid-let ((*recursive-compilation-results* '()))
    (thunk)))

(define (bind-assembler&linker-variables thunk)
  (fluid-let ((current-register-list)
	      (free-assignments)
	      (free-references)
	      (free-uuo-links)
	      (global-uuo-links)
	      (label-num)
	      (labels)
	      (objects)
	      (permanent-register-list)
	      (*block-label*)
	      (*disambiguator*)
	      (*start-label*)
	      (*external-labels*)
	      (*special-labels*)
	      (*invoke-interface*)
	      (*used-invoke-primitive*)
	      (*use-jump-execute-chache*)
	      (*use-pop-return*)
	      (*purification-root-object*)
	      (*end-of-block-code*)
	      (*C-code-name*)
	      (*C-data-name*)
	      (*ntags*)
	      (*labels*)
	      (*code*))
    (thunk)))

(define (assembler&linker-reset!)
  (set! *recursive-compilation-results* '())
  (set! current-register-list)
  (set! free-assignments)
  (set! free-references)
  (set! free-uuo-links)
  (set! global-uuo-links)
  (set! label-num)
  (set! labels)
  (set! objects)
  (set! permanent-register-list)
  (set! *block-label*)
  (set! *disambiguator*)
  (set! *start-label*)
  (set! *external-labels*)
  (set! *special-labels*)
  (set! *invoke-interface*)
  (set! *used-invoke-primitive*)
  (set! *use-jump-execute-chache*)
  (set! *use-pop-return*)
  (set! *purification-root-object*)
  (set! *end-of-block-code*)
  (set! *C-code-name*)
  (set! *C-data-name*)
  (set! *ntags*)
  (set! *labels*)
  (set! *code*)
  unspecific)

(define (initialize-back-end!)
  (set! current-register-list '())
  (set! free-assignments (make-table))
  (set! free-references (make-table))
  (set! free-uuo-links (list 'FOO))
  (set! global-uuo-links (list 'BAR))
  (set! label-num 0)
  (set! labels '())
  (set! objects (make-table))
  (set! permanent-register-list '())
  (set! *block-label* (generate-label))
  (set! *disambiguator*
	(if (zero? *recursive-compilation-number*)
	    ""
	    (string-append (number->string *recursive-compilation-number*)
			   "_")))
  (set! *external-labels* '())
  (set! *special-labels* (make-special-labels))
  (set! *invoke-interface* 'INFINITY)
  (set! *used-invoke-primitive* false)
  (set! *use-jump-execute-chache* false)
  (set! *use-pop-return* false)
  (set! *purification-root-object* false)
  (set! *end-of-block-code* (LAP))
  unspecific)

(define (phase/assemble pathname)
  (compiler-phase
   "Pseudo-Assembly"			; garbage collection
   (lambda ()
     (with-values
	 (lambda ()
	   (stringify
	    (if (eq? pathname 'RECURSIVE)
		(string-append "_"
			       (number->string *recursive-compilation-number*))
		"")
	    (last-reference *start-label*)
	    (last-reference *lap*)
	    (if (eq? pathname 'RECURSIVE)
		(cons *info-output-filename*
		      *recursive-compilation-number*)
		pathname)))
       (lambda (code-name data-name ntags labels code)
	 (set! *C-code-name* code-name)
	 (set! *C-data-name* data-name)
	 (set! *ntags* ntags)
	 (set! *labels* labels)
	 (set! *code* code)
	 unspecific)))))

(define (phase/output-generation)
  (if (not (null? *ic-procedure-headers*))
      (error "phase/output-generation: Can't hack IC procedures"))

  (set! *result*
	(if *procedure-result?*
	    (let* ((linking-info *subprocedure-linking-info*)
		   (translate-label
		    (lambda (label)
		      (let ((place (assq label *labels*)))
			(if (not place)
			    (error "translate-label: Not found" label)
			    (cdr place)))))
		   (translate-symbol
		    (lambda (index)
		      (translate-label (vector-ref linking-info index))))
		   (index *recursive-compilation-number*)
		   (name (fake-compiled-block-name index)))
	      (cons (make-fake-compiled-procedure
		     name
		     (translate-label *entry-label*))
		    (vector
		     (make-fake-compiled-block name
					       *C-code-name*
					       *C-data-name*
					       *code*
					       index
					       *ntags*)
		     (translate-symbol 0)
		     (translate-symbol 1)
		     (translate-symbol 2))))
	    (cons *C-code-name*
		  *code*)))

  (if (not compiler:preserve-data-structures?)
      (begin
	(set! *subprocedure-linking-info*)
	(set! *labels*)
	(set! *block-label*)
	(set! *entry-label*)
	(set! *ic-procedure-headers*)
	(set! *code*)
	unspecific)))

(define (phase/info-generation-2 labels pathname)
  (info-generation-2 labels pathname))

(define (info-generation-2 labels pathname)
  (compiler-phase "Debugging Information Generation"
    (lambda ()
      (let ((info
	     (info-generation-phase-3
	      (last-reference *dbg-expression*)
	      (last-reference *dbg-procedures*)
	      (last-reference *dbg-continuations*)
	      labels
	      (last-reference *external-labels*))))
	(cond ((eq? pathname 'KEEP)	; for dynamic execution
	       info)
	      ((eq? pathname 'RECURSIVE) ; recursive compilation
	       (set! *recursive-compilation-results*
		     (cons (vector *recursive-compilation-number*
				   info
				   false)
			   *recursive-compilation-results*))
	       unspecific)
	      (else
	       (compiler:dump-info-file
		(let ((others (recursive-compilation-results)))
		  (if (null? others)
		      info
		      (list->vector
		       (cons info
			     (map (lambda (other) (vector-ref other 1))
				  others)))))
		pathname)
	       *info-output-filename*))))))

(define (compiler:dump-bci-file binf pathname)
  (let ((bci-path (pathname-new-type pathname "bci")))
    (split-inf-structure! binf false)
    (call-with-temporary-filename
      (lambda (bif-name)
	(fasdump binf bif-name true)
	(compress bif-name bci-path)))
    (announce-info-files bci-path)))

(define (announce-info-files . files)
  (if compiler:noisy?
      (let ((port (nearest-cmdl/port)))
	(let loop ((files files))
	  (if (null? files)
	      unspecific
	      (begin
		(fresh-line port)
		(write-string ";")
		(write (->namestring (car files)))
		(write-string " dumped ")
		(loop (cdr files))))))))

(define compiler:dump-info-file compiler:dump-bci-file)

;; This defintion exported to compiler to handle losing C name restrictions

(define (canonicalize-label-name prefix)
  (if (string-null? prefix)
      "empty_string"
      (let* ((str (if (char-alphabetic? (string-ref prefix 0))
		      (string-copy prefix)
		      (string-append "Z_" prefix)))
	     (len (string-length str)))
	(do ((i 0 (1+ i)))
	    ((>= i len) str)
	  (let ((char (string-ref str i)))
	    (if (not (char-alphanumeric? char))
		(string-set! str i
			     (case char
			       ((#\?) #\P)
			       ((#\!) #\B)
			       (else #\_)))))))))