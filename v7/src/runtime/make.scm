#| -*-Scheme-*-

$Id: make.scm,v 14.105 2006/09/16 11:19:09 gjr Exp $

Copyright 1988,1989,1990,1991,1992,1993 Massachusetts Institute of Technology
Copyright 1994,1995,1996,1997,1998,2000 Massachusetts Institute of Technology
Copyright 2001,2002,2003,2004,2005,2006 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Make Runtime System
;;; package: ()

(declare (usual-integrations))

(set-interrupt-enables! 0)

;; This must be defined as follows so that it is no part of a multi-define
;; itself.  It must also precede any other top-level definitions in this file
;; that are not performed directly using LOCAL-ASSIGNMENT.

(local-assignment
 #f ;global environment
 'DEFINE-MULTIPLE
 (lambda (env names values)
   (if (or (not (vector? names))
	   (not (vector? values))
	   (not (fix:= (vector-length names) (vector-length values))))
       (error "DEFINE-MULTIPLE: Invalid arguments" names values))
   (let ((len (vector-length names)))
     (let loop ((i 0) (val unspecific))
       (if (fix:< i len)
	   (loop (fix:+ i 1)
		 (local-assignment env
				   (vector-ref names i)
				   (vector-ref values i)))
	   val)))))

(define system-global-environment #f)
;; This simplifies upgrading to new macros:
(define system-global-syntax-table system-global-environment)

;; *MAKE-ENVIRONMENT is referred to by compiled code.  It must go
;; before the uses of the-environment later, and after apply above.
(define (*make-environment parent names . values)
  (let-syntax
      ((ucode-type
	(sc-macro-transformer
	 (lambda (form environment)
	   environment
	   (microcode-type (cadr form))))))
    (system-list->vector
     (ucode-type environment)
     (cons (system-pair-cons (ucode-type procedure)
			     (system-pair-cons (ucode-type lambda)
					       unspecific
					       names)
			     parent)
	   values))))

(let ((environment-for-package
       (*make-environment system-global-environment
			  (vector lambda-tag:unnamed))))

(define-syntax ucode-primitive
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (apply make-primitive-procedure (cdr form)))))

(define-syntax ucode-type
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (microcode-type (cadr form)))))

(define-integrable + (ucode-primitive integer-add))
(define-integrable - (ucode-primitive integer-subtract))
(define-integrable < (ucode-primitive integer-less?))
(define-integrable binary-fasload (ucode-primitive binary-fasload))
(define-integrable channel-write (ucode-primitive channel-write 4))
(define-integrable exit-with-value (ucode-primitive exit-with-value))
(define-integrable file-exists? (ucode-primitive file-exists? 1))
(define-integrable garbage-collect (ucode-primitive garbage-collect))
(define-integrable lexical-reference (ucode-primitive lexical-reference))
(define-integrable link-variables (ucode-primitive link-variables 4))
(define-integrable microcode-identify (ucode-primitive microcode-identify))
(define-integrable scode-eval (ucode-primitive scode-eval))
(define-integrable string->symbol (ucode-primitive string->symbol))
(define-integrable string-allocate (ucode-primitive string-allocate))
(define-integrable string-length (ucode-primitive string-length))
(define-integrable substring=? (ucode-primitive substring=?))
(define-integrable substring-downcase! (ucode-primitive substring-downcase!))
(define-integrable tty-output-channel (ucode-primitive tty-output-channel 0))
(define-integrable vector-ref (ucode-primitive vector-ref))
(define-integrable vector-set! (ucode-primitive vector-set!))
(define-integrable with-interrupt-mask (ucode-primitive with-interrupt-mask))

(define-integrable get-fixed-objects-vector
  (ucode-primitive get-fixed-objects-vector))

(define-integrable lexical-unreferenceable?
  (ucode-primitive lexical-unreferenceable?))

(define-integrable set-fixed-objects-vector!
  (ucode-primitive set-fixed-objects-vector!))

(define-integrable set-interrupt-enables!
  (ucode-primitive set-interrupt-enables!))

(define-integrable substring-move-right!
  (ucode-primitive substring-move-right!))

(define microcode-identification (microcode-identify))
(define os-name-string (vector-ref microcode-identification 8))
(define tty-output-descriptor (tty-output-channel))

(define (tty-write-string string)
  (let ((end (string-length string)))
    (let loop ((start 0) (n-left end))
      (let ((n (channel-write tty-output-descriptor string start end)))
	(cond ((not n) (loop start n-left))
	      ((< n n-left) (loop (+ start n) (- n-left n))))))))

(define (fatal-error message)
  (tty-write-string newline-string)
  (tty-write-string message)
  (tty-write-string newline-string)
  (exit-with-value 1))

;;;; GC, Interrupts, Errors

(define safety-margin 4500)

(let ((condition-handler/gc
       (lambda (interrupt-code interrupt-enables)
	 interrupt-code interrupt-enables
	 (with-interrupt-mask 0
	   (lambda (interrupt-mask)
	     interrupt-mask
	     (garbage-collect safety-margin)))))
      (condition-handler/stack-overflow
       (lambda (interrupt-code interrupt-enables)
	 interrupt-code interrupt-enables
	 (fatal-error "Stack overflow!")))
      (condition-handler/hardware-trap
       (lambda (escape-code)
	 escape-code
	 (fatal-error "Hardware trap!")))
      (fixed-objects (get-fixed-objects-vector)))
  (let ((interrupt-vector (vector-ref fixed-objects 1)))
    (vector-set! interrupt-vector 0 condition-handler/stack-overflow)
    (vector-set! interrupt-vector 2 condition-handler/gc))
  (vector-set! fixed-objects #x0C condition-handler/hardware-trap)
  (set-fixed-objects-vector! fixed-objects))

(set-interrupt-enables! #x0005)

;;;; Utilities

(define (package-initialize package-name procedure-name mandatory?)
  (define (print-name string)
    (tty-write-string newline-string)
    (tty-write-string string)
    (tty-write-string " (")
    (let loop ((name package-name))
      (if (not (null? name))
	  (begin
	    (if (not (eq? name package-name))
		(tty-write-string " "))
	    (tty-write-string (system-pair-car (car name)))
	    (loop (cdr name)))))
    (tty-write-string ")"))

  (let ((env (package-reference package-name)))
    (cond ((not (lexical-unreferenceable? env procedure-name))
	   (print-name "initialize:")
	   (if (not (eq? procedure-name 'INITIALIZE-PACKAGE!))
	       (begin
		 (tty-write-string " [")
		 (tty-write-string (system-pair-car procedure-name))
		 (tty-write-string "]")))
	   ((lexical-reference env procedure-name)))
	  ((not mandatory?)
	   (print-name "* skipping:"))
	  (else
	   ;; Missing mandatory package! Report it and die.
	   (print-name "Package")
	   (tty-write-string " is missing initialization procedure ")
	   (tty-write-string (system-pair-car procedure-name))
	   (fatal-error "Could not initialize a required package.")))))

(define (package-reference name)
  (package/environment (find-package name)))

(define (package-initialization-sequence specs)
  (let loop ((specs specs))
    (if (pair? specs)
	(let ((spec (car specs)))
	  (if (or (not (pair? spec))
		  (symbol? (car spec)))
	      (package-initialize spec 'INITIALIZE-PACKAGE! #f)
	      (package-initialize (car spec) (cadr spec) (caddr spec)))
	  (loop (cdr specs))))))

(define (remember-to-purify purify? filename value)
  (if purify?
      (set! fasload-purification-queue
	    (cons (cons filename value)
		  fasload-purification-queue)))
  value)

(define (fasload filename purify?)
  (tty-write-string newline-string)
  (tty-write-string filename)
  (let ((value (binary-fasload filename)))
    (tty-write-string " loaded")
    (remember-to-purify purify? filename value)))

(define (map-filename filename)
  (let ((com-file (string-append filename ".com")))
    (if (file-exists? com-file)
	com-file
	(let ((bin-file (string-append filename ".bin")))
	  (and (file-exists? bin-file)
	       bin-file)))))

(define (file->object filename purify? optional?)
  (let* ((block-name (string-append "runtime_" filename))
	 (value (initialize-c-compiled-block block-name)))
    (cond (value
	   (tty-write-string newline-string)
	   (tty-write-string block-name)
	   (tty-write-string " initialized")
	   (remember-to-purify purify? filename value))
	  ((map-filename filename)
	   => (lambda (mapped)
		(fasload mapped purify?)))
	  ((not optional?)
	   (fatal-error (string-append "Could not find " filename)))
	  (else
	   #f))))

(define (eval object environment)
  (let ((value (scode-eval object environment)))
    (tty-write-string " evaluated")
    value))

(define (string-append x y)
  (let ((x-length (string-length x))
	(y-length (string-length y)))
    (let ((result (string-allocate (+ x-length y-length))))
      (substring-move-right! x 0 x-length result 0)
      (substring-move-right! y 0 y-length result x-length)
      result)))

(define (string-downcase string)
  (let ((size (string-length string)))
    (let ((result (string-allocate size)))
      (substring-move-right! string 0 size result 0)
      (substring-downcase! result 0 size)
      result)))

(define (string=? string1 string2)
  (substring=? string1 0 (string-length string1)
	       string2 0 (string-length string2)))

(define (intern string)
  (string->symbol (string-downcase string)))

(define fasload-purification-queue
  '())

(define (implemented-primitive-procedure? primitive)
  ((ucode-primitive get-primitive-address)
   (intern ((ucode-primitive get-primitive-name) (object-datum primitive)))
   #f))

(define initialize-c-compiled-block
  (let ((prim (ucode-primitive initialize-c-compiled-block 1)))
    (if (implemented-primitive-procedure? prim)
	prim
	(lambda (name)
	  name				; ignored
	  #f))))

(define os-name
  (intern os-name-string))

(define newline-string
  (if (eq? 'UNIX os-name)
      "\n"
      "\r\n"))

;; Construct the package structure.
;; Lotta hair here to load the package code before its package is built.
(eval (file->object "packag" #t #f) environment-for-package)
((lexical-reference environment-for-package 'INITIALIZE-PACKAGE!))
(let ((export
       (lambda (name)
	 (link-variables system-global-environment name
			 environment-for-package name))))
  (export '*ALLOW-PACKAGE-REDEFINITION?*)
  (export 'CONSTRUCT-PACKAGES-FROM-FILE)
  (export 'ENVIRONMENT->PACKAGE)
  (export 'FIND-PACKAGE)
  (export 'LOAD-PACKAGE-SET)
  (export 'LOAD-PACKAGES-FROM-FILE)
  (export 'NAME->PACKAGE)
  (export 'PACKAGE-SET-PATHNAME)
  (export 'PACKAGE/ADD-CHILD!)
  (export 'PACKAGE/CHILD)
  (export 'PACKAGE/CHILDREN)
  (export 'PACKAGE/ENVIRONMENT)
  (export 'PACKAGE/NAME)
  (export 'PACKAGE/PARENT)
  (export 'PACKAGE/REFERENCE)
  (export 'PACKAGE?)
  (export 'SYSTEM-GLOBAL-PACKAGE))
(package/add-child! system-global-package 'PACKAGE environment-for-package)

(define packages-file
  (let ((name (cond ((eq? os-name 'NT) "runtime-w32")
		    ((eq? os-name 'OS/2) "runtime-os2")
		    ((eq? os-name 'UNIX) "runtime-unx")
		    (else "runtime-unk"))))
    (or (initialize-c-compiled-block (string-append "runtime_" name))
	(fasload (string-append name ".pkd") #f))))

((lexical-reference environment-for-package 'CONSTRUCT-PACKAGES-FROM-FILE)
 packages-file)

;;; Global databases.  Load, then initialize.
(let ((files1
       '(("gcdemn" . (RUNTIME GC-DAEMONS))
	 ("gc" . (RUNTIME GARBAGE-COLLECTOR))
	 ("boot" . (RUNTIME BOOT-DEFINITIONS))
	 ("queue" . (RUNTIME SIMPLE-QUEUE))
	 ("equals" . (RUNTIME EQUALITY))
	 ("list" . (RUNTIME LIST))
	 ("symbol" . (RUNTIME SYMBOL))
	 ("uproc" . (RUNTIME PROCEDURE))
	 ("fixart" . (RUNTIME FIXNUM-ARITHMETIC))
	 ("random" . (RUNTIME RANDOM-NUMBER))
	 ("gentag" . (RUNTIME GENERIC-PROCEDURE))
	 ("poplat" . (RUNTIME POPULATION))
	 ("record" . (RUNTIME RECORD))
	 ("syntax-transforms" . (RUNTIME SYNTACTIC-CLOSURES))))
      (files2
       '(("prop1d" . (RUNTIME 1D-PROPERTY))
	 ("events" . (RUNTIME EVENT-DISTRIBUTOR))
	 ("gdatab" . (RUNTIME GLOBAL-DATABASE))
	 ("gcfinal" . (RUNTIME GC-FINALIZER))
	 ("string" . (RUNTIME STRING))))
      (load-files
       (lambda (files)
	 (do ((files files (cdr files)))
	     ((null? files))
	   (eval (file->object (car (car files)) #t #f)
		 (package-reference (cdr (car files))))))))
  (load-files files1)
  (package-initialize '(RUNTIME GC-DAEMONS) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME GARBAGE-COLLECTOR) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME RANDOM-NUMBER) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME GENERIC-PROCEDURE) 'INITIALIZE-TAG-CONSTANTS!
		      #t)
  (package-initialize '(RUNTIME POPULATION) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME RECORD) 'INITIALIZE-RECORD-TYPE-TYPE! #t)
  (package-initialize '(RUNTIME SYNTACTIC-CLOSURES)
		      'INITIALIZE-SYNTAX-TRANSFORMS!
		      #t)
  (load-files files2)
  (package-initialize '(RUNTIME 1D-PROPERTY) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME EVENT-DISTRIBUTOR) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME GLOBAL-DATABASE) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME POPULATION) 'INITIALIZE-UNPARSER! #t)
  (package-initialize '(RUNTIME 1D-PROPERTY) 'INITIALIZE-UNPARSER! #t)
  (package-initialize '(RUNTIME GC-FINALIZER) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME STRING) 'INITIALIZE-PACKAGE! #t)

  ;; Load everything else.
  ((lexical-reference environment-for-package 'LOAD-PACKAGES-FROM-FILE)
   packages-file
   `((SORT-TYPE . MERGE-SORT)
     (OS-TYPE . ,os-name)
     (OPTIONS . NO-LOAD))
   (let ((file-member?
	  (lambda (filename files)
	    (let loop ((files files))
	      (and (pair? files)
		   (or (string=? (car (car files)) filename)
		       (loop (cdr files))))))))
     (lambda (filename environment)
       (if (not (or (string=? filename "make")
		    (string=? filename "packag")
		    (file-member? filename files1)
		    (file-member? filename files2)))
	   (eval (file->object filename #t #f)
		 environment))
       unspecific))))

;;; Funny stuff is done.  Rest of sequence is standardized.
(package-initialization-sequence
 '(
   ;; Microcode interface
   ((RUNTIME MICROCODE-TABLES) READ-MICROCODE-TABLES! #t)
   (RUNTIME STATE-SPACE)
   (RUNTIME APPLY)
   (RUNTIME HASH)			; First GC daemon!
   (RUNTIME PRIMITIVE-IO)
   (RUNTIME SYSTEM-CLOCK)
   ((RUNTIME GC-FINALIZER) INITIALIZE-EVENTS! #t)
   ;; Basic data structures
   (RUNTIME NUMBER)
   ((RUNTIME NUMBER) INITIALIZE-DRAGON4! #t)
   (RUNTIME MISCELLANEOUS-GLOBAL)
   (RUNTIME CHARACTER)
   (RUNTIME CHARACTER-SET)
   (RUNTIME GENSYM)
   (RUNTIME STREAM)
   (RUNTIME 2D-PROPERTY)
   (RUNTIME HASH-TABLE)
   ;; Microcode data structures
   (RUNTIME HISTORY)
   (RUNTIME LAMBDA-ABSTRACTION)
   (RUNTIME SCODE)
   (RUNTIME SCODE-COMBINATOR)
   (RUNTIME SCODE-WALKER)
   (RUNTIME CONTINUATION-PARSER)
   (RUNTIME PROGRAM-COPIER)
   ;; Generic Procedures
   ((RUNTIME GENERIC-PROCEDURE EQHT) INITIALIZE-ADDRESS-HASHING! #t)
   ((RUNTIME GENERIC-PROCEDURE) INITIALIZE-GENERIC-PROCEDURES! #t)
   ((RUNTIME GENERIC-PROCEDURE MULTIPLEXER) INITIALIZE-MULTIPLEXER! #t)
   ((RUNTIME TAGGED-VECTOR) INITIALIZE-TAGGED-VECTOR! #t)
   ((RUNTIME RECORD-SLOT-ACCESS) INITIALIZE-RECORD-SLOT-ACCESS! #t)
   ((RUNTIME RECORD) INITIALIZE-RECORD-PROCEDURES! #t)
   ((PACKAGE) FINALIZE-PACKAGE-RECORD-TYPE! #t)
   ((RUNTIME RANDOM-NUMBER) FINALIZE-RANDOM-STATE-TYPE! #t)
   ;; Condition System
   (RUNTIME ERROR-HANDLER)
   (RUNTIME MICROCODE-ERRORS)
   ((RUNTIME GENERIC-PROCEDURE) INITIALIZE-CONDITIONS! #t)
   ((RUNTIME GENERIC-PROCEDURE MULTIPLEXER) INITIALIZE-CONDITIONS! #t)
   ((RUNTIME RECORD-SLOT-ACCESS) INITIALIZE-CONDITIONS! #t)
   ((RUNTIME STREAM) INITIALIZE-CONDITIONS! #t)
   ;; System dependent stuff
   ((RUNTIME OS-PRIMITIVES) INITIALIZE-SYSTEM-PRIMITIVES! #t)
   ;; Threads
   (RUNTIME THREAD)
   ;; I/O
   (RUNTIME GENERIC-I/O-PORT)
   (RUNTIME FILE-I/O-PORT)
   (RUNTIME CONSOLE-I/O-PORT)
   (RUNTIME TRANSCRIPT)
   (RUNTIME STRING-INPUT)
   (RUNTIME STRING-OUTPUT)
   (RUNTIME TRUNCATED-STRING-OUTPUT)
   ;; These MUST be done before (RUNTIME PATHNAME) 
   ;; Typically only one of them is loaded.
   (RUNTIME PATHNAME UNIX)
   (RUNTIME PATHNAME DOS)
   (RUNTIME PATHNAME)
   (RUNTIME WORKING-DIRECTORY)
   (RUNTIME LOAD)
   (RUNTIME UNICODE)
   (RUNTIME SIMPLE-FILE-OPS)
   ((RUNTIME OS-PRIMITIVES) INITIALIZE-MIME-TYPES! #f)
   ;; Syntax
   (RUNTIME NUMBER-PARSER)
   (RUNTIME PARSER)
   ((RUNTIME PATHNAME) INITIALIZE-PARSER-METHOD! #t)
   (RUNTIME UNPARSER)
   (RUNTIME UNSYNTAXER)
   (RUNTIME PRETTY-PRINTER)
   (RUNTIME EXTENDED-SCODE-EVAL)
   ;; REP Loops
   (RUNTIME INTERRUPT-HANDLER)
   (RUNTIME GC-STATISTICS)
   (RUNTIME REP)
   ;; Debugging
   (RUNTIME COMPILER-INFO)
   (RUNTIME ADVICE)
   (RUNTIME DEBUGGER-COMMAND-LOOP)
   (RUNTIME DEBUGGER-UTILITIES)
   (RUNTIME ENVIRONMENT-INSPECTOR)
   (RUNTIME DEBUGGING-INFO)
   (RUNTIME DEBUGGER)
   ;; Misc (e.g., version)
   (RUNTIME)
   (RUNTIME CRYPTO)
   ;; Graphics.  The last type initialized is the default for
   ;; MAKE-GRAPHICS-DEVICE, only the types that are valid for the
   ;; operating system are actually loaded and initialized.
   (RUNTIME STARBASE-GRAPHICS)
   (RUNTIME X-GRAPHICS)
   (RUNTIME OS2-GRAPHICS)
   ;; Emacs -- last because it installs hooks everywhere which must be initted.
   (RUNTIME EMACS-INTERFACE)
   ;; More debugging
   ((RUNTIME CONTINUATION-PARSER) INITIALIZE-SPECIAL-FRAMES! #f)
   (RUNTIME URI)
   (RUNTIME HTTP-CLIENT)))

(if (eq? os-name 'NT)
    (package-initialize '(RUNTIME WIN32-REGISTRY) 'INITIALIZE-PACKAGE! #f))

(let ((obj (file->object "site" #t #t)))
  (if obj
      (eval obj system-global-environment)))

(link-variables (->environment '(RUNTIME ENVIRONMENT)) 'PACKAGE-NAME-TAG
		(->environment '(PACKAGE)) 'PACKAGE-NAME-TAG)

(let ((roots
       (list->vector
	((lexical-reference (->environment '(RUNTIME COMPILER-INFO))
			    'WITH-DIRECTORY-REWRITING-RULE)
	 (working-directory-pathname)
	 (pathname-as-directory "runtime")
	 (lambda ()
	   (let ((fasload/update-debugging-info!
		  (lexical-reference (->environment '(RUNTIME COMPILER-INFO))
				     'FASLOAD/UPDATE-DEBUGGING-INFO!))
		 (load/purification-root
		  (lexical-reference (->environment '(RUNTIME LOAD))
				     'LOAD/PURIFICATION-ROOT)))
	     (map (lambda (entry)
		    (let ((object (cdr entry)))
		      (fasload/update-debugging-info! object (car entry))
		      (load/purification-root object)))
		  fasload-purification-queue)))))))
  (lexical-assignment (->environment '(RUNTIME GARBAGE-COLLECTOR))
		      'GC-BOOT-LOADING?
		      #f)
  (set! fasload-purification-queue)
  (newline console-output-port)
  (write-string "purifying..." console-output-port)
  ;; First, flush whatever we can.
  (gc-clean)
  ;; Then, really purify the rest.
  (purify roots #t #f)
  (write-string "done" console-output-port))

)

(package/add-child! system-global-package 'USER user-initial-environment)
(start-thread-timer)
(initial-top-level-repl)