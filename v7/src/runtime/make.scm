#| -*-Scheme-*-

$Id: make.scm,v 14.68 2001/05/09 13:58:54 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

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

;;;; Make Runtime System
;;; package: ()

(declare (usual-integrations))

((ucode-primitive set-interrupt-enables! 1) 0)

;; This must be defined as follows so that it is no part of a multi-define
;; itself.  It must also precede any other top-level defintiions in this file
;; that are not performed directly using LOCAL-ASSIGNMENT.

((ucode-primitive local-assignment 3)
 (the-environment)
 'DEFINE-MULTIPLE
 (named-lambda (define-multiple env names values)
   (if (or (not (vector? names))
	   (not (vector? values))
	   (not (= (vector-length names) (vector-length values))))
       (error "define-multiple: Invalid arguments" names values)
       (let ((len (vector-length names)))
	 (let loop ((i 0) (val unspecific))
	   (if (>= i len)
	       val
	       (loop (1+ i)
		     (local-assignment env
				       (vector-ref names i)
				       (vector-ref values i)))))))))

;; This definition is replaced later in the boot sequence.

(define apply (ucode-primitive apply 2))

;; This must go before the uses of the-environment later,
;; and after apply above.

(define (*make-environment parent names . values)
  (apply
   ((ucode-primitive scode-eval 2)
    #|
    (make-slambda (vector-ref names 0)
		  (subvector->list names 1 (vector-length names)))
    |#
    ((ucode-primitive system-pair-cons 3)	; &typed-pair-cons
     (ucode-type lambda)			; slambda-type
     ((ucode-primitive object-set-type 2)	; (make-the-environment)
      (ucode-type the-environment)
      0)
     names)
    parent)
   values))

(define system-global-environment (the-environment))

(define *dashed-hairy-migration-support:false-value*
  #F)

(define *dashed-hairy-migration-support:system-global-environment*
  system-global-environment)

(let ((environment-for-package (let () (the-environment))))

(define-primitives
  (+ integer-add)
  (- integer-subtract)
  (< integer-less?)
  binary-fasload
  (channel-write 4)
  environment-link-name
  exit-with-value
  (file-exists? 1)
  garbage-collect
  get-fixed-objects-vector
  get-next-constant
  get-primitive-address
  get-primitive-name
  lexical-reference
  lexical-unreferenceable?
  microcode-identify
  scode-eval
  set-fixed-objects-vector!
  set-interrupt-enables!
  string->symbol
  string-allocate
  string-length
  substring=?
  substring-move-right!
  substring-downcase!
  (tty-output-channel 0)
  vector-ref
  vector-set!
  with-interrupt-mask)

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
(define constant-space/base (get-next-constant))

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
    (if (not (null? specs))
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

(define (implemented-primitive-procedure? primitive)
  (get-primitive-address (intern (get-primitive-name (object-datum primitive)))
			 #f))

(define fasload-purification-queue
  '())

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
((access initialize-package! environment-for-package))
(let loop ((names
	    '(*ALLOW-PACKAGE-REDEFINITION?*
	      ENVIRONMENT->PACKAGE
	      FIND-PACKAGE
	      NAME->PACKAGE
	      PACKAGE/ADD-CHILD!
	      PACKAGE/CHILD
	      PACKAGE/CHILDREN
	      PACKAGE/ENVIRONMENT
	      PACKAGE/NAME
	      PACKAGE/PARENT
	      PACKAGE/REFERENCE
	      PACKAGE/SYSTEM-LOADER
	      PACKAGE?
	      SYSTEM-GLOBAL-PACKAGE)))
  (if (not (null? names))
      (begin
	(environment-link-name system-global-environment
			       environment-for-package
			       (car names))
	(loop (cdr names)))))
(package/add-child! system-global-package 'PACKAGE environment-for-package)
(eval (fasload "runtime.bco" #f) system-global-environment)

;;; Global databases.  Load, then initialize.
(let ((files1
       '(("gcdemn" . (RUNTIME GC-DAEMONS))
	 ("gc" . (RUNTIME GARBAGE-COLLECTOR))
	 ("boot" . ())
	 ("queue" . ())
	 ("equals" . ())
	 ("list" . (RUNTIME LIST))
	 ("symbol" . ())
	 ("uproc" . (RUNTIME PROCEDURE))
	 ("fixart" . ())
	 ("random" . (RUNTIME RANDOM-NUMBER))
	 ("gentag" . (RUNTIME GENERIC-PROCEDURE))
	 ("poplat" . (RUNTIME POPULATION))
	 ("record" . (RUNTIME RECORD))
	 ("defstr" . (RUNTIME DEFSTRUCT))))
      (files2
       '(("prop1d" . (RUNTIME 1D-PROPERTY))
	 ("events" . (RUNTIME EVENT-DISTRIBUTOR))
	 ("gdatab" . (RUNTIME GLOBAL-DATABASE))
	 ("gcfinal" . (RUNTIME GC-FINALIZER))))
      (load-files
       (lambda (files)
	 (do ((files files (cdr files)))
	     ((null? files))
	   (eval (file->object (car (car files)) #t #f)
		 (package-reference (cdr (car files))))))))
  (load-files files1)
  (package-initialize '(RUNTIME GC-DAEMONS) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME GARBAGE-COLLECTOR) 'INITIALIZE-PACKAGE! #t)
  (lexical-assignment (package-reference '(RUNTIME GARBAGE-COLLECTOR))
		      'CONSTANT-SPACE/BASE
		      constant-space/base)
  (package-initialize '(RUNTIME LIST) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME RANDOM-NUMBER) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME GENERIC-PROCEDURE) 'INITIALIZE-TAG-CONSTANTS!
		      #t)
  (package-initialize '(RUNTIME POPULATION) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME RECORD) 'INITIALIZE-RECORD-TYPE-TYPE! #t)
  (package-initialize '(RUNTIME DEFSTRUCT) 'INITIALIZE-STRUCTURE-TYPES! #t)
  (load-files files2)
  (package-initialize '(RUNTIME 1D-PROPERTY) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME EVENT-DISTRIBUTOR) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME GLOBAL-DATABASE) 'INITIALIZE-PACKAGE! #t)
  (package-initialize '(RUNTIME POPULATION) 'INITIALIZE-UNPARSER! #t)
  (package-initialize '(RUNTIME 1D-PROPERTY) 'INITIALIZE-UNPARSER! #t)
  (package-initialize '(RUNTIME GC-FINALIZER) 'INITIALIZE-PACKAGE! #t)

;; Load everything else.
;; Note: The following code needs MAP* and MEMBER-PROCEDURE
;; from runtime/list. Fortunately that file has already been loaded.

  ((eval (fasload "runtime.bld" #f) system-global-environment)
   (let ((to-avoid
	  (cons "packag"
		(map* (if (file-exists? "runtime.bad")
			  (fasload "runtime.bad" #f)
			  '())
		      car
		      (append files1 files2))))
	 (string-member? (member-procedure string=?)))
     (lambda (filename environment)
       (if (not (string-member? filename to-avoid))
	   (eval (file->object filename #t #f) environment))
       unspecific))
   `((SORT-TYPE . MERGE-SORT)
     (OS-TYPE . ,os-name)
     (OPTIONS . NO-LOAD))))

;;; Funny stuff is done.  Rest of sequence is standardized.
(package-initialization-sequence
 '(
   ;; Microcode interface
   ((RUNTIME MICROCODE-TABLES) READ-MICROCODE-TABLES! #t)
   (RUNTIME STATE-SPACE)
   (RUNTIME APPLY)
   (RUNTIME HASH)			; First GC daemon!
   (RUNTIME PRIMITIVE-IO)
   (RUNTIME SAVE/RESTORE)
   (RUNTIME SYSTEM-CLOCK)
   ((RUNTIME GC-FINALIZER) INITIALIZE-EVENTS! #t)
   ;; Basic data structures
   (RUNTIME NUMBER)
   (RUNTIME CHARACTER)
   (RUNTIME CHARACTER-SET)
   (RUNTIME GENSYM)
   (RUNTIME STREAM)
   (RUNTIME 2D-PROPERTY)
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
   (() INITIALIZE-SYSTEM-PRIMITIVES! #f)
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
   ;; This MUST be done before (RUNTIME PATHNAME) 
   (RUNTIME PATHNAME UNIX)
   (RUNTIME PATHNAME)
   (RUNTIME WORKING-DIRECTORY)
   (RUNTIME LOAD)
   ;; Syntax
   (RUNTIME NUMBER-PARSER)
   (RUNTIME PARSER)
   (RUNTIME UNPARSER)
   (RUNTIME SYNTAXER)
   (RUNTIME ILLEGAL-DEFINITIONS)
   (RUNTIME MACROS)
   (RUNTIME SYSTEM-MACROS)
   ((RUNTIME DEFSTRUCT) INITIALIZE-DEFINE-STRUCTURE-MACRO! #t)
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
   (RUNTIME STRING)
   ;; Emacs -- last because it installs hooks everywhere which must be initted.
   (RUNTIME EMACS-INTERFACE)
   ;; More debugging
   ((RUNTIME CONTINUATION-PARSER) INITIALIZE-SPECIAL-FRAMES! #f)))

(let ((obj (file->object "site" #t #t)))
  (if obj
      (eval obj system-global-environment)))

(environment-link-name (->environment '(RUNTIME ENVIRONMENT))
		       (->environment '(PACKAGE))
		       'PACKAGE-NAME-TAG)

(let ((roots
       (list->vector
	((access with-directory-rewriting-rule
		 (->environment '(RUNTIME COMPILER-INFO)))
	 (working-directory-pathname)
	 (pathname-as-directory "runtime")
	 (lambda ()
	   (let ((fasload/update-debugging-info!
		  (access fasload/update-debugging-info!
			  (->environment '(RUNTIME COMPILER-INFO))))
		 (load/purification-root
		  (access load/purification-root
			  (->environment '(RUNTIME LOAD)))))
	     (map (lambda (entry)
		    (let ((object (cdr entry)))
		      (fasload/update-debugging-info! object (car entry))
		      (load/purification-root object)))
		  fasload-purification-queue)))))))
  (set! (access gc-boot-loading? (->environment '(RUNTIME GARBAGE-COLLECTOR)))
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