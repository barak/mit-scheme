#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/make.scm,v 14.5 1988/10/29 00:12:33 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Make Runtime System

(declare (usual-integrations))

((ucode-primitive set-interrupt-enables!) 0)
(define system-global-environment (the-environment))

(let ((environment-for-package (let () (the-environment))))

(define-primitives
  (+ &+)
  binary-fasload
  exit
  (file-exists? 1)
  garbage-collect
  get-fixed-objects-vector
  get-next-constant
  get-primitive-address
  get-primitive-name
  lexical-reference
  microcode-identify
  primitive-purify
  scode-eval
  set-fixed-objects-vector!
  set-interrupt-enables!
  string->symbol
  string-allocate
  string-length
  substring=?
  substring-move-right!
  substring-upcase!
  tty-beep
  tty-flush-output
  tty-read-char-immediate
  tty-write-char
  tty-write-string
  vector-ref
  vector-set!
  with-interrupt-mask)

(define microcode-identification
  (microcode-identify))

(define newline-char
  (vector-ref microcode-identification 5))

(define os-name-string
  (vector-ref microcode-identification 8))

(define (fatal-error message)
  (tty-write-char newline-char)
  (tty-write-string message)
  (tty-write-char newline-char)
  (tty-flush-output)
  (exit))

(define (prompt-for-confirmation prompt)
  (let loop ()
    (tty-write-char newline-char)
    (tty-write-string prompt)
    (tty-write-string "(y or n) ")
    (tty-flush-output)
    (let ((char (tty-read-char-immediate)))
      (cond ((or (eq? #\y char)
		 (eq? #\Y char))
	     (tty-write-string "Yes")
	     (tty-flush-output)
	     true)
	    ((or (eq? #\n char)
		 (eq? #\N char))
	     (tty-write-string "No")
	     (tty-flush-output)
	     false)
	    (else
	     (tty-beep)
	     (loop))))))

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

(define (fasload filename)
  (tty-write-char newline-char)
  (tty-write-string filename)
  (tty-flush-output)
  (let ((value (binary-fasload filename)))
    (tty-write-string " loaded")
    (tty-flush-output)
    value))

(define (eval object environment)
  (let ((value (scode-eval object environment)))
    (tty-write-string " evaluated")
    (tty-flush-output)
    value))

(define (cold-load/purify object)
  (if (not (car (primitive-purify object #t safety-margin)))
      (fatal-error "Error! insufficient pure space"))
  (tty-write-string " purified")
  (tty-flush-output)
  object)

(define (implemented-primitive-procedure? primitive)
  (get-primitive-address (get-primitive-name (object-datum primitive)) false))

(define map-filename
  (if (and (implemented-primitive-procedure? file-exists?)
	   (not (prompt-for-confirmation "Load interpreted? ")))
      (lambda (filename)
	(let ((com-file (string-append filename ".com")))
	  (if (file-exists? com-file)
	      com-file
	      (string-append filename ".bin"))))
      (lambda (filename)
	(string-append filename ".bin"))))

(define (string-append x y)
  (let ((x-length (string-length x))
	(y-length (string-length y)))
    (let ((result (string-allocate (+ x-length y-length))))
      (substring-move-right! x 0 x-length result 0)
      (substring-move-right! y 0 y-length result x-length)
      result)))

(define (string-upcase string)
  (let ((size (string-length string)))
    (let ((result (string-allocate size)))
      (substring-move-right! string 0 size result 0)
      (substring-upcase! result 0 size)
      result)))

(define (string=? string1 string2)
  (substring=? string1 0 (string-length string1)
	       string2 0 (string-length string2)))

(define (package-initialize package-name procedure-name)
  (tty-write-char newline-char)
  (tty-write-string "initialize: (")
  (let loop ((name package-name))
    (if (not (null? name))
	(begin (if (not (eq? name package-name))
		   (tty-write-string " "))
	       (tty-write-string (system-pair-car (car name)))
	       (loop (cdr name)))))
  (tty-write-string ")")
  (if (not (eq? procedure-name 'INITIALIZE-PACKAGE!))
      (begin (tty-write-string " [")
	     (tty-write-string (system-pair-car procedure-name))
	     (tty-write-string "]")))
  (tty-flush-output)
  ((lexical-reference (package-reference package-name) procedure-name)))

(define (package-reference name)
  (package/environment (find-package name)))

(define (package-initialization-sequence packages)
  (let loop ((packages packages))
    (if (not (null? packages))
	(begin (package-initialize (car packages) 'INITIALIZE-PACKAGE!)
	       (loop (cdr packages))))))

;; Construct the package structure.
;; Lotta hair here to load the package code before its package is built.
(eval (cold-load/purify (fasload (map-filename "packag")))
      environment-for-package)
((access initialize-package! environment-for-package))
(let loop ((names
	    '(ENVIRONMENT->PACKAGE
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
      (begin (environment-link-name system-global-environment
				    environment-for-package
				    (car names))
	     (loop (cdr names)))))
(package/add-child! system-global-package 'PACKAGE environment-for-package)
(eval (fasload "runtim.bcon") system-global-environment)

;; Global databases.  Load, then initialize.
(let loop
    ((files
      '(("gcdemn" . (RUNTIME GC-DAEMONS))
	("poplat" . (RUNTIME POPULATION))
	("prop1d" . (RUNTIME 1D-PROPERTY))
	("events" . (RUNTIME EVENT-DISTRIBUTOR))
	("gdatab" . (RUNTIME GLOBAL-DATABASE))
	("boot" . ())
	("queue" . ())
	("gc" . (RUNTIME GARBAGE-COLLECTOR)))))
  (if (not (null? files))
      (begin
	(eval (cold-load/purify (fasload (map-filename (car (car files)))))
	      (package-reference (cdr (car files))))
	(loop (cdr files)))))
(package-initialize '(RUNTIME GC-DAEMONS) 'INITIALIZE-PACKAGE!)
(package-initialize '(RUNTIME POPULATION) 'INITIALIZE-PACKAGE!)
(package-initialize '(RUNTIME 1D-PROPERTY) 'INITIALIZE-PACKAGE!)
(package-initialize '(RUNTIME EVENT-DISTRIBUTOR) 'INITIALIZE-PACKAGE!)
(package-initialize '(RUNTIME GLOBAL-DATABASE) 'INITIALIZE-PACKAGE!)
(package-initialize '(RUNTIME POPULATION) 'INITIALIZE-UNPARSER!)
(package-initialize '(RUNTIME 1D-PROPERTY) 'INITIALIZE-UNPARSER!)
(package-initialize '(RUNTIME EVENT-DISTRIBUTOR) 'INITIALIZE-UNPARSER!)
(package-initialize '(PACKAGE) 'INITIALIZE-UNPARSER!)
(package-initialize '(RUNTIME GARBAGE-COLLECTOR) 'INITIALIZE-PACKAGE!)
(lexical-assignment (package-reference '(RUNTIME GARBAGE-COLLECTOR))
		    'CONSTANT-SPACE/BASE
		    constant-space/base)

;; Load everything else.
((eval (fasload "runtim.bldr") system-global-environment)
 (lambda (filename environment)
   (if (not (or (string=? filename "packag")
		(string=? filename "gcdemn")
		(string=? filename "poplat")
		(string=? filename "prop1d")
		(string=? filename "events")
		(string=? filename "gdatab")
		(string=? filename "boot")
		(string=? filename "queue")
		(string=? filename "gc")))
       (eval (purify (fasload (map-filename filename))) environment)))
 `((SORT-TYPE . MERGE-SORT)
   (OS-TYPE . ,(string->symbol (string-upcase os-name-string)))
   (OPTIONS . NO-LOAD)))

;; Funny stuff is done.  Rest of sequence is standardized.
(package-initialization-sequence
 '(
   ;; Microcode interface
   (RUNTIME MICROCODE-TABLES)
   (RUNTIME PRIMITIVE-IO)
   (RUNTIME SAVE/RESTORE)
   (RUNTIME STATE-SPACE)
   (RUNTIME SYSTEM-CLOCK)

   ;; Basic data structures
   (RUNTIME NUMBER)
   (RUNTIME LIST)
   (RUNTIME CHARACTER)
   (RUNTIME CHARACTER-SET)
   (RUNTIME GENSYM)
   (RUNTIME STREAM)
   (RUNTIME 2D-PROPERTY)
   (RUNTIME HASH)
   (RUNTIME RANDOM-NUMBER)

   ;; Microcode data structures
   (RUNTIME HISTORY)
   (RUNTIME LAMBDA-ABSTRACTION)
   (RUNTIME SCODE)
   (RUNTIME SCODE-COMBINATOR)
   (RUNTIME SCODE-WALKER)
   (RUNTIME CONTINUATION-PARSER)

   ;; I/O
   (RUNTIME CONSOLE-INPUT)
   (RUNTIME CONSOLE-OUTPUT)
   (RUNTIME FILE-INPUT)
   (RUNTIME FILE-OUTPUT)
   (RUNTIME STRING-INPUT)
   (RUNTIME STRING-OUTPUT)
   (RUNTIME TRUNCATED-STRING-OUTPUT)
   (RUNTIME INPUT-PORT)
   (RUNTIME OUTPUT-PORT)
   (RUNTIME WORKING-DIRECTORY)
   (RUNTIME DIRECTORY)
   (RUNTIME LOAD)

   ;; Syntax
   (RUNTIME PARSER)
   (RUNTIME NUMBER-UNPARSER)   (RUNTIME UNPARSER)
   (RUNTIME SYNTAXER)
   (RUNTIME MACROS)
   (RUNTIME SYSTEM-MACROS)
   (RUNTIME DEFSTRUCT)
   (RUNTIME UNSYNTAXER)
   (RUNTIME PRETTY-PRINTER)
   ;; REP Loops
   (RUNTIME ERROR-HANDLER)
   (RUNTIME MICROCODE-ERRORS)
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

   (RUNTIME)
   ;; Emacs -- last because it grabs the kitchen sink.
   (RUNTIME EMACS-INTERFACE)
   ))

)

(package/add-child! system-global-package 'USER user-initial-environment)(initial-top-level-repl)