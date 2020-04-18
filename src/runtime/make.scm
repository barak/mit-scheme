#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Make Runtime System
;;; package: ()

(declare (usual-integrations))

((ucode-primitive set-interrupt-enables!) 0)

;; This must be defined as follows so that it is no part of a multi-define
;; itself.  It must also precede any other top-level definitions in this file
;; that are not performed directly using LOCAL-ASSIGNMENT.

((ucode-primitive local-assignment)
 #f ;global environment
 'define-multiple
 (lambda (env names values)
   (if (or (not (vector? names))
	   (not (vector? values))
	   (not (fix:= (vector-length names) (vector-length values))))
       (error "DEFINE-MULTIPLE: Invalid arguments" names values))
   (let ((len (vector-length names)))
     (let loop ((i 0) (val unspecific))
       (if (fix:< i len)
	   (loop (fix:+ i 1)
		 ((ucode-primitive local-assignment) env
						     (vector-ref names i)
						     (vector-ref values i)))
	   val)))))

(define system-global-environment #f)
;; This simplifies upgrading to new macros:
(define system-global-syntax-table system-global-environment)

;; *MAKE-ENVIRONMENT is referred to by compiled code.  It must go
;; before the uses of the-environment later, and after apply above.
(define (*make-environment parent names . values)
  ((ucode-primitive system-list-to-vector)
   (ucode-type environment)
   (cons ((ucode-primitive system-pair-cons)
	  (ucode-type procedure)
	  ((ucode-primitive system-pair-cons) (ucode-type lambda)
					      unspecific
					      names)
	  parent)
	 values)))

(let ((environment-for-package
       (*make-environment system-global-environment
			  (vector scode-lambda-name:unnamed))))

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
  (vector-set! fixed-objects #x0C condition-handler/hardware-trap))

(set-interrupt-enables! #x0005)

;;;; Utilities

(define (run-pkg-boot-inits package-name)
  (cond ((let ((package (find-package package-name #f)))
	   (and package
		(let ((seq (package-sequencer package)))
		  (and (not (seq 'inert?))
		       seq))))
	 => (lambda (seq)
	      (seq 'trigger!)))
	(else
	 (print-package-name "Package" package-name)
	 (tty-write-string " has no boot initialization")
	 (fatal-error "Could not initialize package"))))

(define (call-pkg-init-proc package-name procedure-name)
  (cond ((let ((package (find-package package-name #f)))
	   (and package
		(let ((env (package/environment package)))
		  (and (not (lexical-unreferenceable? env procedure-name))
		       (lexical-reference env procedure-name)))))
	 => (lambda (procedure)
	      (print-package-name "initialize:" package-name)
	      (tty-write-string " [")
	      (tty-write-string (system-pair-car procedure-name))
	      (tty-write-string "]")
	      (procedure)))
	(else
	 (print-package-name "Package" package-name)
	 (tty-write-string " is missing initialization procedure ")
	 (tty-write-string (system-pair-car procedure-name))
	 (fatal-error "Could not initialize package"))))

(define (print-package-name prefix package-name)
  (tty-write-string newline-string)
  (tty-write-string prefix)
  (tty-write-string " (")
  (let loop ((name package-name))
    (if (not (null? name))
	(begin
	  (if (not (eq? name package-name))
	      (tty-write-string " "))
	  (tty-write-string (system-pair-car (car name)))
	  (loop (cdr name)))))
  (tty-write-string ")"))

(define ((package-init-printer package))
  (print-package-name "initialize:" (package/name package)))

(define (package-reference name)
  (package/environment (find-package name)))

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

(define runtime-prefix
  "http://www.gnu.org/software/mit-scheme/lib/runtime/")

(define (file->object filename purify? required?)
  (let* ((block-name (string-append runtime-prefix filename ".so"))
	 (value (initialize-c-compiled-block block-name)))
    (cond (value
	   (tty-write-string newline-string)
	   (tty-write-string block-name)
	   (tty-write-string " initialized")
	   (remember-to-purify purify? filename value))
	  ((map-filename filename)
	   => (lambda (mapped)
		(fasload mapped purify?)))
	  (required? (fatal-error (string-append "Could not find " filename)))
	  (else #f))))

(define (eval object environment)
  (let ((value (scode-eval object environment)))
    (tty-write-string " evaluated")
    value))

(define (string-append . strings)
  (let ((result
	 (string-allocate
	  (let loop ((strings strings) (n 0))
	    (if (pair? strings)
		(loop (cdr strings) (fix:+ (string-length (car strings)) n))
		n)))))
    (let loop ((strings strings) (start 0))
      (if (pair? strings)
	  (let ((n (string-length (car strings))))
	    (substring-move-right! (car strings) 0 n result start)
	    (loop (cdr strings) (fix:+ start n)))))
    result))

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

(define fasload-purification-queue '())

(define (implemented-primitive-procedure? primitive)
  ((ucode-primitive get-primitive-address)
   (intern ((ucode-primitive get-primitive-name)
	    ((ucode-primitive object-datum) primitive)))
   #f))

(define initialize-c-compiled-block
  (let ((prim (ucode-primitive initialize-c-compiled-block 1)))
    (if (implemented-primitive-procedure? prim)
	prim
	(lambda (name) name #f))))

(define os-name
  (intern os-name-string))

(define newline-string
  (if (eq? 'unix os-name)
      "\n"
      "\r\n"))

;; Construct the package structure.
;; Lotta hair here to load the package code before its package is built.
(eval (file->object "packag" #t #t) environment-for-package)
((lexical-reference environment-for-package 'initialize-package!))
(let ((export
       (lambda (name)
	 (link-variables system-global-environment name
			 environment-for-package name))))
  (export '*allow-package-redefinition?*)
  (export 'all-packages)
  (export 'construct-packages-from-file)
  (export 'environment->package)
  (export 'find-package)
  (export 'load-package-set)
  (export 'load-packages-from-file)
  (export 'name->package)
  (export 'package-name=?)
  (export 'package-name?)
  (export 'package-set-pathname)
  (export 'package/add-child!)
  (export 'package/children)
  (export 'package/cross-compiling?)
  (export 'package/environment)
  (export 'package/name)
  (export 'package/parent)
  (export 'package/reference)
  (export 'package?))
(package/add-child! (find-package '()) 'package environment-for-package)

(define packages-file
  (let ((name
	 (string-append "runtime-"
			(cond ((eq? os-name 'nt) "w32")
			      ((eq? os-name 'unix) "unx")
			      (else "unk"))
			".pkd")))
    (or (initialize-c-compiled-block (string-append runtime-prefix name))
	(fasload name #f))))

((lexical-reference environment-for-package 'construct-packages-from-file)
 packages-file)

(define runtime-env
  (package-reference '(runtime)))

;;; Initial part of load/eval/initialize sequence is manual and carefully
;;; constructed to provide the mechanisms so that the remaining files can be
;;; loaded and evaluated.  Then dependency-based sequencing is used to
;;; initialize everything else.
(define package-sequencer)
(let ((files0
       '(("gcdemn" . (runtime gc-daemons))
	 ("gc" . (runtime garbage-collector))
	 ("boot-seq" . (runtime))
	 ("boot" . (runtime boot-definitions))
	 ("generator" . (runtime generator))
	 ("weak-pair" . (runtime weak-pair))
	 ("queue" . (runtime simple-queue))
	 ("equals" . (runtime equality))
	 ("vector" . (runtime vector))
	 ("procedure" . (runtime procedure))
	 ("list" . (runtime list))
	 ("primitive-arithmetic" . (runtime primitive-arithmetic))
	 ("srfi-1" . (runtime srfi-1))
	 ("thread-low" . (runtime thread))))
      (files1
       '(("msort" . (runtime merge-sort))
	 ("string" . (runtime string))
	 ("bytevector-low" . (runtime bytevector))
	 ("symbol" . (runtime symbol))
	 ("random" . (runtime random-number))
	 ("dispatch-tag" . (runtime tagged-dispatch))
	 ("poplat" . (runtime population))
	 ("prop1d" . (runtime 1d-property))
	 ("record" . (runtime record))))
      (files2
       '(("bundle" . (runtime bundle))
	 ("syntax-low" . (runtime syntax low))
	 ("thread" . (runtime thread))
	 ("wind" . (runtime state-space))
	 ("events" . (runtime event-distributor))
	 ("gcfinal" . (runtime gc-finalizer)))))

  (define (load-files files)
    (do ((files files (cdr files)))
	((null? files))
      (eval (file->object (car (car files)) #t #t)
	    (package-reference (cdr (car files))))))

  (load-files files0)

  ((lexical-reference runtime-env 'initialize-after-sequencers!)
   package-init-printer)

  (set! package-sequencer
	(lexical-reference runtime-env 'package-sequencer))

  (define with-current-package
    (lexical-reference runtime-env 'with-current-package))

  (define (load-files-with-boot-inits files)
    (do ((files files (cdr files)))
	((null? files))
      (load-file-with-boot-inits (car (car files))
				 (find-package (cdr (car files))))))

  (define (load-file-with-boot-inits file-name package)
    (let ((file-object (file->object file-name #t #t)))
      (with-current-package package
	(lambda ()
	  (eval file-object (package/environment package))))))

  (load-files-with-boot-inits files1)
  (call-pkg-init-proc '(runtime gc-daemons) 'initialize-package!)
  (call-pkg-init-proc '(runtime garbage-collector) 'initialize-package!)
  (run-pkg-boot-inits '(runtime population))
  (run-pkg-boot-inits '(runtime 1d-property))
  (run-pkg-boot-inits '(runtime random-number))

  (load-files-with-boot-inits files2)
  (run-pkg-boot-inits '(runtime state-space))
  (call-pkg-init-proc '(runtime thread) 'initialize-low!)
  (run-pkg-boot-inits '(runtime gc-finalizer))

  (call-pkg-init-proc '(package) 'finalize-package-record-type!)
  (call-pkg-init-proc '(runtime random-number) 'finalize-random-state-type!)

  ;; Load everything else.
  ((lexical-reference environment-for-package 'load-packages-from-file)
   packages-file
   `((sort-type . merge-sort)
     (os-type . ,os-name)
     (options . no-load))
   (let ((file-member?
	  (lambda (filename files)
	    (let loop ((files files))
	      (and (pair? files)
		   (or (string=? (car (car files)) filename)
		       (loop (cdr files))))))))
     (lambda (filename package)
       (if (not (or (string=? filename "make")
		    (string=? filename "packag")
		    (file-member? filename files0)
		    (file-member? filename files1)
		    (file-member? filename files2)))
	   (load-file-with-boot-inits filename package))
       unspecific))))

;; All remaining initizations are rooted in seq:after-files-loaded.  Triggering
;; that starts the initialization cascade.
(define seq:after-files-loaded
  (lexical-reference runtime-env 'seq:after-files-loaded))

;; Must be done manually since the ucd-tables are generated:
((package-sequencer (find-package '(runtime ucd-tables)))
 'add-before! seq:after-files-loaded)

(seq:after-files-loaded 'trigger!)

;; Done very late since it will look up lots of global variables.
(call-pkg-init-proc '(runtime library standard) 'finish-host-library-db!)

;; Last since it turns on runtime handling of microcode errors.
(call-pkg-init-proc '(runtime microcode-errors) 'initialize-error-hooks!)

(let ((obj (file->object "site" #t #f)))
  (if obj
      (eval obj system-global-environment)))

(let* ((package-env (->environment '(package)))
       (export
	(lambda (target-package names)
	  (let ((target-env (->environment target-package)))
	    (for-each (lambda (name)
			(link-variables target-env name package-env name))
		      names)))))
  (export '(runtime environment)
	  '(package-name-tag))
  (export '(runtime library standard)
	  '(link-description/inner-name
	    link-description/outer-name
	    link-description/package
	    link-description/status
	    package-description/exports
	    package-description/name
	    package-file/descriptions)))

(let ((roots
       (list->vector
	((lexical-reference (->environment '(runtime compiler-info))
			    'with-directory-rewriting-rule)
	 (working-directory-pathname)
	 (pathname-as-directory "runtime")
	 (lambda ()
	   (let ((fasload/update-debugging-info!
		  (lexical-reference (->environment '(runtime compiler-info))
				     'fasload/update-debugging-info!))
		 (load/purification-root
		  (lexical-reference (->environment '(runtime load))
				     'load/purification-root)))
	     (map (lambda (entry)
		    (let ((object (cdr entry)))
		      (fasload/update-debugging-info! object (car entry))
		      (load/purification-root object)))
		  fasload-purification-queue)))))))
  (lexical-assignment (->environment '(runtime garbage-collector))
		      'gc-boot-loading?
		      #f)
  (set! fasload-purification-queue)
  (newline (console-i/o-port))
  (write-string "purifying..." (console-i/o-port))
  ;; First, flush whatever we can.
  (gc-clean)
  ;; Then, really purify the rest.
  (purify roots #t #f)
  (write-string "done" (console-i/o-port)))

((lexical-reference (->environment '(runtime library standard))
		    'initialize-synthetic-libraries!)
 packages-file)

)

(package/add-child! (find-package '()) 'user user-initial-environment)
;; Might be better to do this sooner, to trap on floating-point
;; mistakes earlier in the cold load.
(flo:set-environment! (flo:default-environment))
(start-thread-timer)
(initial-top-level-repl)