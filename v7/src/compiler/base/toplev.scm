#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/toplev.scm,v 4.8 1988/07/16 21:51:09 cph Exp $

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

;;;; Compiler Top Level

(declare (usual-integrations))

;;; Global variables

(define *recursive-compilation-count*)
(define *recursive-compilation-number*)
(define *recursive-compilation-results*)
(define *recursive-compilation-rtl-blocks*)

(define *info-output-pathname* false)
(define *rtl-output-pathname* false)

(define *input-scode*)
(define *scode*)
(define *ic-procedure-headers*)
(define *root-block*)
(define *root-expression*)
(define *rtl-expression*)
(define *rtl-procedures*)
(define *rtl-continuations*)
(define *rtl-graphs*)
(define label->object)

;;; These variable names mistakenly use the format "compiler:..."
;;; instead of the correct format, which is "*...*".  Fix it sometime.
(define compiler:external-labels)
(define compiler:label-bindings)
(define compiler:block-label)
(define compiler:entry-label)
(define compiler:bits)
(define compiler:code-vector)
(define compiler:entry-points)
(define compiler:expression)

(define compiler:phase-wrapper false)
(define compiler:process-time 0)
(define compiler:real-time 0)

(define (compiler:reset!)
  (set! *recursive-compilation-number* 0)
  (set! *recursive-compilation-count* 1)
  (set! *recursive-compilation-results* '())
  (set! *recursive-compilation-rtl-blocks* '())
  (set! *input-scode*)
  (set! *scode*)
  (set! *current-label-number*)
  (set! *constants*)
  (set! *blocks*)
  (set! *expressions*)
  (set! *procedures*)
  (set! *lvalues*)
  (set! *applications*)
  (set! *parallels*)
  (set! *assignments*)
  (set! *ic-procedure-headers*)
  (set! *root-expression*)
  (set! *root-block*)
  (set! *rtl-expression*)
  (set! *rtl-procedures*)
  (set! *rtl-continuations*)
  (set! *rtl-graphs*)
  (set! label->object)
  (set! *machine-register-map*)
  (set! compiler:external-labels)
  (set! compiler:label-bindings)
  (set! compiler:block-label)
  (set! compiler:entry-label)
  (set! compiler:bits)
  (set! compiler:code-vector)
  (set! compiler:entry-points)
  (set! compiler:expression))

(define (in-compiler-recursively thunk)
  (fluid-let ((*input-scode*)
	      (*scode*)
	      (*current-label-number*)
	      (*constants*)
	      (*blocks*)
	      (*expressions*)
	      (*procedures*)
	      (*lvalues*)
	      (*applications*)
	      (*parallels*)
	      (*assignments*)
	      (*ic-procedure-headers*)
	      (*root-expression*)
	      (*root-block*))
    (fluid-let ((*rtl-expression*)
		(*rtl-procedures*)
		(*rtl-continuations*)
		(*rtl-graphs*)
		(label->object)
		(*machine-register-map*)
		(compiler:external-labels)
		(compiler:label-bindings)
		(compiler:block-label)
		(compiler:entry-label)
		(compiler:bits)
		(compiler:code-vector)
		(compiler:entry-points)
		(compiler:expression))
      (thunk))))

(define (in-compiler thunk)
  (fluid-let ((compiler:process-time 0)
	      (compiler:real-time 0))
    (compiler:reset!)
    (let*  ((topl (thunk))
	    (value
	     (generate-top-level-object topl *recursive-compilation-results*)))
      (if (not compiler:preserve-data-structures?)
	  (compiler:reset!))
      (compiler-time-report "Total compilation time"
			    compiler:process-time
			    compiler:real-time)
      value)))

;;;; The file compiler, its usual mode.

(define (cf input #!optional output)
  (let ((kernel
	 (lambda (source-file)
	     (let ((scode-file
		    (merge-pathnames
		     (make-pathname false false false false "bin" false)
		     (->pathname source-file))))
	       ;; Maybe this should be done only if scode-file
	       ;; does not exist or is older than source-file.
	       (sf source-file scode-file)
	       (newline)
	       (if (default-object? output)
		   (compile-bin-file scode-file)
		   (compile-bin-file scode-file output))))))
    (if (pair? input)
	(for-each kernel input)
	(kernel input))))

(define (compile-bin-file input-string #!optional output-string)
  (compiler-pathnames input-string
		      (and (not (default-object? output-string)) output-string)
		      (make-pathname false false false false "bin" 'NEWEST)
    (lambda (input-pathname output-pathname)
      (compile-scode (compiler-fasload input-pathname)
		     (and compiler:generate-rtl-files?
			  (pathname-new-type output-pathname "brtl"))
		     (pathname-new-type output-pathname "binf")))))

;;;; Utilities for compiling in batch mode

(define compiler:batch-mode? false)
(define compiler:abort-handled? false)
(define compiler:abort-continuation)

(define (compiler:batch-compile input #!optional output)
  (fluid-let ((compiler:batch-mode? true))
    (bind-condition-handler '() compiler:batch-error-handler
      (lambda ()
	(if (default-object? output)
	    (compile-bin-file input)
	    (compile-bin-file input output))))))

(define (compiler:batch-error-handler condition)
  (and (condition/error? condition)
       (begin (warn (condition/report-string condition))
	      (compiler:abort false))))

(define (compiler:abort value)
  (if compiler:abort-handled?
      (begin
	(newline)
	(display "*** Aborting...")
	(compiler:abort-continuation value))
      (error "compiler:abort: Not set up to abort" value)))

(define (batch-kernel real-kernel)
  (lambda (input-string)
    (call-with-current-continuation
     (lambda (abort-compilation)
       (fluid-let ((compiler:abort-continuation abort-compilation)
		   (compiler:abort-handled? true))
	 (real-kernel input-string))))))

(define (compiler-pathnames input-string output-string default transform)
  (let* ((core
	  (lambda (input-string)
	    (let ((input-pathname
		   (pathname->input-truename
		    (merge-pathnames (->pathname input-string) default))))
	      (if (not input-pathname)
		  (error "File does not exist" input-string))
	      (let ((output-pathname
		     (let ((output-pathname
			    (pathname-new-type input-pathname "com")))
		       (if output-string
			   (merge-pathnames (->pathname output-string)
					    output-pathname)
			   output-pathname))))
		(newline)
		(write-string "Compile File: ")
		(write (pathname->string input-pathname))
		(write-string " => ")
		(write (pathname->string output-pathname))
		(fasdump (transform input-pathname output-pathname)
			 output-pathname)))))
	 (kernel
	  (if compiler:batch-mode?
	      (batch-kernel core)
	      core)))
    (if (pair? input-string)
	(for-each kernel input-string)
	(kernel input-string))))

(define (compiler-fasload pathname)
  (let ((scode
	 (let ((scode (fasload pathname)))
	   (if (scode/comment? scode)
	       (scode/comment-expression scode)
	       scode))))
    (if (scode/open-block? scode)
	(scode/open-block-components scode
	  (lambda (names declarations body)
	    (if (null? names)
		(scan-defines body
		  (lambda (names declarations* body)
		    (make-open-block names
				     (append declarations declarations*)
				     body)))
		scode)))
	(scan-defines scode make-open-block))))

(define (compile-procedure procedure)
  (scode-eval (compile-scode (procedure-lambda procedure) false false)
	      (procedure-environment procedure)))

;; The rtl output should be fixed

(define (compile-recursively scode)
  (let ((my-number *recursive-compilation-count*))
    (set! *recursive-compilation-count* (1+ my-number))
    (newline)
    (newline)
    (display "    *** Recursive compilation ")
    (write my-number)
    (display " ***")
    (let ((val
	   (fluid-let ((*recursive-compilation-number* my-number)
		       (compiler:package-optimization-level 'NONE))
	     (compile-scode scode
			    (and *rtl-output-pathname* true)
			    (and *info-output-pathname* true)
			    in-compiler-recursively))))
      (newline)
      (display "    *** Done with recursive compilation ")
      (write my-number)
      (display " ***")
      (newline)
      val)))

(define (compile-scode scode
		       #!optional
		       rtl-output-pathname
		       info-output-pathname
		       wrapper)

  (if (default-object? rtl-output-pathname)
      (set! rtl-output-pathname false))
  (if (default-object? info-output-pathname)
      (set! info-output-pathname false))

  (fluid-let ((*info-output-pathname*
	       (if (and info-output-pathname
			(not (eq? info-output-pathname true)))
		   info-output-pathname
		   *info-output-pathname*))
	      (*rtl-output-pathname*
	       (if (and rtl-output-pathname
			(not (eq? rtl-output-pathname true)))
		   rtl-output-pathname
		   *rtl-output-pathname*)))
    ((if (default-object? wrapper)
	 in-compiler
	 wrapper)
     (lambda ()
       (set! *input-scode* scode)
       (phase/fg-generation)
       (phase/fg-optimization)
       (phase/rtl-generation)
       #|
       (if info-output-pathname
	   (phase/info-generation-1 info-output-pathname))
       |#
       (phase/rtl-optimization)
       (if rtl-output-pathname
	   (phase/rtl-file-output rtl-output-pathname))
       (phase/bit-generation)
       (phase/bit-linearization)
       (phase/assemble)
       (if info-output-pathname
	   (phase/info-generation-2 info-output-pathname))
       (phase/link)
       compiler:expression))))

(define (compiler-phase name thunk)
  (compiler-phase/visible name
    (lambda ()
      (compiler-phase/invisible thunk))))

(define (compiler-superphase name thunk)
  (if compiler:show-subphases?
      (thunk)
      (compiler-phase/visible name thunk)))

(define (compiler-subphase name thunk)
  (if compiler:show-subphases?
      (compiler-phase name thunk)
      (compiler-phase/invisible thunk)))

(define (compiler-phase/visible name thunk)
  (newline)
  (display "    ")
  (display name)
  (display "...")
  (let ((process-start (process-time-clock))
	(real-start (real-time-clock)))
    (thunk)
    (let ((process-delta (- (process-time-clock) process-start))
	  (real-delta (- (real-time-clock) real-start)))
      (set! compiler:process-time (+ process-delta compiler:process-time))
      (set! compiler:real-time (+ real-delta compiler:real-time))
      (compiler-time-report "      Time taken" process-delta real-delta))))

(define (compiler-phase/invisible thunk)
  (if compiler:phase-wrapper
      (compiler:phase-wrapper thunk)
      (thunk)))

(define (compiler-time-report prefix process-time real-time)
  (newline)
  (write-string prefix)
  (write-string ": ")
  (write (/ process-time 1000))
  (write-string " (process time); ")
  (write (/ real-time 1000))
  (write-string " (real time)"))

(define-macro (last-reference name)
  `(IF COMPILER:PRESERVE-DATA-STRUCTURES?
       ,name
       (SET! ,name)))

(define (phase/fg-generation)
  (compiler-superphase
   "Generating the Flow Graph"
   (lambda ()
     (phase/canonicalize-scode)
     (phase/translate-scode))))

(define (phase/canonicalize-scode)
  (compiler-subphase "Canonicalizing Scode"
   (lambda ()
     (set! *scode* (canonicalize/top-level (last-reference *input-scode*))))))

(define (phase/translate-scode)
  (compiler-subphase "Translating Scode into Flow Graph"
   (lambda ()
     (set! *current-label-number* 0)
     (set! *constants* '())
     (set! *blocks* '())
     (set! *expressions* '())
     (set! *procedures* '())
     (set! *lvalues* '())
     (set! *applications* '())
     (set! *parallels* '())
     (set! *assignments* '())
     (set! *root-expression* (construct-graph (last-reference *scode*)))
     (set! *root-block* (expression-block *root-expression*))
     (if (or (null? *expressions*)
	     (not (null? (cdr *expressions*))))
	 (error "Multiple expressions"))
     (set! *expressions*))))

(define (phase/fg-optimization)
  (compiler-superphase "Optimizing the Flow Graph"
    (lambda ()
      (phase/simulate-application)
      (phase/outer-analysis)
      (phase/fold-constants)
      (phase/open-coding-analysis)
      (phase/operator-analysis)
      (phase/identify-closure-limits)
      (phase/setup-block-types)      (phase/continuation-analysis)
      (phase/simplicity-analysis)
      (phase/subproblem-ordering)
      (phase/connectivity-analysis)
      (phase/design-environment-frames)
      (phase/compute-node-offsets)
      (phase/fg-optimization-cleanup))))

(define (phase/simulate-application)
  (compiler-subphase "Simulating Applications"
    (lambda ()
      (simulate-application *lvalues* *applications*))))

(define (phase/outer-analysis)
  (compiler-subphase "Outer Analysis"
    (lambda ()
      (outer-analysis *root-expression* *procedures* *applications*))))

(define (phase/fold-constants)
  (compiler-subphase "Constant Folding"
    (lambda ()
      (fold-constants *lvalues* *applications*))))

(define (phase/open-coding-analysis)
  (compiler-subphase "Open Coding Analysis"
    (lambda ()
      (open-coding-analysis *applications*))))

(define (phase/operator-analysis)
  (compiler-subphase "Operator Analysis"
    (lambda ()
      (operator-analysis *procedures* *applications*))))

(define (phase/identify-closure-limits)
  (compiler-subphase "Identifying Closure Limits"
    (lambda ()
      (identify-closure-limits! *procedures* *applications* *assignments*))))

(define (phase/setup-block-types)
  (compiler-subphase "Setting Up Block Types"
    (lambda ()
      (setup-block-types! *root-block*))))

(define (phase/continuation-analysis)
  (compiler-subphase "Continuation Analysis"
    (lambda ()
      (continuation-analysis *blocks*))))

(define (phase/simplicity-analysis)
  (compiler-subphase "Simplicity Analysis"
    (lambda ()
      (simplicity-analysis *parallels*))))

(define (phase/subproblem-ordering)
  (compiler-subphase "Ordering Subproblems"
    (lambda ()
      (subproblem-ordering *parallels*))))

(define (phase/connectivity-analysis)
  (compiler-subphase "Connectivity Analysis"
    (lambda ()
      (connectivity-analysis *root-expression* *procedures*))))

(define (phase/design-environment-frames)
  (compiler-subphase "Designing Environment Frames"
    (lambda ()
      (design-environment-frames! *blocks*))))

(define (phase/compute-node-offsets)
  (compiler-subphase "Computing Node Offsets"
    (lambda ()
      (compute-node-offsets *root-expression*))))

(define (phase/fg-optimization-cleanup)
  (compiler-subphase "Cleaning Up After Flow Graph Optimization"
    (lambda ()
      (if (not compiler:preserve-data-structures?)
	  (begin (set! *constants*)
		 (set! *blocks*)
		 (set! *procedures*)
		 (set! *lvalues*)
		 (set! *applications*)
		 (set! *parallels*)
		 (set! *assignments*)
		 (set! *root-block*))))))

(define (phase/rtl-generation)
  (compiler-phase "Generating RTL"
    (lambda ()
      (set! *rtl-procedures* '())
      (set! *rtl-continuations* '())
      (set! *rtl-graphs* '())
      (set! *ic-procedure-headers* '())
      (initialize-machine-register-map!)
      (generate/top-level (last-reference *root-expression*))
      (set! label->object
	    (make/label->object *rtl-expression*
				*rtl-procedures*
				*rtl-continuations*))
      (for-each (lambda (entry)
		  (set-cdr! entry
			    (rtl-procedure/external-label
			     (label->object (cdr entry)))))
		*ic-procedure-headers*)
      (let ((n-registers
	     (map (lambda (rgraph)
		    (- (rgraph-n-registers rgraph)
		       number-of-machine-registers))
		  *rtl-graphs*)))
	(newline)
	(write-string "      Registers used: ")
	(write (apply max n-registers))
	(write-string " max, ")
	(write (apply min n-registers))
	(write-string " min, ")
	(write (/ (apply + n-registers) (length n-registers)))
	(write-string " mean")))))

(define (phase/rtl-optimization)
  (compiler-superphase "Optimizing RTL"
    (lambda ()
      (if compiler:cse?
	  (phase/common-subexpression-elimination))
      (phase/lifetime-analysis)
      (if compiler:code-compression?
	  (phase/code-compression))
      (phase/register-allocation)
      (phase/rtl-optimization-cleanup))))

(define (phase/common-subexpression-elimination)
  (compiler-subphase "Eliminating Common Subexpressions"
    (lambda ()
      (common-subexpression-elimination *rtl-graphs*))))
(define (phase/lifetime-analysis)
  (compiler-subphase "Lifetime Analysis"
    (lambda ()
      (lifetime-analysis *rtl-graphs*))))

(define (phase/code-compression)
  (compiler-subphase "Code Compression"
    (lambda ()
      (code-compression *rtl-graphs*))))

(define (phase/rtl-file-output pathname)
  (compiler-phase "RTL File Output"
    (lambda ()
      (let ((rtl (linearize-rtl *rtl-graphs*)))
	(if (eq? pathname true)
	    ;; recursive compilation
	    (set! *recursive-compilation-rtl-blocks*
		  (cons (cons *recursive-compilation-number* rtl)
			*recursive-compilation-rtl-blocks*))
	    (fasdump (if (null? *recursive-compilation-rtl-blocks*)
			 rtl
			 (list->vector
			  (cons (cons 0 rtl)
				*recursive-compilation-rtl-blocks*)))
		     pathname))))))

(define (phase/register-allocation)
  (compiler-subphase "Allocating Registers"
    (lambda ()
      (register-allocation *rtl-graphs*))))

(define (phase/rtl-optimization-cleanup)
  (if (not compiler:preserve-data-structures?)
      (for-each (lambda (rgraph)
		  ;; **** this slot is reused. ****
		  ;;(set-rgraph-register-bblock! rgraph false)
		  (set-rgraph-register-crosses-call?! rgraph false)
		  (set-rgraph-register-n-deaths! rgraph false)
		  (set-rgraph-register-live-length! rgraph false)
		  (set-rgraph-register-n-refs! rgraph false))
		*rtl-graphs*)))

(define (phase/bit-generation)
  (compiler-phase "Generating BITs"
    (lambda ()
      (set! compiler:external-labels '())
      (generate-bits
       *rtl-graphs*
       (lambda (block-label prefix)
	 (set! compiler:block-label block-label)
	 (node-insert-snode! (rtl-expr/entry-node *rtl-expression*)
			     (make-sblock prefix))))
      (set! compiler:entry-label (rtl-expr/label *rtl-expression*))
      (if (not compiler:preserve-data-structures?)
	  (begin (set! label->object)
		 (set! *rtl-expression*)
		 (set! *rtl-procedures*)
		 (set! *rtl-continuations*))))))

(define (phase/bit-linearization)
  (compiler-phase "Linearizing BITs"
    (lambda ()
      (set! compiler:bits
	    (append-instruction-sequences!
	     (lap:make-entry-point compiler:entry-label compiler:block-label)
	     (linearize-bits (last-reference *rtl-graphs*)))))))

(define (phase/assemble)
  (compiler-phase "Assembling"
    (lambda ()
      (if compiler:preserve-data-structures?
	  (assemble compiler:block-label compiler:bits phase/assemble-finish)
	  (assemble (set! compiler:block-label)
		    (set! compiler:bits)
		    phase/assemble-finish)))))

(define (phase/assemble-finish count code-vector labels bindings linkage-info)
  linkage-info ;; ignored
  (set! compiler:code-vector code-vector)
  (set! compiler:entry-points labels)
  (set! compiler:label-bindings bindings)
  (newline)
  (display "      Branch tensioning done in ")
  (write (1+ count))
  (if (zero? count)
      (display " iteration.")
      (display " iterations.")))

(define (phase/info-generation-2 pathname)
  (compiler-phase "Generating Debugging Information (pass 2)"
   (lambda ()
     (let ((info
	    (generation-phase2 compiler:label-bindings
			       (last-reference compiler:external-labels))))
	     
       (if (eq? pathname true)		; recursive compilation
	   (begin
	     (set! *recursive-compilation-results*
		   (cons (list *recursive-compilation-number*
			       info
			       compiler:code-vector)
			 *recursive-compilation-results*))
	     (set-compiled-code-block/debugging-info!
	      compiler:code-vector
	      (cons (pathname->string *info-output-pathname*)
		    *recursive-compilation-number*)))
	   (begin
	     (fasdump
	      (generate-top-level-info info *recursive-compilation-results*)
	      pathname)
	     (set-compiled-code-block/debugging-info!
	      compiler:code-vector
	      (pathname->string pathname))))))))

(define (phase/link)
  (compiler-phase "Linking"
    (lambda ()
      ;; This has sections locked against GC to prevent relocation
      ;; while computing addresses.
      (let ((bindings
	     (map (lambda (label)
		    (cons
		     label
		     (with-absolutely-no-interrupts
		      (lambda ()
			((ucode-primitive &make-object)
			 type-code:compiled-entry
			 (make-non-pointer-object
			  (+ (cdr (or (assq label compiler:label-bindings)
				      (error "Missing entry point" label)))
			     (object-datum compiler:code-vector))))))))
		  compiler:entry-points)))
	(let ((label->expression
	       (lambda (label)
		 (cdr (or (assq label bindings)
			  (error "Label not defined as entry point" label))))))
	  (set! compiler:expression (label->expression compiler:entry-label))
	  (for-each (lambda (entry)
		      (set-lambda-body! (car entry)
					(label->expression (cdr entry))))
		    *ic-procedure-headers*)))
      (set! compiler:code-vector)
      (set! compiler:entry-points)
      (set! compiler:label-bindings)
      (set! compiler:entry-label)
      (set! *ic-procedure-headers*))))