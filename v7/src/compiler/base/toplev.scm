#| -*-Scheme-*-

$Id: toplev.scm,v 4.68 2006/10/25 17:43:23 cph Exp $

Copyright 1987,1988,1989,1990,1991,1992 Massachusetts Institute of Technology
Copyright 1993,1994,1997,1999,2000,2001 Massachusetts Institute of Technology
Copyright 2006 Massachusetts Institute of Technology

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

;;;; Compiler Top Level
;;; package: (compiler top-level)

(declare (usual-integrations))

;;;; Incremental File Compiler

(define compile-file:override-usual-integrations '())
(define compile-file:sf-only? #f)
(define compile-file:force? #f)
(define compiler:compile-data-files-as-expressions? #t)
(define compile-file)
(let ((scm-pathname (lambda (path) (pathname-new-type path "scm")))
      (bin-pathname (lambda (path) (pathname-new-type path "bin")))
      (ext-pathname (lambda (path) (pathname-new-type path "ext")))
      (com-pathname (lambda (path)
		      (pathname-new-type path compiled-output-extension))))

  (define (process-file input-file output-file dependencies processor)
    (let ((doit (lambda () (processor input-file output-file dependencies))))
    (if compile-file:force?
	(doit)
	(let ((reasons
	       (let ((output-time (file-modification-time output-file)))
		 (if (not output-time)
		     (list input-file)
		     (list-transform-positive (cons input-file dependencies)
		       (lambda (dependency)
			 (let ((dep-time (file-modification-time dependency)))
			   (if dep-time
			       (> dep-time output-time)
			       (begin
				 (warn "Missing dependency:"
				       (->namestring dependency))
				 #f)))))))))
	  (if (pair? reasons)
	      (begin
		(write-notification-line
		 (lambda (port)
		   (write-string "Generating " port)
		   (write (->namestring output-file) port)
		   (write-string " because of:" port)
		   (for-each (lambda (reason)
			       (write-char #\space port)
			       (write (->namestring reason) port))
			     reasons)))
		(doit)))))))

  (set! compile-file
	(named-lambda (compile-file file #!optional dependencies environment)
	  (process-file (scm-pathname file)
			(bin-pathname file)
			(map ext-pathname
			     (if (default-object? dependencies)
				 '()
				 dependencies))
	    (lambda (input-file output-file dependencies)
	      (fluid-let ((sf/default-syntax-table
			   (if (default-object? environment)
			       #f
			       (begin
				 (if (not (environment? environment))
				     (error:wrong-type-argument environment
								"environment"
								'COMPILE-FILE))
				 environment)))
			  (sf/default-declarations
			   `((USUAL-INTEGRATIONS
			      ,@compile-file:override-usual-integrations)
			     ,@(if (null? dependencies)
				   '()
				   `((INTEGRATE-EXTERNAL ,@dependencies))))))
		(sf input-file output-file))))
	  (if (not compile-file:sf-only?)
	      (process-file (bin-pathname file)
			    (com-pathname file)
			    '()
		(lambda (input-file output-file dependencies)
		  dependencies
		  (fluid-let ((compiler:coalescing-constant-warnings? #f))
		    (compile-bin-file input-file output-file))))))))

;;;; Non-Incremental File Compiler

(define (cf input #!optional output)
  (let ((kernel
	 (lambda (source-file)
	   (receive (source-pathname bin-pathname spec-pathname)
	       (sf/pathname-defaulting source-file #f #f)
	     ;; Maybe this should be done only if scode-file
	     ;; does not exist or is older than source-file.
	     (sf source-pathname bin-pathname spec-pathname)
	     (if (default-object? output)
		 (compile-bin-file bin-pathname)
		 (compile-bin-file bin-pathname output))))))
    (if (pair? input)
	(for-each kernel input)
	(kernel input))))

(define (cbf input . rest)
  (apply compile-bin-file input rest))

(define (compile-bin-file input-string #!optional output-string)
  (if compiler:cross-compiling?
      (apply cross-compile-bin-file
	     (cons input-string (if (default-object? output-string)
				    '()
				    (list output-string))))
      (begin
	(compiler-pathnames
	 input-string
	 (and (not (default-object? output-string)) output-string)
	 (make-pathname #f #f #f #f "bin" 'NEWEST)
	 (lambda (input-pathname output-pathname)
	   (let ((scode (compiler-fasload input-pathname)))
	     (if (and (scode/constant? scode)
		      (not compiler:compile-data-files-as-expressions?))
		 (compile-data-from-file scode output-pathname)
		 (maybe-open-file
		  compiler:generate-rtl-files?
		  (pathname-new-type output-pathname "rtl")
		  (lambda (rtl-output-port)
		    (maybe-open-file
		     compiler:generate-lap-files?
		     (pathname-new-type output-pathname "lap")
		     (lambda (lap-output-port)
		       (fluid-let ((*debugging-key*
				    (random-byte-vector 32)))
			 (compile-scode/internal
			  scode
			  (pathname-new-type output-pathname "inf")
			  rtl-output-port
			  lap-output-port))))))))))
	unspecific)))

(define *debugging-key*)

(define (maybe-open-file open? pathname receiver)
  (if open?
      (call-with-output-file pathname receiver)
      (receiver #f)))

(define (compiler-pathnames input-string output-string default transform)
  (let* ((core
	  (lambda (input-string)
	    (let ((input-pathname (merge-pathnames input-string default)))
	      (let ((output-pathname
		     (let ((output-pathname
			    (pathname-new-type input-pathname
					       compiled-output-extension)))
		       (if output-string
			   (merge-pathnames output-string output-pathname)
			   output-pathname))))
		(let ((do-it
		       (lambda ()
			 (compiler-file-output
			  (transform input-pathname output-pathname)
			  output-pathname))))
		  (if compiler:noisy?
		      (with-notification
			  (lambda (port)
			    (write-string "Compiling file: " port)
			    (write (enough-namestring input-pathname) port)
			    (write-string " => " port)
			    (write (enough-namestring output-pathname) port))
			do-it)
		      (do-it)))))))
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
    (cond ((scode/constant? scode)
	   scode)
	  ((scode/open-block? scode)
	   (scode/open-block-components
	    scode
	    (lambda (names declarations body)
	      (if (null? names)
		  (scan-defines
		   body
		   (lambda (names declarations* body)
		     (make-open-block names
				      (append declarations declarations*)
				      body)))
		  scode))))
	  (else
	   (scan-defines scode make-open-block)))))

;;;; Alternate Entry Points

(define (compile-scode scode #!optional keep-debugging-info?)
  (compiler-output->compiled-expression
   (compile-scode/no-file
    scode
    (and (or (default-object? keep-debugging-info?)
	     keep-debugging-info?)
	 'KEEP))))

(define (compile-procedure procedure #!optional keep-debugging-info?)
  (compiler-output->procedure
   (compile-scode/no-file
    (procedure-lambda procedure)
    (and (or (default-object? keep-debugging-info?)
	     keep-debugging-info?)
	 'KEEP))
   (procedure-environment procedure)))

(define (compile-scode/no-file scode keep-debugging-info?)
  (fluid-let ((compiler:noisy? #f)
	      (*info-output-filename* keep-debugging-info?))
    (compile-scode/internal/hook
     (lambda ()
       (compile-scode/internal scode keep-debugging-info?)))))

(define (compiler:batch-compile input #!optional output)
  (fluid-let ((compiler:batch-mode? #t))
    (bind-condition-handler (list condition-type:error)
	compiler:batch-error-handler
      (lambda ()
	(if (default-object? output)
	    (compile-bin-file input)
	    (compile-bin-file input output))))))

(define (compiler:batch-error-handler condition)
  (let ((port (nearest-cmdl/port)))
    (fresh-line port)
    (write-condition-report condition port)
    (newline port))
  (compiler:abort #f))

(define (compiler:abort value)
  (if (not compiler:abort-handled?)
      (error "Not set up to abort" value))
  (fresh-line)
  (write-string ";*** Aborting...")
  (newline)
  (compiler:abort-continuation value))

(define (batch-kernel real-kernel)
  (lambda (input-string)
    (call-with-current-continuation
     (lambda (abort-compilation)
       (fluid-let ((compiler:abort-continuation abort-compilation)
		   (compiler:abort-handled? #t))
	 (real-kernel input-string))))))

(define compiler:batch-mode? #f)
(define compiler:abort-handled? #f)
(define compiler:abort-continuation)

(define (compile-recursively scode procedure-result? procedure-name)
  ;; Used by the compiler when it wants to compile subexpressions as
  ;; separate code-blocks.
  ;; The rtl output should be fixed.
  (let ((my-number *recursive-compilation-count*))
    (set! *recursive-compilation-count* (+ my-number 1))
    (let ((do-it
	   (lambda ()
	     (compile-recursively-1 scode
				    procedure-result?
				    procedure-name
				    my-number))))
      (if (and compiler:show-phases?
	       (not compiler:show-procedures?))
	  (with-notification (lambda (port)
			       (write-string "*** Recursive compilation " port)
			       (write my-number port))
	    do-it)
	  (do-it)))))

(define (compile-recursively-1 scode procedure-result? procedure-name
			       my-number)
  (let ((do-it
	 (lambda ()
	   (fluid-let ((*recursive-compilation-number* my-number)
		       (compiler:package-optimization-level 'NONE)
		       (*procedure-result?* procedure-result?))
	     (compile-scode/internal
	      scode
	      (and *info-output-filename*
		   (if (eq? *info-output-filename* 'KEEP)
		       'KEEP
		       'RECURSIVE))
	      *rtl-output-port*
	      *lap-output-port*
	      bind-compiler-variables)))))
    (if procedure-result?
	(let ((do-it
	       (lambda ()
		 (let ((result (do-it)))
		   (set! *remote-links*
			 (cons (cdr result) *remote-links*))
		   (car result)))))
	  (if compiler:show-procedures?
	      (compiler-phase/visible
	       (call-with-output-string
		 (lambda (port)
		   (write-string "Compiling procedure: " port)
		   (write procedure-name port)))
	       do-it)
	      (do-it)))
	(fluid-let ((*remote-links* '()))
	  (do-it)))))

;;;; Global variables

(define *recursive-compilation-count*)
(define *recursive-compilation-number*)
(define *procedure-result?*)
(define *remote-links*)
(define *process-time*)
(define *real-time*)

(define *info-output-filename* #f)
(define *rtl-output-port* #f)
(define *lap-output-port* #f)

;; First set: input to compilation
;; Last used: phase/canonicalize-scode
(define *input-scode*)

;; First set: phase/canonicalize-scode
;; Last used: phase/translate-scode
(define *scode*)

;; First set: phase/translate-scode
;; Last used: phase/fg-optimization-cleanup
(define *root-block*)

;; First set: phase/translate-scode
;; Last used: phase/rtl-generation
(define *root-expression*)
(define *root-procedure*)

;; First set: phase/rtl-generation
;; Last used: phase/lap-linearization
(define *rtl-expression*)
(define *rtl-procedures*)
(define *rtl-continuations*)
(define *rtl-graphs*)
(define label->object)
(define *rtl-root*)

;; First set: phase/rtl-generation
;; Last used: phase/link
(define *ic-procedure-headers*)
(define *entry-label*)

;; First set: phase/lap-generation
;; Last used: phase/link
(define *subprocedure-linking-info*)

;; First set: phase/lap-linearization
;; Last used: phase/assemble
(define *lap*)

;; First set: phase/lap-linearization
;; Last used: phase/info-generation-2
(define *dbg-expression*)
(define *dbg-procedures*)
(define *dbg-continuations*)

(define (in-compiler thunk)
  (let ((run-compiler
	 (lambda ()
	   (let ((value
		  (let ((expression (thunk)))
		    (let ((others
			   (map (lambda (other) (vector-ref other 2))
				(recursive-compilation-results))))
		      (cond ((not (compiled-code-address? expression))
			     (vector compiler:compile-by-procedures?
				     expression
				     others))
			    ((null? others)
			     expression)
			    (else
			     (scode/make-comment
			      (make-dbg-info-vector
			       (let ((all-blocks
				      (list->vector
				       (cons
					(compiled-code-address->block
					 expression)
					others))))
				 (if compiler:compile-by-procedures?
				     (list 'COMPILED-BY-PROCEDURES
					   all-blocks
					   (list->vector others))
				     all-blocks)))
			      expression)))))))
	     (if compiler:show-time-reports?
		 (compiler-time-report "Total compilation time"
				       *process-time*
				       *real-time*))
	     value))))
    (if compiler:preserve-data-structures?
	(begin
	  (compiler:reset!)
	  (run-compiler))
	(fluid-let ((*recursive-compilation-number* 0)
		    (*recursive-compilation-count* 1)
		    (*procedure-result?* #f)
		    (*remote-links* '())
		    (*process-time* 0)
		    (*real-time* 0))
	  (bind-assembler&linker-top-level-variables
	   (lambda ()
	     (bind-compiler-variables run-compiler)))))))

(define (bind-compiler-variables thunk)
  ;; Split this fluid-let because compiler was choking on it.
  (fluid-let ((*ic-procedure-headers*)
	      (*current-label-number*)
	      (*dbg-expression*)
	      (*dbg-procedures*)
	      (*dbg-continuations*)
	      (*lap*)
	      (*constants*)
	      (*blocks*)
	      (*expressions*)
	      (*procedures*)
	      (*lvalues*))
    (fluid-let ((*applications*)
		(*parallels*)
		(*input-scode*)
		(*scode*)
		(*root-expression*)
		(*root-procedure*)
		(*root-block*)
		(*rtl-expression*)
		(*rtl-procedures*)
		(*rtl-continuations*)
		(*rtl-graphs*)
		(label->object)
		(*rtl-root*)
		(*machine-register-map*)
		(*entry-label*)
		(*subprocedure-linking-info*))
      (bind-assembler&linker-variables thunk))))

(define (compiler:reset!)
  (set! *recursive-compilation-number* 0)
  (set! *recursive-compilation-count* 1)
  (set! *procedure-result?* #f)
  (set! *remote-links* '())
  (set! *process-time* 0)
  (set! *real-time* 0)

  (set! *ic-procedure-headers*)
  (set! *current-label-number*)
  (set! *dbg-expression*)
  (set! *dbg-procedures*)
  (set! *dbg-continuations*)
  (set! *lap*)
  (set! *constants*)
  (set! *blocks*)
  (set! *expressions*)
  (set! *procedures*)
  (set! *lvalues*)
  (set! *applications*)
  (set! *parallels*)
  (set! *input-scode*)
  (set! *scode*)
  (set! *root-expression*)
  (set! *root-procedure*)
  (set! *root-block*)
  (set! *rtl-expression*)
  (set! *rtl-procedures*)
  (set! *rtl-continuations*)
  (set! *rtl-graphs*)
  (set! label->object)
  (set! *rtl-root*)
  (set! *machine-register-map*)
  (set! *entry-label*)
  (set! *subprocedure-linking-info*)
  (assembler&linker-reset!))

;;;; Main Entry Point

(define (compile-scode/internal scode
				#!optional
				info-output-pathname
				rtl-output-port
				lap-output-port
				wrapper)
  (let ((info-output-pathname
	 (if (default-object? info-output-pathname)
	     #f
	     info-output-pathname))
	(rtl-output-port
	 (if (default-object? rtl-output-port) #f rtl-output-port))
	(lap-output-port
	 (if (default-object? lap-output-port) #f lap-output-port))
	(wrapper
	 (if (default-object? wrapper) in-compiler wrapper)))
    (fluid-let ((*info-output-filename*
		 (if (pathname? info-output-pathname)
		     info-output-pathname
		     *info-output-filename*))
		(*rtl-output-port* rtl-output-port)
		(*lap-output-port* lap-output-port)
		(compiler:show-phases?
		 (and compiler:noisy? compiler:show-phases?))
		(compiler:show-subphases?
		 (and compiler:noisy? compiler:show-subphases?))
		(compiler:show-time-reports?
		 (and compiler:noisy? compiler:show-time-reports?))
		(compiler:show-procedures?
		 (and compiler:noisy? compiler:show-procedures?)))
      (wrapper
       (lambda ()
	 (set! *input-scode* scode)
	 (phase/fg-generation)
	 (phase/fg-optimization)
	 (phase/rtl-generation)
	 #|
	 ;; Current info-generation keeps state in-core.
	 (if info-output-pathname
	     (phase/info-generation-1 info-output-pathname))
	 |#
	 (phase/rtl-optimization)
	 (if rtl-output-port
	     (phase/rtl-file-output scode rtl-output-port))
	 (phase/lap-generation)
	 (phase/lap-linearization)
	 (if lap-output-port
	     (phase/lap-file-output scode lap-output-port))
	 (assemble&link info-output-pathname))))))

(define (compiler-phase name thunk)
  (if compiler:show-phases?
      (compiler-phase/visible name
	(lambda ()
	  (compiler-phase/invisible thunk)))
      (compiler-phase/invisible thunk)))

(define (compiler-superphase name thunk)
  (if compiler:show-subphases?
      (thunk)
      (compiler-phase name thunk)))

(define (compiler-subphase name thunk)
  (if compiler:show-subphases?
      (compiler-phase name thunk)
      (compiler-phase/invisible thunk)))

(define (compiler-phase/visible name thunk)
  (let ((thunk
	 (lambda ()
	   (with-notification (lambda (port) (write-string name port))
	     thunk))))
    (if compiler:show-time-reports?
	(let ((process-start *process-time*)
	      (real-start *real-time*))
	  (let ((value (thunk)))
	    (compiler-time-report "Time taken"
				  (- *process-time* process-start)
				  (- *real-time* real-start))
	    value))
	(thunk))))

(define *phase-level* 0)

(define (compiler-phase/invisible thunk)
  (fluid-let ((*phase-level* (1+ *phase-level*)))
    (let ((do-it
	   (if compiler:phase-wrapper
	       (lambda () (compiler:phase-wrapper thunk))
	       thunk)))
      (if (= 1 *phase-level*)
	  (let ((process-start (process-time-clock))
		(real-start (real-time-clock)))
	    (let ((value (do-it)))
	      (let ((process-delta (- (process-time-clock) process-start))
		    (real-delta (- (real-time-clock) real-start)))
		(set! *process-time* (+ process-delta *process-time*))
		(set! *real-time* (+ real-delta *real-time*)))
	      value))
	  (do-it)))))

(define (compiler-time-report prefix process-time real-time)
  (write-notification-line
   (lambda (port)
     (write-string prefix port)
     (write-string ": " port)
     (write (/ (exact->inexact process-time) 1000) port)
     (write-string " (process time); " port)
     (write (/ (exact->inexact real-time) 1000) port)
     (write-string " (real time)" port))))

(define (phase/fg-generation)
  (compiler-superphase "Flow Graph Generation"
    (lambda ()
      (phase/canonicalize-scode)
      (phase/translate-scode))))

(define (phase/canonicalize-scode)
  (compiler-subphase "Scode Canonicalization"
    (lambda ()
      (set! *scode* (canonicalize/top-level (last-reference *input-scode*)))
      unspecific)))

(define (phase/translate-scode)
  (compiler-subphase "Translation of Scode into Flow Graph"
    (lambda ()
      (set! *current-label-number* 0)
      (set! *constants* '())
      (set! *blocks* '())
      (set! *expressions* '())
      (set! *procedures* '())
      (set! *lvalues* '())
      (set! *applications* '())
      (set! *parallels* '())
      (set! *root-expression* (construct-graph (last-reference *scode*)))
      (if *procedure-result?*
	  (let ((node (expression-entry-node *root-expression*)))
	    (if (not (and (application? node)
			  (application/return? node)))
		(error "Entry node of procedure compilation not return" node))
	    (let ((operand (return/operand node)))
	      (if (not (procedure? operand))
		  (error "Value of procedure compilation not procedure" node))
	      (set! *root-procedure* operand))))
      (set! *root-block* (expression-block *root-expression*))
      (if (or (null? *expressions*)
	      (not (null? (cdr *expressions*))))
	  (error "Multiple expressions"))
      (set! *expressions*)
      unspecific)))

(define (phase/fg-optimization)
  (compiler-superphase "Flow Graph Optimization"
    (lambda ()
      (phase/simulate-application)
      (phase/outer-analysis)
      (phase/fold-constants)
      (phase/open-coding-analysis)
      (phase/operator-analysis)
      (phase/environment-optimization)
      (phase/identify-closure-limits)
      (phase/setup-block-types)
      (phase/variable-indirection)
      (phase/compute-call-graph)
      (phase/side-effect-analysis)
      (phase/continuation-analysis)
      (phase/subproblem-analysis)
      (phase/delete-integrated-parameters)
      (phase/subproblem-ordering)
      (phase/delete-integrated-parameters)
      (phase/design-environment-frames)
      (phase/connectivity-analysis)
      (phase/compute-node-offsets)
      (phase/return-equivalencing)
      (phase/info-generation-1)
      (phase/fg-optimization-cleanup))))

(define (phase/simulate-application)
  (compiler-subphase "Application Simulation"
    (lambda ()
      (simulate-application *lvalues* *applications*))))

(define (phase/outer-analysis)
  (compiler-subphase "Outer Analysis"
    (lambda ()
      (outer-analysis *root-expression* *procedures* *applications*))))

(define (phase/fold-constants)
  (compiler-subphase "Fold Constants"
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

(define (phase/variable-indirection)
  (compiler-subphase "Variable Indirection"
    (lambda ()
      (initialize-variable-indirections! *lvalues*))))

(define (phase/environment-optimization)
  (compiler-subphase "Environment Optimization"
    (lambda ()
      (optimize-environments! *procedures*))))

(define (phase/identify-closure-limits)
  (compiler-subphase "Closure Limit Identification"
    (lambda ()
      (identify-closure-limits! *procedures* *applications* *lvalues*)
      (if (not compiler:preserve-data-structures?)
	  (for-each (lambda (procedure)
		      (if (not (procedure-continuation? procedure))
			  (begin
			    (set-procedure-free-callees! procedure '())
			    (set-procedure-free-callers! procedure '()))))
		    *procedures*)))))

(define (phase/setup-block-types)
  (compiler-subphase "Block Type Determination"
    (lambda ()
      (setup-block-types! *root-block*)
      (if (not compiler:preserve-data-structures?)
	  (for-each (lambda (procedure)
		      (if (not (procedure-continuation? procedure))
			  (set-procedure-variables! procedure '())))
		    *procedures*))
      (setup-closure-contexts! *root-expression* *procedures*))))

(define (phase/compute-call-graph)
  (compiler-subphase "Call Graph Computation"
    (lambda ()
      (compute-call-graph! *procedures*))))

(define (phase/side-effect-analysis)
  (compiler-subphase "Side Effect Analysis"
    (lambda ()
      (side-effect-analysis *procedures* *applications*))))

(define (phase/continuation-analysis)
  (compiler-subphase "Continuation Analysis"
    (lambda ()
      (continuation-analysis *blocks*)
      (setup-frame-adjustments *applications*)
      (setup-block-static-links! *blocks*))))

(define (phase/subproblem-analysis)
  (compiler-subphase "Subproblem Analysis"
    (lambda ()
      (simplicity-analysis *parallels*)
      (compute-subproblem-free-variables *parallels*))))

(define (phase/delete-integrated-parameters)
  (compiler-subphase "Integrated Parameter Deletion"
		     (lambda ()
		       (delete-integrated-parameters *blocks*))))

(define (phase/subproblem-ordering)
  (compiler-subphase "Subproblem Ordering"
    (lambda ()
      (subproblem-ordering *parallels*))))

(define (phase/connectivity-analysis)
  (compiler-subphase "Connectivity Analysis"
    (lambda ()
      (connectivity-analysis *root-expression* *procedures*))))

(define (phase/design-environment-frames)
  (compiler-subphase "Environment Frame Design"
		     (lambda ()
		       (design-environment-frames! *blocks*))))

(define (phase/compute-node-offsets)
  (compiler-subphase "Stack Frame Offset Determination"
    (lambda ()
      (compute-node-offsets *root-expression*))))

(define (phase/return-equivalencing)
  (compiler-subphase "Return Equivalencing"
    (lambda ()
      (find-equivalent-returns! *lvalues* *applications*))))

(define (phase/info-generation-1)
  (compiler-subphase "Debugging Information Initialization"
    (lambda ()
      (info-generation-phase-1 *root-expression* *procedures*))))

(define (phase/fg-optimization-cleanup)
  (compiler-subphase "Flow Graph Optimization Cleanup"
    (lambda ()
      (if (not compiler:preserve-data-structures?)
	  (begin
	    (clear-call-graph! *procedures*)
	    (set! *constants*)
	    (set! *blocks*)
	    (set! *procedures*)
	    (set! *lvalues*)
	    (set! *applications*)
	    (set! *parallels*)
	    (set! *root-block*)
	    unspecific)))))

(define (phase/rtl-generation)
  (compiler-phase "RTL Generation"
    (lambda ()
      (set! *ic-procedure-headers* '())
      (initialize-machine-register-map!)
      (receive (expression procedures continuations rgraphs)
	  (generate/top-level (last-reference *root-expression*))
	(set! *rtl-expression* expression)
	(set! *rtl-procedures* procedures)
	(set! *rtl-continuations* continuations)
	(set! *rtl-graphs* rgraphs)
	unspecific)
      (if *procedure-result?*
	  (set! *rtl-expression* #f))
      (set! label->object
	    (make/label->object *rtl-expression*
				*rtl-procedures*
				*rtl-continuations*))
      (set! *rtl-root*
	    (if *procedure-result?*
		(label->object
		 (procedure-label (last-reference *root-procedure*)))
		*rtl-expression*))
      (for-each (lambda (entry)
		  (set-cdr! entry
			    (rtl-procedure/external-label
			     (label->object (cdr entry)))))
		*ic-procedure-headers*)
      (if compiler:show-phases?
	  (let ((n-registers
		 (map (lambda (rgraph)
			(- (rgraph-n-registers rgraph)
			   number-of-machine-registers))
		      *rtl-graphs*)))
	    (write-notification-line
	     (lambda (port)
	       (write-string "Registers used: " port)
	       (write (apply max n-registers) port)
	       (write-string " max, " port)
	       (write (apply min n-registers) port)
	       (write-string " min, " port)
	       (write (exact->inexact (/ (apply + n-registers)
					 (length n-registers)))
		      port)
	       (write-string " mean" port))))))))

(define (phase/rtl-optimization)
  (compiler-superphase "RTL Optimization"
    (lambda ()
      (phase/rtl-dataflow-analysis)
      (phase/rtl-rewriting rtl-rewriting:pre-cse)
      (if compiler:cse?
	  (phase/common-subexpression-elimination))
      (phase/invertible-expression-elimination)
      (phase/rtl-rewriting rtl-rewriting:post-cse)
      (phase/common-suffix-merging)
      (phase/lifetime-analysis)
      (if compiler:code-compression?
	  (phase/code-compression))
      (phase/linearization-analysis)
      (phase/register-allocation)
      (phase/rtl-optimization-cleanup))))

(define (phase/rtl-dataflow-analysis)
  (compiler-subphase "RTL Dataflow Analysis"
    (lambda ()
      (rtl-dataflow-analysis *rtl-graphs*))))

(define (phase/rtl-rewriting rtl-rewriting)
  (compiler-subphase "RTL Rewriting"
    (lambda ()
      (rtl-rewriting *rtl-graphs*))))

(define (phase/common-subexpression-elimination)
  (compiler-subphase "Common Subexpression Elimination"
    (lambda ()
      (common-subexpression-elimination *rtl-graphs*))))

(define (phase/invertible-expression-elimination)
  (compiler-subphase "Invertible Expression Elimination"
    (lambda ()
      (invertible-expression-elimination *rtl-graphs*))))

(define (phase/common-suffix-merging)
  (compiler-subphase "Common Suffix Merging"
    (lambda ()
      (merge-common-suffixes! *rtl-graphs*))))

(define (phase/lifetime-analysis)
  (compiler-subphase "Lifetime Analysis"
    (lambda ()
      (lifetime-analysis *rtl-graphs*))))

(define (phase/code-compression)
  (compiler-subphase "Instruction Combination"
    (lambda ()
      (code-compression *rtl-graphs*))))

(define (phase/linearization-analysis)
  (compiler-subphase "Linearization Analysis"
    (lambda ()
      (setup-bblock-continuations! *rtl-graphs*))))

(define (phase/register-allocation)
  (compiler-subphase "Register Allocation"
    (lambda ()
      (register-allocation *rtl-graphs*))))

(define (phase/rtl-optimization-cleanup)
  (if (not compiler:preserve-data-structures?)
      (for-each (lambda (rgraph)
		  (set-rgraph-bblocks! rgraph #f)
		  ;; **** this slot is reused. ****
		  ;;(set-rgraph-register-bblock! rgraph #f)
		  (set-rgraph-register-crosses-call?! rgraph #f)
		  (set-rgraph-register-n-deaths! rgraph #f)
		  (set-rgraph-register-live-length! rgraph #f)
		  (set-rgraph-register-n-refs! rgraph #f)
		  (set-rgraph-register-known-values! rgraph #f))
		*rtl-graphs*)))

(define (phase/rtl-file-output scode port)
  (compiler-phase "RTL File Output"
    (lambda ()
      (write-string "RTL for object " port)
      (write *recursive-compilation-number* port)
      (newline port)
      (pp scode port #t 4)
      (newline port)
      (newline port)
      (write-rtl-instructions (linearize-rtl *rtl-root*
					     *rtl-procedures*
					     *rtl-continuations*)
			      port)
      (if (not (zero? *recursive-compilation-number*))
	  (begin
	    (write-char #\page port)
	    (newline port)))
      (output-port/flush-output port))))

(define (phase/lap-generation)
  (compiler-phase "LAP Generation"
    (lambda ()
      (initialize-back-end!)
      (if *procedure-result?*
	  (generate-lap *rtl-graphs* '()
	    (lambda (prefix environment-label free-ref-label n-sections)
	      (node-insert-snode! (rtl-procedure/entry-node *rtl-root*)
				  (make-sblock prefix))
	      (set! *entry-label*
		    (rtl-procedure/external-label *rtl-root*))
	      (set! *subprocedure-linking-info*
		    (vector environment-label free-ref-label n-sections))
	      unspecific))
	  (begin
	    (let ((prefix (generate-lap *rtl-graphs* *remote-links* #f)))
	      (node-insert-snode! (rtl-expr/entry-node *rtl-root*)
				  (make-sblock prefix)))
	    (set! *entry-label* (rtl-expr/label *rtl-root*))
	    unspecific)))))

(define (phase/lap-linearization)
  (compiler-phase "LAP Linearization"
    (lambda ()
      (set! *lap*
	    (optimize-linear-lap
	     (wrap-lap *entry-label*
		       (linearize-lap *rtl-root*
				      *rtl-procedures*
				      *rtl-continuations*))))
      (receive (expression procedures continuations)
	  (info-generation-phase-2 *rtl-expression*
				   *rtl-procedures*
				   *rtl-continuations*)
	(set! *dbg-expression* expression)
	(set! *dbg-procedures* procedures)
	(set! *dbg-continuations* continuations)
	unspecific)
      (if (not compiler:preserve-data-structures?)
	  (begin
	    (set! *rtl-expression*)
	    (set! *rtl-procedures*)
	    (set! *rtl-continuations*)
	    (set! *rtl-graphs*)
	    (set! label->object)
	    (set! *rtl-root*)
	    unspecific)))))

(define (phase/lap-file-output scode port)
  (compiler-phase "LAP File Output"
    (lambda ()
      (fluid-let ((*unparser-radix* 16)
		  (*unparse-uninterned-symbols-by-name?* #t))
	(with-output-to-port port
	  (lambda ()
	    (write-string "LAP for object ")
	    (write *recursive-compilation-number*)
	    (newline)
	    (pp scode (current-output-port) #t 4)
	    (newline)
	    (newline)
	    (newline)
	    (for-each
		(lambda (instruction)
		  (cond ((and (pair? instruction)
			      (eq? (car instruction) 'LABEL))
			 (write (cadr instruction))
			 (write-char #\:))
			((and (pair? instruction)
			      (eq? (car instruction) 'COMMENT))
			 (write-char #\tab)
			 (write-string ";;")
			 (for-each (lambda (frob)
				     (write-string " ")
				     (write (if (and (pair? frob)
						     (eq? (car frob) 'RTL))
						(cadr frob)
						frob)))
			   (cdr instruction)))
			(else
			 (write-char #\tab)
			 (write instruction)))
		  (newline))
	      *lap*)
	    (if (not (zero? *recursive-compilation-number*))
		(begin
		  (write-char #\page)
		  (newline)))
	    (output-port/flush-output port)))))))