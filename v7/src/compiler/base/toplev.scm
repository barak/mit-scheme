#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/toplev.scm,v 4.43 1992/06/12 01:43:14 jinx Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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
;;; package: (compiler top-level)

(declare (usual-integrations))

;;;; Usual Entry Point: File Compilation

(define (cf input #!optional output)
  (let ((kernel
	 (lambda (source-file)
	   (with-values
	       (lambda () (sf/pathname-defaulting source-file false false))
	     (lambda (source-pathname bin-pathname spec-pathname)
	       ;; Maybe this should be done only if scode-file
	       ;; does not exist or is older than source-file.
	       (sf source-pathname bin-pathname spec-pathname)
	       (if (default-object? output)
		   (compile-bin-file bin-pathname)
		   (compile-bin-file bin-pathname output)))))))
    (if (pair? input)
	(for-each kernel input)
	(kernel input))))

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
	 (make-pathname false false false false "bin" 'NEWEST)
	 (lambda (input-pathname output-pathname)
	   (maybe-open-file
	    compiler:generate-rtl-files?
	    (pathname-new-type output-pathname "rtl")
	    (lambda (rtl-output-port)
	      (maybe-open-file compiler:generate-lap-files?
			       (pathname-new-type output-pathname "lap")
			       (lambda (lap-output-port)
				 (compile-scode/internal
				  (compiler-fasload input-pathname)
				  (pathname-new-type output-pathname "inf")
				  rtl-output-port
				  lap-output-port)))))))
	unspecific)))

(define (maybe-open-file open? pathname receiver)
  (if open?
      (call-with-output-file pathname receiver)
      (receiver false)))

(define (compiler-pathnames input-string output-string default transform)
  (let* ((core
	  (lambda (input-string)
	    (let ((input-pathname (merge-pathnames input-string default)))
	      (let ((output-pathname
		     (let ((output-pathname
			    (pathname-new-type input-pathname "com")))
		       (if output-string
			   (merge-pathnames output-string output-pathname)
			   output-pathname))))
		(if compiler:noisy?
		    (begin
		      (newline)
		      (write-string "Compile File: ")
		      (write (enough-namestring input-pathname))
		      (write-string " => ")
		      (write (enough-namestring output-pathname))))
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

;;;; Alternate Entry Points

(define (compile-scode scode #!optional keep-debugging-info?)
  (let ((keep-debugging-info?
	 (and (or (default-object? keep-debugging-info?)
		  keep-debugging-info?)
	      'KEEP)))
    (fluid-let ((compiler:noisy? false)
		(*info-output-filename* keep-debugging-info?))
      (compile-scode/internal scode
			      keep-debugging-info?))))  

(define (compile-procedure procedure #!optional keep-debugging-info?)
  (scode-eval (let ((keep-debugging-info?
		     (and (or (default-object? keep-debugging-info?)
			      keep-debugging-info?)
			  'KEEP)))
		(fluid-let ((compiler:noisy? false)
			    (*info-output-filename* keep-debugging-info?))
		  (compile-scode/internal (procedure-lambda procedure)
				 keep-debugging-info?)))
	      (procedure-environment procedure)))

(define (compiler:batch-compile input #!optional output)
  (fluid-let ((compiler:batch-mode? true))
    (bind-condition-handler (list condition-type:error)
	compiler:batch-error-handler
      (lambda ()
	(if (default-object? output)
	    (compile-bin-file input)
	    (compile-bin-file input output))))))

(define (compiler:batch-error-handler condition)
  (let ((port (nearest-cmdl/port)))
    (newline port)
    (write-condition-report condition port))
  (compiler:abort false))

(define (compiler:abort value)
  (if (not compiler:abort-handled?)
      (error "Not set up to abort" value))
  (newline)
  (write-string "*** Aborting...")
  (compiler:abort-continuation value))

(define (batch-kernel real-kernel)
  (lambda (input-string)
    (call-with-current-continuation
     (lambda (abort-compilation)
       (fluid-let ((compiler:abort-continuation abort-compilation)
		   (compiler:abort-handled? true))
	 (real-kernel input-string))))))

(define compiler:batch-mode? false)
(define compiler:abort-handled? false)
(define compiler:abort-continuation)

;;; Example of `lap->code' usage (MC68020):

#|
(define bar
  ;; defines bar to be a procedure that adds 1 to its argument
  ;; with no type or range checks.
  (scode-eval
   (lap->code
    'start
    `((entry-point start)
      (dc uw #xffff)
      (block-offset start)
      (label start)
      (pea (@pcr proc))
      (or b (& ,(* (microcode-type 'compiled-entry) 4)) (@a 7))
      (mov l (@a+ 7) (@ao 6 8))
      (and b (& #x3) (@a 7))
      (rts)
      (dc uw #x0202)
      (block-offset proc)
      (label proc)
      (mov l (@a+ 7) (d 0))
      (addq l (& 1) (d 0))
      (mov l (d 0) (@ao 6 8))
      (and b (& #x3) (@a 7))
      (rts)))
   '()))
|#

(define (lap->code label instructions)
  (in-compiler
   (lambda ()
     (set! *lap* instructions)
     (set! *entry-label* label)
     (set! *current-label-number* 0)
     (set! *next-constant* 0)
     (set! *interned-constants* '())
     (set! *interned-variables* '())
     (set! *interned-assignments* '())
     (set! *interned-uuo-links* '())
     (set! *interned-global-links* '())
     (set! *interned-static-variables* '())
     (set! *block-label* (generate-label))
     (set! *external-labels* '())
     (set! *ic-procedure-headers* '())
     (phase/assemble)
     (phase/link)
     *result*)))

(define (compile-recursively scode procedure-result? procedure-name)
  ;; Used by the compiler when it wants to compile subexpressions as
  ;; separate code-blocks.
  ;; The rtl output should be fixed.
  (let ((my-number *recursive-compilation-count*)
	(output?
	 (and compiler:show-phases?
	      (not compiler:show-procedures?))))
    (set! *recursive-compilation-count* (1+ my-number))
    (if output?
	(begin
	  (newline)
	  (newline)
	  (write-string *output-prefix*)
	  (write-string "*** Recursive compilation ")
	  (write my-number)
	  (write-string " ***")))
    (let ((value
	   ((let ((do-it
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
			(lambda ()
			  (compiler-phase/visible
			   (string-append
			    "Compiling procedure: "
			    (write-to-string procedure-name))
			   do-it))
			do-it))
		  (lambda ()
		    (fluid-let ((*remote-links* '()))
		      (do-it))))))))
      (if output?
	  (begin
	    (newline)
	    (write-string *output-prefix*)
	    (write-string "*** Done with recursive compilation ")
	    (write my-number)
	    (write-string " ***")
	    (newline)))
      value)))

;;;; Global variables

(define *recursive-compilation-count*)
(define *recursive-compilation-number*)
(define *recursive-compilation-results*)
(define *procedure-result?*)
(define *remote-links*)
(define *process-time*)
(define *real-time*)

(define *info-output-filename* false)
(define *rtl-output-port* false)
(define *lap-output-port* false)

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
(define *block-label*)

;; First set: phase/lap-generation
;; Last used: phase/info-generation-2
(define *external-labels*)

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

;; First set: phase/assemble
;; Last used: phase/link
(define *label-bindings*)
(define *code-vector*)
(define *entry-points*)

;; First set: phase/link
;; Last used: result of compilation
(define *result*)

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
		    (*recursive-compilation-results* '())
		    (*procedure-result?* false)
		    (*remote-links* '())
		    (*process-time* 0)
		    (*real-time* 0))
	  (bind-compiler-variables run-compiler)))))

(define (bind-compiler-variables thunk)
  ;; Split this fluid-let because compiler was choking on it.
  (fluid-let ((*ic-procedure-headers*)
	      (*current-label-number*)
	      (*external-labels*)
	      (*block-label*)
	      (*dbg-expression*)
	      (*dbg-procedures*)
	      (*dbg-continuations*)
	      (*lap*)
	      (*next-constant*)
	      (*interned-constants*)
	      (*interned-variables*)
	      (*interned-assignments*)
	      (*interned-uuo-links*)
	      (*interned-global-links*)
	      (*interned-static-variables*)
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
		(*label-bindings*)
		(*code-vector*)
		(*entry-points*)
		(*subprocedure-linking-info*)
		(*result*))
      (thunk))))

(define (recursive-compilation-results)
  (sort *recursive-compilation-results*
	(lambda (x y) (< (vector-ref x 0) (vector-ref y 0)))))

(define (compiler:reset!)
  (set! *recursive-compilation-number* 0)
  (set! *recursive-compilation-count* 1)
  (set! *recursive-compilation-results* '())
  (set! *procedure-result?* false)
  (set! *remote-links* '())
  (set! *process-time* 0)
  (set! *real-time* 0)

  (set! *ic-procedure-headers*)
  (set! *current-label-number*)
  (set! *external-labels*)
  (set! *block-label*)
  (set! *dbg-expression*)
  (set! *dbg-procedures*)
  (set! *dbg-continuations*)
  (set! *lap*)
  (set! *next-constant*)
  (set! *interned-constants*)
  (set! *interned-variables*)
  (set! *interned-assignments*)
  (set! *interned-uuo-links*)
  (set! *interned-global-links*)
  (set! *interned-static-variables*)
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
  (set! *label-bindings*)
  (set! *code-vector*)
  (set! *entry-points*)
  (set! *subprocedure-linking-info*)
  (set! *result*)
  unspecific)

;;;; Main Entry Point

(define (compile-scode/internal scode
				#!optional
				info-output-pathname
				rtl-output-port
				lap-output-port
				wrapper)
  (let ((info-output-pathname
	 (if (default-object? info-output-pathname)
	     false
	     info-output-pathname))
	(rtl-output-port
	 (if (default-object? rtl-output-port) false rtl-output-port))
	(lap-output-port
	 (if (default-object? lap-output-port) false lap-output-port))
	(wrapper
	 (if (default-object? wrapper) in-compiler wrapper)))
    (fluid-let ((*info-output-filename*
		 (if (pathname? info-output-pathname)
		     (->namestring info-output-pathname)
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
	     (phase/rtl-file-output rtl-output-port))
	 (phase/lap-generation)
	 (phase/lap-linearization)
	 (if lap-output-port
	     (phase/lap-file-output lap-output-port))
	 (phase/assemble)
	 (if info-output-pathname
	     (phase/info-generation-2 info-output-pathname))
	 (phase/link)
	 *result*)))))

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
  (fluid-let ((*output-prefix* (string-append "    " *output-prefix*)))
    (newline)
    (write-string *output-prefix*)
    (write-string name)
    (write-string "...")
    (if compiler:show-time-reports?
	(let ((process-start *process-time*)
	      (real-start *real-time*))
	  (let ((value (thunk)))
	    (compiler-time-report "  Time taken"
				  (- *process-time* process-start)
				  (- *real-time* real-start))
	    value))
	(thunk))))

(define *output-prefix* "")
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
  (newline)
  (write-string *output-prefix*)
  (write-string prefix)
  (write-string ": ")
  (write (/ (exact->inexact process-time) 1000))
  (write-string " (process time); ")
  (write (/ (exact->inexact real-time) 1000))
  (write-string " (real time)"))

(define-macro (last-reference name)
  (let ((x (generate-uninterned-symbol)))
    `(IF COMPILER:PRESERVE-DATA-STRUCTURES?
	 ,name
	 (LET ((,x ,name))
	   (SET! ,name)
	   ,x))))

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
      (with-values
	  (lambda ()
	    (generate/top-level (last-reference *root-expression*)))
	(lambda (expression procedures continuations rgraphs)
	  (set! *rtl-expression* expression)
	  (set! *rtl-procedures* procedures)
	  (set! *rtl-continuations* continuations)
	  (set! *rtl-graphs* rgraphs)
	  unspecific))
      (if *procedure-result?*
	  (set! *rtl-expression* false))
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
	    (newline)
	    (write-string *output-prefix*)
	    (write-string "  Registers used: ")
	    (write (apply max n-registers))
	    (write-string " max, ")
	    (write (apply min n-registers))
	    (write-string " min, ")
	    (write
	     (exact->inexact (/ (apply + n-registers) (length n-registers))))
	    (write-string " mean"))))))

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
		  (set-rgraph-bblocks! rgraph false)
		  ;; **** this slot is reused. ****
		  ;;(set-rgraph-register-bblock! rgraph false)
		  (set-rgraph-register-crosses-call?! rgraph false)
		  (set-rgraph-register-n-deaths! rgraph false)
		  (set-rgraph-register-live-length! rgraph false)
		  (set-rgraph-register-n-refs! rgraph false)
		  (set-rgraph-register-known-values! rgraph false))
		*rtl-graphs*)))

(define (phase/rtl-file-output port)
  (compiler-phase "RTL File Output"
    (lambda ()
      (write-string "RTL for object " port)
      (write *recursive-compilation-number* port)
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
      (set! *next-constant* 0)
      (set! *interned-constants* '())
      (set! *interned-variables* '())
      (set! *interned-assignments* '())
      (set! *interned-uuo-links* '())
      (set! *interned-global-links* '())
      (set! *interned-static-variables* '())
      (set! *block-label* (generate-label))
      (set! *external-labels* '())
      (initialize-lap-linearizer!)
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
	    (let ((prefix (generate-lap *rtl-graphs* *remote-links* false)))
	      (node-insert-snode! (rtl-expr/entry-node *rtl-root*)
				  (make-sblock prefix)))
	    (set! *entry-label* (rtl-expr/label *rtl-root*))
	    unspecific)))))

(define (phase/lap-linearization)
  (compiler-phase "LAP Linearization"
    (lambda ()
      (set! *lap*
	    (optimize-linear-lap
	     (LAP ,@(if *procedure-result?*
			(LAP (ENTRY-POINT ,*entry-label*))
			(lap:make-entry-point *entry-label* *block-label*))
		  ,@(linearize-lap *rtl-root*
				   *rtl-procedures*
				   *rtl-continuations*))))
      (with-values
	  (lambda ()
	    (info-generation-phase-2 *rtl-expression*
				     *rtl-procedures*
				     *rtl-continuations*))
	(lambda (expression procedures continuations)
	  (set! *dbg-expression* expression)
	  (set! *dbg-procedures* procedures)
	  (set! *dbg-continuations* continuations)
	  unspecific))
      (if (not compiler:preserve-data-structures?)
	  (begin
	    (set! *rtl-expression*)
	    (set! *rtl-procedures*)
	    (set! *rtl-continuations*)
	    (set! *rtl-graphs*)
	    (set! label->object)
	    (set! *rtl-root*)
	    unspecific)))))

(define (phase/lap-file-output port)
  (compiler-phase "LAP File Output"
    (lambda ()
      (fluid-let ((*unparser-radix* 16)
		  (*unparse-uninterned-symbols-by-name?* true))
	(with-output-to-port port
	  (lambda ()
	    (write-string "LAP for object ")
	    (write *recursive-compilation-number*)
	    (newline)
	    (newline)
	    (for-each (lambda (instruction)
			(if (and (pair? instruction)
				 (eq? (car instruction) 'LABEL))
			    (begin
			      (write (cadr instruction))
			      (write-char #\:))
			    (begin
			      (write-char #\tab)
			      (write instruction)))
			(newline))
		      *lap*)
	    (if (not (zero? *recursive-compilation-number*))
		(begin
		  (write-char #\page)
		  (newline)))
	    (output-port/flush-output port)))))))

(define (phase/assemble)
  (compiler-phase "Assembly"
    (lambda ()
      (with-values (lambda () (assemble *block-label* (last-reference *lap*)))
	(lambda (count code-vector labels bindings linkage-info)
	  linkage-info			;ignored
	  (set! *code-vector* code-vector)
	  (set! *entry-points* labels)
	  (set! *label-bindings* bindings)
	  (if compiler:show-phases?
	      (begin
		(newline)
		(write-string *output-prefix*)
		(write-string "  Branch tensioning done in ")
		(write (1+ count))
		(write-string
		 (if (zero? count) " iteration." " iterations.")))))))))

(define (phase/info-generation-2 pathname)
  (info-generation-2 pathname set-compiled-code-block/debugging-info!))

(define (info-generation-2 pathname set-debugging-info!)
  (compiler-phase "Debugging Information Generation"
    (lambda ()
      (set-debugging-info!
       *code-vector*
       (let ((info
	      (info-generation-phase-3
	       (last-reference *dbg-expression*)
	       (last-reference *dbg-procedures*)
	       (last-reference *dbg-continuations*)
	       *label-bindings*
	       (last-reference *external-labels*))))
	 (cond ((eq? pathname 'KEEP)	; for dynamic execution
		info)
	       ((eq? pathname 'RECURSIVE) ; recursive compilation
		(set! *recursive-compilation-results*
		      (cons (vector *recursive-compilation-number*
				    info
				    *code-vector*)
			    *recursive-compilation-results*))
		(cons *info-output-filename* *recursive-compilation-number*))
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
		*info-output-filename*)))))))

;;; Various ways of dumping an info file

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

(define (compiler:dump-inf-file binf pathname)
  (fasdump binf pathname true)
  (announce-info-files pathname))
  
(define (compiler:dump-bif/bsm-files binf pathname)
  (let ((bif-path (pathname-new-type pathname "bif"))
	(bsm-path (pathname-new-type pathname "bsm")))
    (inf-structure->bif/bsm binf bif-path bsm-path)
    (announce-info-files bif-path bsm-path)))
  
(define (compiler:dump-bci/bcs-files binf pathname)
  (let ((bci-path (pathname-new-type pathname "bci"))
	(bcs-path (pathname-new-type pathname "bcs")))
    (load-option 'COMPRESS)
    (call-with-temporary-filename
      (lambda (bif-name)
	(let ((bif-path (merge-pathnames bif-name)))
	  (call-with-temporary-filename
            (lambda (bsm-name)
	      (let ((bsm-path (merge-pathnames bsm-name)))
		(inf-structure->bif/bsm binf bif-path bsm-path)
		(compress bif-path bci-path)
		(compress bsm-path bcs-path)
		(announce-info-files bci-path bcs-path)))))))))

(define (compiler:dump-bci-file binf pathname)
  (let ((bci-path (pathname-new-type pathname "bci")))
    (load-option 'COMPRESS)
    (call-with-temporary-filename
      (lambda (bif-name)
	(let ((bif-path (merge-pathnames bif-name)))
	  (inf-structure->bif/bsm binf bif-path false)
	  (compress bif-path bci-path)
	  (announce-info-files bci-path))))))

(define compiler:dump-info-file compiler:dump-bci-file)

(define (phase/link)
  (compiler-phase "Linkification"
    (lambda ()
      ;; This has sections locked against GC to prevent relocation
      ;; while computing addresses.
      (let* ((label->offset
	      (lambda (label)
		(cdr (or (assq label *label-bindings*)
			 (error "Missing entry point" label)))))
	     (bindings
	      (map (lambda (label)
		     (cons
		      label
		      (with-absolutely-no-interrupts
		       (lambda ()
			 ((ucode-primitive primitive-object-set-type)
			  type-code:compiled-entry
			  (make-non-pointer-object
			   (+ (label->offset label)
			      (object-datum *code-vector*))))))))
		   *entry-points*))
	     (label->address
	      (lambda (label)
		(cdr (or (assq label bindings)
			 (error "Label not defined as entry point"
				label))))))
	(set! *result*
	      (if *procedure-result?*
		  (let ((linking-info *subprocedure-linking-info*))
		    (let ((compiled-procedure (label->address *entry-label*))
			  (translate-label
			   (let ((block-offset (label->offset *block-label*)))
			     (lambda (index)
			       (let ((label (vector-ref linking-info index)))
				 (and label
				      (- (label->offset label)
					 block-offset)))))))
		      (cons compiled-procedure
			    (vector
			     (compiled-code-address->block compiled-procedure)
			     (translate-label 0)
			     (translate-label 1)
			     (vector-ref linking-info 2)))))
		  (label->address *entry-label*)))
	(for-each (lambda (entry)
		    (set-lambda-body! (car entry)
				      (label->address (cdr entry))))
		  *ic-procedure-headers*))
      (if (not compiler:preserve-data-structures?)
	  (begin
	    (set! *code-vector*)
	    (set! *entry-points*)
	    (set! *subprocedure-linking-info*)
	    (set! *label-bindings*)
	    (set! *block-label*)
	    (set! *entry-label*)
	    (set! *ic-procedure-headers*)
	    unspecific)))))