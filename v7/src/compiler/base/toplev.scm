#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/toplev.scm,v 4.23 1989/11/02 08:08:04 cph Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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
  (compiler-pathnames input-string
		      (and (not (default-object? output-string)) output-string)
		      (make-pathname false false false false "bin" 'NEWEST)
    (lambda (input-pathname output-pathname)
      (compile-scode (compiler-fasload input-pathname)
		     (and compiler:generate-rtl-files?
			  (pathname-new-type output-pathname "brtl"))
		     (pathname-new-type output-pathname "binf"))))
  unspecific)

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

;;;; Alternate Entry Points

(define (compile-procedure procedure)
  (scode-eval (compile-scode (procedure-lambda procedure) false false)
	      (procedure-environment procedure)))

(define (compiler:batch-compile input #!optional output)
  (fluid-let ((compiler:batch-mode? true))
    (bind-condition-handler '() compiler:batch-error-handler
      (lambda ()
	(if (default-object? output)
	    (compile-bin-file input)
	    (compile-bin-file input output))))))

(define (compiler:batch-error-handler condition)
  (and (not (condition/internal? condition))
       (condition/error? condition)
       (begin
	 (warn (condition/report-string condition))
	 (compiler:abort false))))

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

(define (compile-recursively scode procedure-result?)
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
		       (compile-scode scode
				      (and *rtl-output-pathname* true)
				      (and *info-output-filename* true)
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
			    (write-to-string (lambda-name scode)))
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
(define *recursive-compilation-rtl-blocks*)
(define *procedure-result?*)
(define *remote-links*)
(define *process-time*)
(define *real-time*)

(define *info-output-filename* false)
(define *rtl-output-pathname* false)

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
;; Last used: phase/bit-linearization
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

;; First set: phase/bit-generation
;; Last used: phase/info-generation-2
(define *external-labels*)

;; First set: phase/bit-generation
;; Last used: phase/link
(define *subprocedure-linking-info*)

;; First set: phase/bit-linearization
;; Last used: phase/assemble
(define *bits*)

;; First set: phase/bit-linearization
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
		    (let ((others (recursive-compilation-results)))
		      (if (null? others)
			  expression
			  (scode/make-comment
			   (make-dbg-info-vector
			    (let* ((others
				    (map (lambda (other) (vector-ref other 2))
					 others))
				   (all-blocks
				    (list->vector
				     (cons
				      (compiled-code-address->block expression)
				      others))))
			      (if compiler:compile-by-procedures?
				  (list 'COMPILED-BY-PROCEDURES
					all-blocks
					(list->vector others))
				  all-blocks)))
			   expression))))))
	     (compiler-time-report "Total compilation time"
				   *process-time*
				   *real-time*)
	     value))))
    (if compiler:preserve-data-structures?
	(begin
	  (compiler:reset!)
	  (run-compiler))
	(fluid-let ((*recursive-compilation-number* 0)
		    (*recursive-compilation-count* 1)
		    (*recursive-compilation-results* '())
		    (*recursive-compilation-rtl-blocks* '())
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
	      (*bits*)
	      (*next-constant*)
	      (*interned-constants*)
	      (*interned-variables*)
	      (*interned-assignments*)
	      (*interned-uuo-links*)
	      (*constants*)
	      (*blocks*)
	      (*expressions*)
	      (*procedures*)
	      (*lvalues*)
	      (*applications*)
	      (*parallels*))
    (fluid-let ((*input-scode*)
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
  (set! *recursive-compilation-rtl-blocks* '())
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
  (set! *bits*)
  (set! *next-constant*)
  (set! *interned-constants*)
  (set! *interned-variables*)
  (set! *interned-assignments*)
  (set! *interned-uuo-links*)
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

(define (compile-scode scode
		       #!optional
		       rtl-output-pathname
		       info-output-pathname
		       wrapper)
  (let ((rtl-output-pathname
	 (if (default-object? rtl-output-pathname)
	     false
	     rtl-output-pathname))
	(info-output-pathname
	 (if (default-object? info-output-pathname)
	     false
	     info-output-pathname))
	(wrapper
	 (if (default-object? wrapper) in-compiler wrapper)))
    (fluid-let ((*info-output-filename*
		 (if (pathname? info-output-pathname)
		     (pathname->string info-output-pathname)
		     *info-output-filename*))
		(*rtl-output-pathname*
		 (if (pathname? rtl-output-pathname)
		     rtl-output-pathname
		     *rtl-output-pathname*)))
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
	 (if rtl-output-pathname
	     (phase/rtl-file-output rtl-output-pathname))
	 (phase/bit-generation)
	 (phase/bit-linearization)
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
  (write (/ process-time 1000))
  (write-string " (process time); ")
  (write (/ real-time 1000))
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
			    (set-procedure-free-callers! procedure '())
			    (set-procedure-variables! procedure '()))))
		    *procedures*)))))

(define (phase/setup-block-types)
  (compiler-subphase "Block Type Determination"
    (lambda ()
      (setup-block-types! *root-block*)
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
	    (write (/ (apply + n-registers) (length n-registers)))
	    (write-string " mean"))))))

(define (phase/rtl-optimization)
  (compiler-superphase "RTL Optimization"
    (lambda ()
      (if compiler:cse?
	  (phase/common-subexpression-elimination))
      (phase/invertible-expression-elimination)
      (phase/common-suffix-merging)
      (phase/lifetime-analysis)
      (if compiler:code-compression?
	  (phase/code-compression))
      (phase/linearization-analysis)
      (phase/register-allocation)
      (phase/rtl-optimization-cleanup))))

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
		  (set-rgraph-register-n-refs! rgraph false))
		*rtl-graphs*)))

(define (phase/rtl-file-output pathname)
  (compiler-phase "RTL File Output"
    (lambda ()
      (let ((rtl
	     (linearize-rtl *rtl-root*
			    *rtl-procedures*
			    *rtl-continuations*)))
	(if (eq? pathname true)
	    ;; recursive compilation
	    (begin
	      (set! *recursive-compilation-rtl-blocks*
		    (cons (cons *recursive-compilation-number* rtl)
			  *recursive-compilation-rtl-blocks*))
	      unspecific)
	    (fasdump (if (null? *recursive-compilation-rtl-blocks*)
			 rtl
			 (list->vector
			  (cons (cons 0 rtl)
				*recursive-compilation-rtl-blocks*)))
		     pathname))))))

(define (phase/bit-generation)
  (compiler-phase "LAP Generation"
    (lambda ()
      (set! *next-constant* 0)
      (set! *interned-constants* '())
      (set! *interned-variables* '())
      (set! *interned-assignments* '())
      (set! *interned-uuo-links* '())
      (set! *block-label* (generate-label))
      (set! *external-labels* '())
      (if *procedure-result?*
	  (generate-bits *rtl-graphs* '()
	    (lambda (prefix environment-label free-ref-label n-sections)
	      (node-insert-snode! (rtl-procedure/entry-node *rtl-root*)
				  (make-sblock prefix))
	      (set! *entry-label*
		    (rtl-procedure/external-label *rtl-root*))
	      (set! *subprocedure-linking-info*
		    (vector environment-label free-ref-label n-sections))
	      unspecific))
	  (begin
	    (let ((prefix (generate-bits *rtl-graphs* *remote-links* false)))
	      (node-insert-snode! (rtl-expr/entry-node *rtl-root*)
				  (make-sblock prefix)))
	    (set! *entry-label* (rtl-expr/label *rtl-root*))
	    unspecific)))))

(define (phase/bit-linearization)
  (compiler-phase "LAP Linearization"
    (lambda ()
      (set! *bits*
	    (append-instruction-sequences!
	     (if *procedure-result?*
		 (LAP (ENTRY-POINT ,*entry-label*))
		 (lap:make-entry-point *entry-label* *block-label*))
	     (linearize-bits *rtl-root*
			     *rtl-procedures*
			     *rtl-continuations*)))
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

(define (phase/assemble)
  (compiler-phase "Assembly"
    (lambda ()
      (assemble *block-label* (last-reference *bits*)
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
  (compiler-phase "Debugging Information Generation"
    (lambda ()
      (set-compiled-code-block/debugging-info!
       *code-vector*
       (let ((info
	      (info-generation-phase-3
	       (last-reference *dbg-expression*)
	       (last-reference *dbg-procedures*)
	       (last-reference *dbg-continuations*)
	       *label-bindings*
	       (last-reference *external-labels*))))
	 (if (eq? pathname true)	; recursive compilation
	     (begin
	       (set! *recursive-compilation-results*
		     (cons (vector *recursive-compilation-number*
				   info
				   *code-vector*)
			   *recursive-compilation-results*))
	       (cons *info-output-filename* *recursive-compilation-number*))
	     (begin
	       (fasdump (let ((others (recursive-compilation-results)))
			  (if (null? others)
			      info
			      (list->vector
			       (cons info
				     (map (lambda (other) (vector-ref other 1))
					  others)))))
			pathname)
	       *info-output-filename*)))))))

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
			 ((ucode-primitive &make-object)
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