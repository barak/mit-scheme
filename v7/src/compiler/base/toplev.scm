#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/toplev.scm,v 4.3 1987/12/30 09:09:57 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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
(define *input-scode*)
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
  (set! *input-scode*)
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

(define (in-compiler thunk)
  (fluid-let ((compiler:process-time 0)
	      (compiler:real-time 0)
	      #|(*input-scode*)
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
	      (*root-block*)
	      (*rtl-expression*)
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
	      (compiler:expression)|#)
    (compiler:reset!)
    (let ((value (thunk)))
      (if (not compiler:preserve-data-structures?)
	  (compiler:reset!))
      (compiler-time-report "Total compilation time"
			    compiler:process-time
			    compiler:real-time)
      value)))

(define (compile-bin-file input-string #!optional output-string)
  (compiler-pathnames input-string
		      (and (not (unassigned? output-string)) output-string)
		      (make-pathname false false false "bin" 'NEWEST)
    (lambda (input-pathname output-pathname)
      (compile-scode (compiler-fasload input-pathname)
		     (and compiler:generate-rtl-files?
			  (pathname-new-type output-pathname "brtl"))
		     (pathname-new-type output-pathname "binf")))))

(define (compiler-pathnames input-string output-string default transform)
  (let ((kernel
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
			output-pathname))))))
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

(define (compile-scode scode
		       #!optional
		       rtl-output-pathname
		       info-output-pathname)

  (if (unassigned? rtl-output-pathname)
      (set! rtl-output-pathname false))
  (if (unassigned? info-output-pathname)
      (set! info-output-pathname false))

  (in-compiler
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
     compiler:expression
     )))

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
  (write-line name)
  (let ((process-start (process-time-clock))
	(real-start (real-time-clock)))
    (thunk)
    (let ((process-delta (- (process-time-clock) process-start))
	  (real-delta (- (real-time-clock) real-start)))
      (set! compiler:process-time (+ process-delta compiler:process-time))
      (set! compiler:real-time (+ real-delta compiler:real-time))
      (compiler-time-report "Time taken" process-delta real-delta))))

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
  (compiler-phase 'FG-GENERATION
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
      (set! *root-expression*
	    ((access construct-graph fg-generator-package)
	     (if compiler:preserve-data-structures?
		 *input-scode*
		 (set! *input-scode*))))
      (set! *root-block* (expression-block *root-expression*))
      (if (or (null? *expressions*)
	      (not (null? (cdr *expressions*))))
	  (error "Multiple expressions"))
      (set! *expressions*))))

(define (phase/fg-optimization)
  (compiler-superphase 'FG-OPTIMIZATION
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
  (compiler-subphase 'SIMULATE-APPLICATION
    (lambda ()
      ((access simulate-application fg-optimizer-package)
       *lvalues*
       *applications*))))

(define (phase/outer-analysis)
  (compiler-subphase 'OUTER-ANALYSIS
    (lambda ()
      ((access outer-analysis fg-optimizer-package)
       *root-expression*
       *procedures*
       *applications*))))

(define (phase/fold-constants)
  (compiler-subphase 'FOLD-CONSTANTS
    (lambda ()
      ((access fold-constants fg-optimizer-package)
       *lvalues*
       *applications*))))

(define (phase/open-coding-analysis)
  (compiler-subphase 'OPEN-CODING-ANALYSIS
    (lambda ()
      ((access open-coding-analysis rtl-generator-package)
       *applications*))))

(define (phase/operator-analysis)
  (compiler-subphase 'OPERATOR-ANALYSIS
    (lambda ()
      ((access operator-analysis fg-optimizer-package)
       *procedures*
       *applications*))))

(define (phase/identify-closure-limits)
  (compiler-subphase 'IDENTIFY-CLOSURE-LIMITS
    (lambda ()
      ((access identify-closure-limits! fg-optimizer-package)
       *procedures*
       *applications*
       *assignments*))))

(define (phase/setup-block-types)
  (compiler-subphase 'SETUP-BLOCK-TYPES
    (lambda ()
      ((access setup-block-types! fg-optimizer-package)
       *root-block*))))

(define (phase/continuation-analysis)
  (compiler-subphase 'CONTINUATION-ANALYSIS
    (lambda ()
      ((access continuation-analysis fg-optimizer-package)
       *blocks*))))

(define (phase/simplicity-analysis)
  (compiler-subphase 'SIMPLICITY-ANALYSIS
    (lambda ()
      ((access simplicity-analysis fg-optimizer-package)
       *parallels*))))

(define (phase/subproblem-ordering)
  (compiler-subphase 'SUBPROBLEM-ORDERING
    (lambda ()
      ((access subproblem-ordering fg-optimizer-package)
       *parallels*))))

(define (phase/connectivity-analysis)
  (compiler-subphase 'CONNECTIVITY-ANALYSIS
    (lambda ()
      ((access connectivity-analysis fg-optimizer-package)
       *root-expression*
       *procedures*))))

(define (phase/design-environment-frames)
  (compiler-subphase 'DESIGN-ENVIRONMENT-FRAMES
    (lambda ()
      ((access design-environment-frames! fg-optimizer-package)
       *blocks*))))

(define (phase/compute-node-offsets)
  (compiler-subphase 'COMPUTE-NODE-OFFSETS
    (lambda ()
      ((access compute-node-offsets fg-optimizer-package)
       *root-expression*))))

(define (phase/fg-optimization-cleanup)
  (compiler-subphase 'FG-OPTIMIZATION-CLEANUP
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
  (compiler-phase 'RTL-GENERATION
    (lambda ()
      (set! *rtl-procedures* '())
      (set! *rtl-continuations* '())
      (set! *rtl-graphs* '())
      (set! *ic-procedure-headers* '())
      (initialize-machine-register-map!)
      ((access generate/top-level rtl-generator-package)
       (if compiler:preserve-data-structures?
	   *root-expression*
	   (set! *root-expression*)))
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
	(write-string "Registers used: ")
	(write (apply max n-registers))
	(write-string " max, ")
	(write (apply min n-registers))
	(write-string " min, ")
	(write (/ (apply + n-registers) (length n-registers)))
	(write-string " mean")))))

(define (phase/rtl-optimization)
  (compiler-superphase 'RTL-OPTIMIZATION
    (lambda ()
      (if compiler:cse?
	  (phase/common-subexpression-elimination))
      (phase/lifetime-analysis)
      (if compiler:code-compression?
	  (phase/code-compression))
      (phase/register-allocation)
      (phase/rtl-optimization-cleanup))))

(define (phase/common-subexpression-elimination)
  (compiler-subphase 'COMMON-SUBEXPRESSION-ELIMINATION
    (lambda ()
      ((access common-subexpression-elimination rtl-cse-package)
       *rtl-graphs*))))
(define (phase/lifetime-analysis)
  (compiler-subphase 'LIFETIME-ANALYSIS
    (lambda ()
      ((access lifetime-analysis rtl-optimizer-package) *rtl-graphs*))))

(define (phase/code-compression)
  (compiler-subphase 'CODE-COMPRESSION
    (lambda ()
      ((access code-compression rtl-optimizer-package) *rtl-graphs*))))

(define (phase/rtl-file-output pathname)
  (compiler-phase 'RTL-FILE-OUTPUT
    (lambda ()
      (fasdump ((access linearize-rtl rtl-generator-package) *rtl-graphs*)
	       pathname))))

(define (phase/register-allocation)
  (compiler-subphase 'REGISTER-ALLOCATION
    (lambda ()
      ((access register-allocation rtl-optimizer-package) *rtl-graphs*))))

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
  (compiler-phase 'BIT-GENERATION
    (lambda ()
      (set! compiler:external-labels '())
      ((access generate-bits lap-syntax-package)
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
  (compiler-phase 'BIT-LINEARIZATION
    (lambda ()
      (set! compiler:bits
	    (LAP ,@(lap:make-entry-point compiler:entry-label
					 compiler:block-label)
		 ,@((access linearize-bits lap-syntax-package)
		    (if compiler:preserve-data-structures?
			*rtl-graphs*
			(set! *rtl-graphs*))))))))

(define (phase/assemble)
  (compiler-phase 'ASSEMBLE
    (lambda ()
      (if compiler:preserve-data-structures?
	  ((access assemble bit-package)
	   compiler:block-label
	   compiler:bits
	   phase/assemble-finish)
	  ((access assemble bit-package)
	   (set! compiler:block-label)
	   (set! compiler:bits)
	   phase/assemble-finish)))))

(define (phase/assemble-finish code-vector labels bindings linkage-info)
  (set! compiler:code-vector code-vector)
  (set! compiler:entry-points labels)
  (set! compiler:label-bindings bindings))

(define (phase/info-generation-2 pathname)
  (compiler-phase 'DEBUGGING-INFO-GENERATION-2
    (lambda ()
      (fasdump ((access generation-phase2 debugging-information-package)
		compiler:label-bindings
		(if compiler:preserve-data-structures?
		    compiler:external-labels
		    (set! compiler:external-labels)))
	       pathname)
      (set-compiled-code-block/debugging-info! compiler:code-vector
					       (pathname->string pathname)))))

(define (phase/link)
  (compiler-phase 'LINK
    (lambda ()
      ;; This has sections locked against GC since the code may not be
      ;; purified.
      (let ((bindings
	     (map (lambda (label)
		    (cons
		     label
		     (with-interrupt-mask interrupt-mask-none
		       (lambda (old)
			 ((ucode-primitive &make-object)
			  type-code:compiled-expression
			  (make-non-pointer-object
			   (+ (cdr (or (assq label compiler:label-bindings)
				       (error "Missing entry point" label)))
			      (primitive-datum compiler:code-vector))))))))
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