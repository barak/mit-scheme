#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/toplev.scm,v 4.1 1987/12/04 20:05:18 cph Exp $

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

;;; These variable names mistakenly use the format "compiler:..."
;;; instead of the correct format, which is "*...*".  Fix it sometime.
(define compiler:continuation-fp-offsets)
(define compiler:external-labels)
(define compiler:label-bindings)

(define compiler:phase-wrapper false)
(define compiler:compile-time 0)

(define (compile-bin-file input-string #!optional output-string)
  (compiler-pathnames input-string
		      (and (not (unassigned? output-string)) output-string)
		      (make-pathname false false false "bin" 'NEWEST)
    (lambda (input-pathname output-pathname)
      (compile-scode (compiler-fasload input-pathname)
		     (pathname-new-type output-pathname "brtl")
		     (pathname-new-type output-pathname "binf")))))

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
  (scode-eval (compile-scode (procedure-lambda procedure))
	      (procedure-environment procedure)))

(define (compiler-pathnames input-string output-string default transform)
  (let ((input-pathname
	 (pathname->input-truename
	  (merge-pathnames (->pathname input-string) default))))
    (if (not input-pathname)
	(error "File does not exist" input-string))
    (let ((output-pathname
	   (let ((output-pathname (pathname-new-type input-pathname "com")))
	     (if output-string
		 (merge-pathnames (->pathname output-string) output-pathname)
		 output-pathname))))
      (newline)
      (write-string "Compile File: ")
      (write (pathname->string input-pathname))
      (write-string " => ")
      (write (pathname->string output-pathname))
      (fasdump (transform input-pathname output-pathname) output-pathname))))

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
     (phase/simulate-application)
     (phase/outer-analysis)
     (phase/fold-constants)
     (phase/open-coding-analysis)
     (phase/operator-analysis)
     (phase/identify-closure-limits)
     (phase/setup-block-types)
     (phase/continuation-analysis)
     (phase/simplicity-analysis)
     (phase/subproblem-ordering)
     (phase/design-environment-frames)
     (phase/rtl-generation)
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
       (write-string " mean"))
#|
     (if info-output-pathname
	 (compiler:info-generation-1 info-output-pathname))
     (compiler:rtl-generation-cleanup)
     (if compiler:cse?
	 (compiler:cse))
     (compiler:lifetime-analysis)
     (if compiler:code-compression?
	 (compiler:code-compression))
     (if rtl-output-pathname
	 (compiler:rtl-file-output rtl-output-pathname))
     (compiler:register-allocation)
     (compiler:rtl-optimization-cleanup)
     (compiler:bit-generation)
     (compiler:bit-linearization)
     (compiler:assemble)
     (if info-output-pathname
	 (compiler:info-generation-2 info-output-pathname))
     (compiler:link)
     compiler:expression
|#
     )))

(define (in-compiler thunk)
  (fluid-let ((compiler:compile-time 0)
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
	      (compiler:continuation-fp-offsets)
	      (compiler:external-labels)
	      (compiler:label-bindings)|#)
    (compiler:reset!)
    (let ((value (thunk)))
;      (compiler:reset!)
      (newline)
      (write-string "Total compilation time: ")
      (write compiler:compile-time)
      value)))

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
  (set! compiler:continuation-fp-offsets)
  (set! compiler:external-labels)
  (set! compiler:label-bindings))

(define (compiler-phase name thunk)
  (write-line name)
  (let ((delta
	 (let ((start-time (runtime)))
	   (if compiler:phase-wrapper
	       (compiler:phase-wrapper thunk)
	       (thunk))
	   (- (runtime) start-time))))
    (set! compiler:compile-time (+ delta compiler:compile-time))
    (newline)
    (write-string "Time taken: ")
    (write delta)))
#|
(define-macro (last-reference name)
  (let ((temp (generate-uninterned-symbol)))
    `(IF COMPILER:PRESERVE-DATA-STRUCTURES?
	 ,name
	 (LET ((,temp name))
	   (set! ,name)
	   ,temp))))
|#

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
	    ((access construct-graph fg-generator-package) *input-scode*))
      (set! *root-block* (expression-block *root-expression*))
      (if (or (null? *expressions*)
	      (not (null? (cdr *expressions*))))
	  (error "Multiple expressions"))
      (set! *expressions*))))

(define (phase/simulate-application)
  (compiler-phase 'SIMULATE-APPLICATION
    (lambda ()
      ((access simulate-application fg-analyzer-package)
       *lvalues*
       *applications*))))

(define (phase/outer-analysis)
  (compiler-phase 'OUTER-ANALYSIS
    (lambda ()
      ((access outer-analysis fg-analyzer-package)
       *root-expression*
       *procedures*
       *applications*))))

(define (phase/fold-constants)
  (compiler-phase 'FOLD-CONSTANTS
    (lambda ()
      ((access fold-constants fg-analyzer-package)
       *lvalues*
       *applications*))))

(define (phase/open-coding-analysis)
  (compiler-phase 'OPEN-CODING-ANALYSIS
    (lambda ()
      ((access open-coding-analysis rtl-generator-package)
       *applications*))))

(define (phase/operator-analysis)
  (compiler-phase 'OPERATOR-ANALYSIS
    (lambda ()
      ((access operator-analysis fg-analyzer-package)
       *procedures*
       *applications*))))

(define (phase/identify-closure-limits)
  (compiler-phase 'IDENTIFY-CLOSURE-LIMITS
    (lambda ()
      ((access identify-closure-limits! fg-analyzer-package)
       *procedures*
       *applications*
       *assignments*))))

(define (phase/setup-block-types)
  (compiler-phase 'SETUP-BLOCK-TYPES
    (lambda ()
      ((access setup-block-types! fg-analyzer-package)
       *root-block*))))

(define (phase/continuation-analysis)
  (compiler-phase 'CONTINUATION-ANALYSIS
    (lambda ()
      ((access continuation-analysis fg-analyzer-package)
       *blocks*
       *procedures*))))

(define (phase/simplicity-analysis)
  (compiler-phase 'SIMPLICITY-ANALYSIS
    (lambda ()
      ((access simplicity-analysis fg-analyzer-package)
       *parallels*))))

(define (phase/subproblem-ordering)
  (compiler-phase 'SUBPROBLEM-ORDERING
    (lambda ()
      ((access subproblem-ordering fg-analyzer-package)
       *parallels*))))

(define (phase/design-environment-frames)
  (compiler-phase 'DESIGN-ENVIRONMENT-FRAMES
    (lambda ()
      ((access design-environment-frames! fg-analyzer-package)
       *blocks*))))

(define (phase/rtl-generation)
  (compiler-phase 'RTL-GENERATION
    (lambda ()
      (set! *rtl-procedures* '())
      (set! *rtl-continuations* '())
      (set! *rtl-graphs* '())
      ((access generate/top-level rtl-generator-package) *root-expression*))))