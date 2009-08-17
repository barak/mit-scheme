#| -*-Scheme-*-

$Id: 90f48017c02972f2fc283c96ac8f584c1d40b6a5 $

Copyright (c) 1994, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(declare (usual-integrations))

;;;; Phase structure

(define *phases-to-omit* '())
(define *debugging?* true)
(define *current-phase-input* false)
(define *entry-label*)

(define debugging-phase-wrapper
  (let ((pending-message #F))

    (lambda (proc this-phase next-phase)
      (define (show-message message)
	(newline)
	;(write-string ";---")
	(write-string message)
	(write this-phase))

      (define (show-program message program)
	(newline)
	(write-char #\Page)
	(if pending-message
	    (display pending-message))
	(set! pending-message #F)
	(show-message message)
	(write-string " #@") (display (hash program))
	(if compiler:kmp-output-abbreviated?
	    (begin
	      (write-string " (compiler:kmp-output-abbreviated? is #T)")
	      (newline)
	      (kmp/ppp program))
	    (begin
	      (newline)
	      (kmp/pp program))))
    
      (define (show? phase)
	(and phase
	     (let ((switch compiler:generate-kmp-files?))
	       (or (eq? switch 'ALL)
		   (and (pair? switch)
			(memq phase switch))))))

      (define (run-phase program)
	(if (memq this-phase *phases-to-omit*)
	    program
	    (proc program)))
      
      (define gather-phase-statistics
	(let ((kind1  (list (symbol-append 
			     'GROWTH/
			     (string->symbol (write-to-string this-phase)))
			    'AVERAGE)))
	  (lambda (program result)
	    (and (pair? program)
		 (pair? result)
		 (begin
		   (sample/1 kind1
			     (lambda ()
			       (/ (exact->inexact (kmp-program-size result))
				  (kmp-program-size program)))))))))

      (lambda (program)
	(set! *current-phase* this-phase)
	(set! *current-phase-input* (and *debugging?* program))
	(phase/pre-hook program)
	(if (and (or compiler:show-subphases? compiler:guru?)
		 (memq this-phase *phases-to-omit*))
	    (begin
	      (newline)
	      (write-string *output-prefix*)
	      (write-string "  Omitting ")
	      (write this-phase)
	      (write-string " (see *phases-to-omit*)")))
	(let ((result 
	       (if (not (show? this-phase))
		   (run-phase program)
		   (begin
		     (with-kmp-output-port
		      (lambda ()
			(show-program "Input to phase " program)))
		     (let ((result (run-phase program)))
		       (if (show? next-phase)
			   (set! pending-message
				 (with-output-to-string
				   (lambda ()
				     (show-message "Output from phase "))))
			   (with-kmp-output-port
			    (lambda ()
			      (show-program "Output from phase " result))))
		       result)))))
	  (phase/post-hook program result)
	  ;;(gather-phase-statistics program result)
	  result)))))

(define (phase-wrapper name rewrite)
  (lambda (program)
    (let ((table *code-rewrite-table*))
      (set! *previous-code-rewrite-table* table)
      (set! *code-rewrite-table* (and table (code/rewrite-table/make)))
      (compiler-subphase
       (with-output-to-string (lambda () (write name)))
       (lambda () (rewrite program))))))

(define (dummy-phase rewrite)
  (lambda (program)
    (set! *code-rewrite-table* *previous-code-rewrite-table*)
    (rewrite program)))

(define (phase/pre-hook program)
  program
  unspecific)

(define (phase/post-hook program program*)
  program program*
  unspecific)
#|
Example:
(define *phase/pp/ann?* #T)
(define *phases-to-pp/ann* 'all)
(define (phase/post-hook prog result)
  (if (and *phase/pp/ann?*
	   (or (eq? *phases-to-pp/ann* 'all)
	       (memq *current-phase* *phases-to-pp/ann*)))
      (begin
	(let ((table *code-rewrite-table*))
	  (pp/ann result
		  (lambda (e) (monotonic-strong-eq-hash-table/get table e #F))))
	(pp `(phase is ,*current-phase*))
	(bkpt ";; proceed"))))
|#

;;;; Top level

(define *current-phase* 'UNKNOWN)
(define *allow-random-choices?* false)
(define *after-cps-conversion?* false)
(define *lift-closure-lambdas?* false)
(define *flush-closure-calls?* false)
(define *order-of-argument-evaluation* 'ANY) ; LEFT-TO-RIGHT, RIGHT-TO-LEFT
(define *earlyrew-expand-genarith?* false)
(define *sup-good-factor* 512)
(define *variable-properties* false)
(define *previous-code-rewrite-table* false)
(define *code-rewrite-table* false)

(let-syntax ((cascade
	      (macro all
		(let ((name (generate-uninterned-symbol 'FORM)))
		  (let loop ((result name)
			     (all all))
		    (if (null? all)
			`(lambda (,name)
			   ,result)
			(loop `((debugging-phase-wrapper
				 (phase-wrapper ',(car all) ,(car all))
				 ',(car all)
				 ',(if (null? (cdr all))
				       false
				       (cadr all)))
				,result)
			      (cdr all))))))))

  (define compile-0
    (cascade inlate/top-level		; scode->kmp-scheme
	     ))

  (define compile-1
    (cascade envconv/top-level		; eliminate free variables
					;  and (the-environment)
					;  introducing cache references
					; rewriting LOOKUP, SET!, etc.
	     ))

  (define compile-2
    (cascade alphaconv/top-level        ; makes all bindings have unique names
	     expand/top-level		; rewrite OR, and DELAY
	     assconv/top-level		; eliminate SET! and introduce LETREC
					;  rewriting LOOKUP and SET!
	     cleanup/top-level/1	; as below
	     coerce/top-level

	     earlyrew/top-level		; rewrite -1+ into -, etc.
	     typerew/top-level		; safety and type inference based
					;  operator replacement

	     ;;!frag/top-level
	     lamlift/top-level/1	; flatten environment structure
					; splitting lambda nodes if necessary
	     ;;!cleanup/top-level/1.5
	     ;;!arity/top-level

	     closconv/top-level/1	; introduce %make-heap-closure
					;  and %heap-closure-ref
					;  after this pass there are no
					;  non-local variable references
	     ;; staticfy/top-level	; broken, for now
	     applicat/top-level		; get rid of #!OPTIONAL and #!REST when
					;  calling known operators
					;  Introduce %internal-apply



	     simplify/top-level/1	; 1st-half of beta substitution
					;  replace variable operators with
					;  lambda expressions
	     cleanup/top-level/2	; 2nd-half of beta substitution
					;  substituting values for bindings


	     split/top-level
	     simplify/top-level/4	; as above
	     cleanup/top-level/5	; as above

	     ;;widen/top-level
	     ;;simplify/top-level/5	; as above
	     ;;cleanup/top-level/6	; as above

	     cpsconv/top-level/1	; cps conversion, sequencing of
					;  parallel expressions
	     simplify/top-level/2	; as above
	     cleanup/top-level/3	; as above
	     lamlift/top-level/2	; as above

	     closconv/top-level/2	; as above, but using
					;  %make-stack-closure and
					;  %stack-closure-ref
	     simplify/top-level/3	; as above
	     cleanup/top-level/4	; as above


	     ;;simplify/top-level
	     ;;cleanup/top-level
	     ;;simplify/top-level
	     ;;cleanup/top-level

	     laterew/top-level		; rewrite &+, vector-cons,
	     cleanup/top-level/7	; as above
	     compat/top-level		; rewrite code for compatibility
					;  with current compiled code
	     stackopt/top-level		; reformat stack closures to use
					;  common formats (prefixes)
	     ;; stackopt/optional-debugging-paranoia
	     ;;indexify/top-level	; OBSOLETE rewrite %vector-index
	     dbg-reduce/top-level	; final environment mappings
	     ))

  (define %optimized-kmp->rtl
    (cascade rtlgen/top-level))

  (define compile-0*
    (cascade (dummy-phase compile-0)
	     (dummy-phase compile-1)
	     (dummy-phase compile-2)))

  (define compile-1*
    (cascade (dummy-phase compile-1)
	     (dummy-phase compile-2))))

(define (within-midend recursive? thunk)
  (fluid-let ((*current-phase* false)
	      (*current-phase-input* false)
	      (*variable-properties*
	       (if (not recursive?)
		   (make-variable-properties)
		   (copy-variable-properties)))
	      (*after-cps-conversion?* false)
	      (*previous-code-rewrite-table* false)
	      (*dbg-rewrites* (dbg-info/make-rewrites))
	      ;;(*dbg-rewrites*
	      ;; (if (not recursive?) (dbg-info/make-rewrites) *dbg-rewrites*))
	      (*code-rewrite-table*
	       (if (not recursive?)
		   (code/rewrite-table/make)
		   (code/rewrite-table/copy *previous-code-rewrite-table*))))
    (if (not recursive?)
	(begin
	  ;; Initialize the uninterned symbol generator
	  ;; in order to obtain comparable programs
	  (generate-uninterned-symbol 'initial)
	  (generate-uninterned-symbol 0)
	  (initialize-new-variable!)))
    (thunk)))

(define *last-code-rewrite-table*)

(define (compile program)
  (within-midend false
    (lambda ()
      (let ((result (compile-0* program)))
	(set! *last-code-rewrite-table* *code-rewrite-table*)
	result))))

(define (scode->kmp program)
  (compile-0 program))

(define (optimize-kmp recursive? program)
  recursive?				; ignored
  (compile-1* program))

(define (kmp->rtl program)
  (fluid-let ((*entry-label* false))
    (let ((code (%optimized-kmp->rtl program)))
      (values code *entry-label*))))

(define (compile-recursively program procedure? name)
  ;; (values result must-be-called?)
  (compile-recursively/new program procedure? name))

;; Some of these have independent names only for debugging

(define (cpsconv/top-level/1 program)
  (let ((result (cpsconv/top-level program)))
    (set! *after-cps-conversion?* true)
    result))

(define (lamlift/top-level/1 program)
  (lamlift/top-level program))

(define (lamlift/top-level/2 program)
  (lamlift/top-level program))

(define (split/top-level program)
  (split-and-drift program))

(define (widen/top-level program)
  (widen-parameter-lists program))

(define (closconv/top-level/1 program)
  (closconv/top-level program *after-cps-conversion?*))

(define (closconv/top-level/2 program)
  (closconv/top-level program *after-cps-conversion?*))

(define (simplify/top-level/1 program)
  (fluid-let ((*simplify/open-code-expression-limit* 2))
    (simplify/top-level program)))

(define (simplify/top-level/2 program)
  (simplify/top-level program))

(define (simplify/top-level/3 program)
  (simplify/top-level program))

(define (simplify/top-level/4 program)
  (simplify/top-level program))

(define (simplify/top-level/5 program)
  (simplify/top-level program))

(define (simplify/top-level/6 program)
  (simplify/top-level program))

(define (cleanup/top-level/1 program)
  (cleanup/top-level program))

(define (cleanup/top-level/1.5 program)
  (cleanup/top-level program))

(define (cleanup/top-level/2 program)
  (fluid-let ((*flush-closure-calls?* true))
    (cleanup/top-level program)))

(define (cleanup/top-level/3 program)
  (cleanup/top-level program))

(define (cleanup/top-level/4 program)
  (cleanup/top-level program))

(define (cleanup/top-level/5 program)
  (cleanup/top-level program))

(define (cleanup/top-level/6 program)
  (cleanup/top-level program))

(define (cleanup/top-level/7 program)
  (cleanup/top-level program))

;;;; Debugging aids

;;; Errors and warnings

;; These should have their own condition types so that specific handlers
;; can be established.

(define (configuration-error complaint . reasons)
  (apply error complaint *current-phase* reasons))

(define (internal-error complaint . reasons)
  (apply error complaint *current-phase* reasons))

(define (user-error complaint . reasons)
  (apply error complaint *current-phase* reasons))

(define (internal-warning complaint . reasons)
  (apply warn complaint *current-phase* reasons))

(define (user-warning complaint . reasons)
  (apply warn complaint reasons))

(define (illegal form)
  (if (and (pair? form)
	   (memq (car form)
		 '(QUOTE   LOOKUP  LAMBDA  LET     DECLARE
		   CALL    BEGIN   IF      LETREC  SET!
		   UNASSIGNED? OR  DELAY   ACCESS  DEFINE
		   IN-PACKAGE THE-ENVIRONMENT)))
      (no-longer-legal form)
      (internal-error "Illegal KMP form" form)))

(define (no-longer-legal form)
  (internal-error "Unexpected KMP form -- should have been expanded"
		  form))

(define (not-yet-legal form)
  (internal-error "Unexpected KMP form -- should not occur yet"
		  form))

(define (free-var-error name)
  (internal-error "Free variable found" name))

(define (unimplemented name)
  (internal-error "Unimplemented procedure" name))

(define (form->source-irritant form)
  ;; Turn FORM into something to put in an error message or warning that
  ;; can help the user figure out where the error is.  Currently
  ;; pretty-prints the DBG expression for FORM if it can be found, and
  ;; prefixes each line with "; ", then wraps the whole text in an
  ;; error irritant.
  ;;
  ;; If nothing helpful can be found returns #F.  This happens only if
  ;; there is a problem in tracking dbg info.
  (define (string-split string separator)
    (let ((end (string-length string)))
      (let loop ((i 0))
	(cond ((substring-find-next-char string i end separator)
	       => (lambda (i*)
		    (cons (substring string i i*) (loop (+ i* 1)))))
	      ((= i end) '())
	      (else (list (substring string i end)))))))
  (define (format-scode scode)
    (fluid-let ((*unparser-list-depth-limit* 3))
      (let ((text  (with-output-to-string (lambda () (pp scode)))))
	(let ((fragments (string-split text #\newline)))
	  (define (prefix s) (if (string-null? s) s (string-append "\n;  " s)))
	  (apply string-append (map prefix fragments))))))
  (define (wrap e) (error-irritant/noise e))
  (define (get-source dbg-object)
    (cond ((new-dbg-expression? dbg-object)
	   (wrap (format-scode (new-dbg-expression/source-code dbg-object))))
	  ((new-dbg-procedure? dbg-object)
	   (wrap (format-scode (new-dbg-procedure/source-code dbg-object))))
	  ((new-dbg-continuation/inner dbg-object)
	   => get-source)
	  ((new-dbg-continuation/outer dbg-object)
	   => get-source)
	  (else (unhelpful))))
  (define (unhelpful)
    (wrap
     (string-append
      (format-scode (string->symbol "<original source not available>"))
      (format-scode (kmp/->ppp form)))))
  (cond ((code-rewrite/original-form form) => get-source)
	((code-rewrite/original-form/previous form) => get-source)
	(else (unhelpful))))

(define (compiler:debug #!optional what)
  "
 (compiler:debug #F)
 (compiler:debug)
 (compiler:debug '(phase-names...))"

  (set! compiler:guru? #T)
  (set! compiler:generate-kmp-files? #T)
  (set! compiler:generate-rtl-files? #T)
  (set! compiler:generate-lap-files? #T)

  (cond ((default-object? what))
        ((equal? what '#F)
	 (set! compiler:guru? #F)
	 (set! compiler:generate-kmp-files? #F)
	 (set! compiler:generate-rtl-files? #F)
	 (set! compiler:generate-lap-files? #F))
        (else
	 (set! compiler:generate-kmp-files? what))))

