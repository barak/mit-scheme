#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/conpar.scm,v 14.4 1988/06/22 21:24:16 cph Exp $

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

;;;; Continuation Parser
;;; package: (runtime continuation-parser)

(declare (usual-integrations))

;;;; Stack Frames

(define-structure (stack-frame
		   (constructor make-stack-frame
				(type elements dynamic-state fluid-bindings
				      interrupt-mask history
				      previous-history-offset
				      previous-history-control-point %next))
		   (conc-name stack-frame/))
  (type false read-only true)
  (elements false read-only true)
  (dynamic-state false read-only true)
  (fluid-bindings false read-only true)
  (interrupt-mask false read-only true)
  (history false read-only true)
  (previous-history-offset false read-only true)
  (previous-history-control-point false read-only true)
  ;; %NEXT is either a parser-state object or the next frame.  In the
  ;; former case, the parser-state is used to compute the next frame.
  %next
  (properties (make-1d-table) read-only true))

(define (stack-frame/reductions stack-frame)
  (let ((history (stack-frame/history stack-frame)))
    (if (eq? history undefined-history)
	'()
	(history-reductions history))))

(define undefined-history
  "no history")

(define (stack-frame/next stack-frame)
  (let ((next (stack-frame/%next stack-frame)))
    (if (parser-state? next)
	(let ((next (parse/start next)))
	  (set-stack-frame/%next! stack-frame next)
	  next)
	next)))

(define-integrable (continuation/first-subproblem continuation)
  (stack-frame/skip-non-subproblems (continuation->stack-frame continuation)))

(define (stack-frame/next-subproblem stack-frame)
  (if (stack-frame/subproblem? stack-frame)
      (let ((stack-frame (stack-frame/next stack-frame)))
	(and stack-frame
	     (stack-frame/skip-non-subproblems stack-frame)))
      (stack-frame/skip-non-subproblems stack-frame)))

(define (stack-frame/skip-non-subproblems stack-frame)
  (if (stack-frame/subproblem? stack-frame)
      stack-frame
      (let ((stack-frame (stack-frame/next stack-frame)))
	(and stack-frame
	     (stack-frame/skip-non-subproblems stack-frame)))))

(define-integrable (stack-frame/length stack-frame)
  (vector-length (stack-frame/elements stack-frame)))

(define (stack-frame/ref stack-frame index)
  (map-reference-trap
   (let ((elements (stack-frame/elements stack-frame)))
     (lambda ()
       (vector-ref elements index)))))
(define-integrable (stack-frame/return-address stack-frame)
  (stack-frame-type/address (stack-frame/type stack-frame)))

(define-integrable (stack-frame/return-code stack-frame)
  (stack-frame-type/code (stack-frame/type stack-frame)))

(define-integrable (stack-frame/subproblem? stack-frame)
  (stack-frame-type/subproblem? (stack-frame/type stack-frame)))

;;;; Parser

(define-structure (parser-state (constructor make-parser-state)
				(conc-name parser-state/))
  (dynamic-state false read-only true)
  (fluid-bindings false read-only true)
  (interrupt-mask false read-only true)
  (history false read-only true)
  (previous-history-offset false read-only true)
  (previous-history-control-point false read-only true)
  (element-stream false read-only true)
  (next-control-point false read-only true))

(define (continuation->stack-frame continuation)
  (parse/control-point (continuation/control-point continuation)
		       (continuation/dynamic-state continuation)
		       (continuation/fluid-bindings continuation)))

(define (parse/control-point control-point dynamic-state fluid-bindings)
  (and control-point
       (parse/start
	(make-parser-state
	 dynamic-state
	 fluid-bindings
	 (control-point/interrupt-mask control-point)
	 (history-transform (control-point/history control-point))
	 (control-point/previous-history-offset control-point)
	 (control-point/previous-history-control-point control-point)
	 (control-point/element-stream control-point)
	 (control-point/next-control-point control-point)))))

(define (parse/start state)
  (let ((stream (parser-state/element-stream state)))
    (if (stream-pair? stream)
	(let ((type (parse/type stream))
	      (stream (stream-cdr stream)))
	  (let ((length (parse/length stream type)))
	    (with-values (lambda () (parse/elements stream length))
	      (lambda (elements stream)
		(parse/dispatch type
				elements
				(parse/next-state state length stream))))))
	(parse/control-point (parser-state/next-control-point state)
			     (parser-state/dynamic-state state)
			     (parser-state/fluid-bindings state)))))

(define (parse/type stream)
  (let ((return-address (element-stream/head stream)))
    (if (not (return-address? return-address))
	(error "illegal return address" return-address))
    (let ((code (return-address/code return-address)))
      (let ((type (microcode-return/code->type code)))
	(if (not type)
	    (error "return-code has no type" code))
	type))))

(define (parse/length stream type)
  (let ((length (stack-frame-type/length type)))
    (if (integer? length)
	length
	(length stream))))

(define (parse/elements stream length)
  (let ((elements (make-vector length)))
    (let loop ((stream stream) (index 0))
      (if (< index length)
	  (begin (if (not (stream-pair? stream))
		     (error "stack too short" index))
		 (vector-set! elements index (stream-car stream))
		 (loop (stream-cdr stream) (1+ index)))
	  (values elements stream)))))

(define (parse/dispatch type elements state)
  ((stack-frame-type/parser type) type elements state))

(define (parse/next-state state length stream)
  (let ((previous-history-control-point
	 (parser-state/previous-history-control-point state)))
    (make-parser-state
     (parser-state/dynamic-state state)
     (parser-state/fluid-bindings state)
     (parser-state/interrupt-mask state)
     (parser-state/history state)
     (if previous-history-control-point
	 (parser-state/previous-history-offset state)
	 (max (- (parser-state/previous-history-offset state) length) 0))
     previous-history-control-point
     stream
     (parser-state/next-control-point state))))

(define (make-frame type elements state element-stream)
  (let ((subproblem? (stack-frame-type/subproblem? type))
	(history (parser-state/history state))
	(previous-history-offset (parser-state/previous-history-offset state))
	(previous-history-control-point
	 (parser-state/previous-history-control-point state)))
    (make-stack-frame type
		      elements
		      (parser-state/dynamic-state state)
		      (parser-state/fluid-bindings state)
		      (parser-state/interrupt-mask state)
		      (if subproblem? history undefined-history)
		      previous-history-offset
		      previous-history-control-point
		      (make-parser-state
		       (parser-state/dynamic-state state)
		       (parser-state/fluid-bindings state)
		       (parser-state/interrupt-mask state)
		       (if subproblem? (history-superproblem history) history)
		       previous-history-offset
		       previous-history-control-point
		       element-stream
		       (parser-state/next-control-point state)))))

(define (element-stream/head stream)
  (if (not (stream-pair? stream)) (error "not a stream-pair" stream))
  (map-reference-trap (lambda () (stream-car stream))))

(define (element-stream/ref stream index)
  (if (not (stream-pair? stream)) (error "not a stream-pair" stream))
  (if (zero? index)
      (map-reference-trap (lambda () (stream-car stream)))
      (element-stream/ref (stream-cdr stream)  (-1+ index))))

;;;; Unparser

(define (stack-frame->continuation stack-frame)
  (make-continuation 'REENTRANT
		     (stack-frame->control-point stack-frame)
		     (stack-frame/dynamic-state stack-frame)
		     (stack-frame/fluid-bindings stack-frame)))

(define (stack-frame->control-point stack-frame)
  (with-values (lambda () (unparse/stack-frame stack-frame))
    (lambda (element-stream next-control-point)
      (make-control-point
       false
       0
       (stack-frame/interrupt-mask stack-frame)
       (history-untransform (stack-frame/history stack-frame))
       (stack-frame/previous-history-offset stack-frame)
       (stack-frame/previous-history-control-point stack-frame)
       element-stream
       next-control-point))))

(define (unparse/stack-frame stack-frame)
  (let ((next (stack-frame/%next stack-frame)))
    (cond ((stack-frame? next)
	   (with-values (lambda () (unparse/stack-frame next))
	     (lambda (element-stream next-control-point)
	       (values (let ((type (stack-frame/type stack-frame)))
			 ((stack-frame-type/unparser type)
			  type
			  (stack-frame/elements stack-frame)
			  element-stream))
		       next-control-point))))
	  ((parser-state? next)
	   (values (parser-state/element-stream next)
		   (parser-state/next-control-point next)))
	  (else (values (stream) false)))))

;;;; Generic Parsers/Unparsers

(define (parser/interpreter-next type elements state)
  (make-frame type elements state (parser-state/element-stream state)))

(define (unparser/interpreter-next type elements element-stream)
  (cons-stream (make-return-address (stack-frame-type/code type))
	       (let ((length (vector-length elements)))
		 (let loop ((index 0))
		   (if (< index length)
		       (cons-stream (vector-ref elements index)
				    (loop (1+ index)))
		       element-stream)))))

(define (parser/compiler-next type elements state)
  (make-frame type elements state
	      (cons-stream
	       (ucode-return-address reenter-compiled-code)
	       (cons-stream
		(- (vector-ref elements 0) (vector-length elements))
		(parser-state/element-stream state)))))

(define (unparser/compiler-next type elements element-stream)
  (unparser/interpreter-next type elements (stream-tail element-stream 2)))

(define (make-restore-frame type
			    elements
			    state
			    dynamic-state
			    fluid-bindings
			    interrupt-mask
			    history
			    previous-history-offset
			    previous-history-control-point)
  (parser/interpreter-next
   type
   elements
   (make-parser-state dynamic-state
		      fluid-bindings
		      interrupt-mask
		      history
		      previous-history-offset
		      previous-history-control-point
		      (parser-state/element-stream state)
		      (parser-state/next-control-point state))))

;;;; Specific Parsers

(define (parser/restore-dynamic-state type elements state)
  (make-restore-frame type elements state
		      ;; Possible problem: the dynamic state really
		      ;; consists of all of the state spaces in
		      ;; existence.  Probably we should have some
		      ;; mechanism for keeping track of them all.
		      (let ((dynamic-state (vector-ref elements 0)))
			(if (eq? system-state-space
				 (state-point/space dynamic-state))
			    dynamic-state
			    (parser-state/dynamic-state state)))
		      (parser-state/fluid-bindings state)
		      (parser-state/interrupt-mask state)
		      (parser-state/history state)
		      (parser-state/previous-history-offset state)
		      (parser-state/previous-history-control-point state)))

(define (parser/restore-fluid-bindings type elements state)
  (make-restore-frame type elements state
		      (parser-state/dynamic-state state)
		      (vector-ref elements 0)
		      (parser-state/interrupt-mask state)
		      (parser-state/history state)
		      (parser-state/previous-history-offset state)
		      (parser-state/previous-history-control-point state)))

(define (parser/restore-interrupt-mask type elements state)
  (make-restore-frame type elements state
		      (parser-state/dynamic-state state)
		      (parser-state/fluid-bindings state)
		      (vector-ref elements 0)
		      (parser-state/history state)
		      (parser-state/previous-history-offset state)
		      (parser-state/previous-history-control-point state)))

(define (parser/restore-history type elements state)
  (make-restore-frame type elements state
		      (parser-state/dynamic-state state)
		      (parser-state/fluid-bindings state)
		      (parser-state/interrupt-mask state)
		      (history-transform (vector-ref elements 0))
		      (vector-ref elements 1)
		      (vector-ref elements 2)))

(define (length/combination-save-value stream)
  (+ 2 (system-vector-length (element-stream/head stream))))

(define ((length/application-frame index missing) stream)
  (+ index 1 (- (object-datum (element-stream/ref stream index)) missing)))

(define (length/repeat-primitive stream)
  (-1+ (primitive-procedure-arity (element-stream/head stream))))

(define (length/reenter-compiled-code stream)
  (1+ (element-stream/head stream)))

;;;; Stack Frame Types

(define-structure (stack-frame-type
		   (constructor make-stack-frame-type
				(code subproblem? length parser unparser))
		   (conc-name stack-frame-type/))
  (code false read-only true)
  (subproblem? false read-only true)
  (properties (make-1d-table) read-only true)
  (length false read-only true)
  (parser false read-only true)
  (unparser false read-only true))

(define (microcode-return/code->type code)
  (if (not (< code (vector-length stack-frame-types)))
      (error "return-code too large" code))
  (vector-ref stack-frame-types code))

(define-integrable (stack-frame-type/address frame-type)
  (make-return-address (stack-frame-type/code frame-type)))

(define (initialize-package!)
  (set! stack-frame-types (make-stack-frame-types)))

(define stack-frame-types)

(define (make-stack-frame-types)
  (let ((types (make-vector (microcode-return/code-limit) false)))

    (define (stack-frame-type name subproblem? length parser unparser)
      (let ((code (microcode-return name)))
	(vector-set! types
		     code
		     (make-stack-frame-type code subproblem? length parser
					    unparser))))

    (define (interpreter-frame name length #!optional parser)
      (stack-frame-type name false length
			(if (default-object? parser)
			    parser/interpreter-next
			    parser)
			unparser/interpreter-next))

    (define (compiler-frame name length #!optional parser)
      (stack-frame-type name false length
			(if (default-object? parser)
			    parser/compiler-next
			    parser)
			unparser/compiler-next))

    (define (interpreter-subproblem name length)
      (stack-frame-type name true length parser/interpreter-next
			unparser/interpreter-next))

    (define (compiler-subproblem name length)
      (stack-frame-type name true length parser/compiler-next
			unparser/compiler-next))

    (interpreter-frame 'RESTORE-TO-STATE-POINT 1 parser/restore-dynamic-state)
    (interpreter-frame 'RESTORE-FLUIDS 1 parser/restore-fluid-bindings)
    (interpreter-frame 'RESTORE-INTERRUPT-MASK 1 parser/restore-interrupt-mask)
    (interpreter-frame 'RESTORE-HISTORY 3 parser/restore-history)
    (interpreter-frame 'RESTORE-DONT-COPY-HISTORY 3 parser/restore-history)

    (interpreter-frame 'NON-EXISTENT-CONTINUATION 1)
    (interpreter-frame 'HALT 1)
    (interpreter-frame 'JOIN-STACKLETS 1)
    (interpreter-frame 'POP-RETURN-ERROR 1)

    (interpreter-subproblem 'IN-PACKAGE-CONTINUE 1)
    (interpreter-subproblem 'ACCESS-CONTINUE 1)
    (interpreter-subproblem 'PRIMITIVE-COMBINATION-1-APPLY 1)
    (interpreter-subproblem 'FORCE-SNAP-THUNK 1)
    (interpreter-subproblem 'GC-CHECK 1)
    (interpreter-subproblem 'RESTORE-VALUE 1)
    (interpreter-subproblem 'ASSIGNMENT-CONTINUE 2)
    (interpreter-subproblem 'DEFINITION-CONTINUE 2)
    (interpreter-subproblem 'SEQUENCE-2-SECOND 2)
    (interpreter-subproblem 'SEQUENCE-3-SECOND 2)
    (interpreter-subproblem 'SEQUENCE-3-THIRD 2)
    (interpreter-subproblem 'CONDITIONAL-DECIDE 2)
    (interpreter-subproblem 'DISJUNCTION-DECIDE 2)
    (interpreter-subproblem 'COMBINATION-1-PROCEDURE 2)
    (interpreter-subproblem 'COMBINATION-2-FIRST-OPERAND 2)
    (interpreter-subproblem 'EVAL-ERROR 2)
    (interpreter-subproblem 'PRIMITIVE-COMBINATION-2-FIRST-OPERAND 2)
    (interpreter-subproblem 'PRIMITIVE-COMBINATION-2-APPLY 2)
    (interpreter-subproblem 'PRIMITIVE-COMBINATION-3-SECOND-OPERAND 2)
    (interpreter-subproblem 'COMBINATION-2-PROCEDURE 3)
    (interpreter-subproblem 'REPEAT-DISPATCH 3)
    (interpreter-subproblem 'PRIMITIVE-COMBINATION-3-FIRST-OPERAND 3)
    (interpreter-subproblem 'PRIMITIVE-COMBINATION-3-APPLY 3)
    (interpreter-subproblem 'MOVE-TO-ADJACENT-POINT 5)

    (interpreter-subproblem 'COMBINATION-SAVE-VALUE
			    length/combination-save-value)

    (interpreter-subproblem 'REPEAT-PRIMITIVE length/repeat-primitive)

    (let ((length (length/application-frame 1 0)))
      (interpreter-subproblem 'COMBINATION-APPLY length)
      (interpreter-subproblem 'INTERNAL-APPLY length))

    (interpreter-subproblem 'REENTER-COMPILED-CODE
			    length/reenter-compiled-code)

    (compiler-frame 'COMPILER-INTERRUPT-RESTART 2)
    (compiler-frame 'COMPILER-LINK-CACHES-RESTART 7)

    (compiler-subproblem 'COMPILER-REFERENCE-RESTART 3)
    (compiler-subproblem 'COMPILER-SAFE-REFERENCE-RESTART 3)
    (compiler-subproblem 'COMPILER-ACCESS-RESTART 3)
    (compiler-subproblem 'COMPILER-UNASSIGNED?-RESTART 3)
    (compiler-subproblem 'COMPILER-UNBOUND?-RESTART 3)
    (compiler-subproblem 'COMPILER-REFERENCE-TRAP-RESTART 3)
    (compiler-subproblem 'COMPILER-SAFE-REFERENCE-TRAP-RESTART 3)
    (compiler-subproblem 'COMPILER-UNASSIGNED?-TRAP-RESTART 3)
    (compiler-subproblem 'COMPILER-ASSIGNMENT-RESTART 4)
    (compiler-subproblem 'COMPILER-DEFINITION-RESTART 4)
    (compiler-subproblem 'COMPILER-ASSIGNMENT-TRAP-RESTART 4)

    (compiler-subproblem 'COMPILER-LOOKUP-APPLY-RESTART
			 (length/application-frame 3 1))

    (let ((length (length/application-frame 3 0)))
      (compiler-subproblem 'COMPILER-LOOKUP-APPLY-TRAP-RESTART length)
      (compiler-subproblem 'COMPILER-OPERATOR-LOOKUP-TRAP-RESTART length))

    types))