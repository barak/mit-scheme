#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/conpar.scm,v 14.11 1989/12/07 05:35:31 cph Exp $

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

;;;; Continuation Parser
;;; package: (runtime continuation-parser)

(declare (usual-integrations))

;;;; Stack Frames

(define-structure (stack-frame
		   (constructor make-stack-frame
				(type elements dynamic-state fluid-bindings
				      interrupt-mask history
				      previous-history-offset
				      previous-history-control-point
				      offset %next))
		   (conc-name stack-frame/))
  (type false read-only true)
  (elements false read-only true)
  (dynamic-state false read-only true)
  (fluid-bindings false read-only true)
  (interrupt-mask false read-only true)
  (history false read-only true)
  (previous-history-offset false read-only true)
  (previous-history-control-point false read-only true)
  (offset false read-only true)
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
  (let ((elements (stack-frame/elements stack-frame)))
    (let ((length (vector-length elements)))
      (if (< index length)
	  (map-reference-trap (lambda () (vector-ref elements index)))
	  (stack-frame/ref (stack-frame/next stack-frame) (- index length))))))

(define-integrable (stack-frame/return-address stack-frame)
  (stack-frame/ref stack-frame 0))

(define (stack-frame/return-code stack-frame)
  (let ((return-address (stack-frame/return-address stack-frame)))
    (and (interpreter-return-address? return-address)
	 (return-address/code return-address))))

(define-integrable (stack-frame/subproblem? stack-frame)
  (stack-frame-type/subproblem? (stack-frame/type stack-frame)))

(define-integrable (stack-frame/compiled-code? stack-frame)
  (compiled-return-address? (stack-frame/return-address stack-frame)))

(define (stack-frame/resolve-stack-address frame address)
  (let loop
      ((frame frame)
       (offset (stack-address->index address (stack-frame/offset frame))))
    (let ((length (stack-frame/length frame)))
      (if (< offset length)
	  (values frame offset)
	  (loop (stack-frame/next frame) (- offset length))))))

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
  (n-elements false read-only true)
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
	 (control-point/n-elements control-point)
	 (control-point/next-control-point control-point)))))

(define (parse/start state)
  (let ((stream (parser-state/element-stream state)))
    (if (stream-pair? stream)
	(let ((type
	       (return-address->stack-frame-type
		(element-stream/head stream))))
	  (let ((length
		 (let ((length (stack-frame-type/length type)))
		   (if (exact-nonnegative-integer? length)
		       length
		       (length stream (parser-state/n-elements state))))))
	    ((stack-frame-type/parser type)
	     type
	     (list->vector (stream-head stream length))
	     (parse/next-state state length (stream-tail stream length)))))
	(parse/control-point (parser-state/next-control-point state)
			     (parser-state/dynamic-state state)
			     (parser-state/fluid-bindings state)))))

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
	 (max (- (parser-state/previous-history-offset state) (-1+ length))
	      0))
     previous-history-control-point
     stream
     (- (parser-state/n-elements state) length)
     (parser-state/next-control-point state))))

(define (make-frame type elements state element-stream n-elements)
  (let ((history-subproblem?
	 (stack-frame-type/history-subproblem? type))
	(history (parser-state/history state))
	(previous-history-offset (parser-state/previous-history-offset state))
	(previous-history-control-point
	 (parser-state/previous-history-control-point state)))
    (make-stack-frame type
		      elements
		      (parser-state/dynamic-state state)
		      (parser-state/fluid-bindings state)
		      (parser-state/interrupt-mask state)
		      (if history-subproblem? history undefined-history)
		      previous-history-offset
		      previous-history-control-point
		      (+ (vector-length elements) n-elements)
		      (make-parser-state
		       (parser-state/dynamic-state state)
		       (parser-state/fluid-bindings state)
		       (parser-state/interrupt-mask state)
		       (if history-subproblem?
			   (history-superproblem history)
			   history)
		       previous-history-offset
		       previous-history-control-point
		       element-stream
		       n-elements
		       (parser-state/next-control-point state)))))

(define (element-stream/head stream)
  (if (not (stream-pair? stream)) (error "not a stream-pair" stream))
  (map-reference-trap (lambda () (stream-car stream))))

(define-integrable (element-stream/ref stream index)
  (map-reference-trap (lambda () (stream-ref stream index))))

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
       (let ((history (stack-frame/history stack-frame)))
	 (if (eq? history undefined-history)
	     (fixed-objects-item 'DUMMY-HISTORY)
	     (history-untransform history)))
       (stack-frame/previous-history-offset stack-frame)
       (stack-frame/previous-history-control-point stack-frame)
       (if (stack-frame/compiled-code? stack-frame)
	   (cons-stream return-address/reenter-compiled-code
			(cons-stream false element-stream))
	   element-stream)
       next-control-point))))

(define (unparse/stack-frame stack-frame)
  (if (eq? (stack-frame/return-address stack-frame)
	   return-address/join-stacklets)
      (values (stream) (vector-ref (stack-frame/elements stack-frame) 1))
      (with-values
	  (lambda ()
	    (let ((next (stack-frame/%next stack-frame)))
	      (cond ((stack-frame? next)
		     (unparse/stack-frame next))
		    ((parser-state? next)
		     (values (parser-state/element-stream next)
			     (parser-state/next-control-point next)))
		    (else
		     (values (stream) false)))))
	(lambda (element-stream next-control-point)
	  (values
	   (let ((elements (stack-frame/elements stack-frame)))
	     (let ((length (vector-length elements)))
	       (let loop ((index 0))
		 (if (< index length)
		     (cons-stream (vector-ref elements index)
				  (loop (1+ index)))
		     element-stream))))
	   next-control-point)))))

(define return-address/join-stacklets)
(define return-address/reenter-compiled-code)

;;;; Special Frame Lengths

(define (length/combination-save-value stream offset)
  offset
  (+ 3 (system-vector-length (element-stream/ref stream 1))))

(define ((length/application-frame index missing) stream offset)
  offset
  (+ index 1 (- (object-datum (element-stream/ref stream index)) missing)))

(define (length/repeat-primitive stream offset)
  offset
  (primitive-procedure-arity (element-stream/ref stream 1)))

(define (length/compiled-return-address stream offset)
  (let ((entry (element-stream/head stream)))
    (let ((frame-size (compiled-continuation/next-continuation-offset entry)))
      (if frame-size
	  (1+ frame-size)
	  (stack-address->index (element-stream/ref stream 1) offset)))))

(define (verify paranoia-index stream offset)
  (or (zero? paranoia-index)
      (stream-null? stream)
      (let* ((type (return-address->stack-frame-type
		    (element-stream/head stream)))
	     (length
	      (let ((length (stack-frame-type/length type)))
		(if (exact-nonnegative-integer? length)
		    length
		    (length stream offset))))
	     (ltail (stream-tail* stream length)))
	(and ltail
	     (return-address? (element-stream/head ltail))
	     (verify (-1+ paranoia-index)
		     ltail
		     (+ offset length))))))

(define (stream-tail* stream n)
  (cond ((or (zero? n) (stream-null? stream))
	 stream)
	((stream-pair? stream)
	 (stream-tail* (stream-cdr stream) (-1+ n)))
	(else
	 (error "stream-tail*: not a proper stream" stream))))	   

;;;; Parsers

(define (parser/standard-next type elements state)
  (make-frame type
	      elements
	      state
	      (parser-state/element-stream state)
	      (parser-state/n-elements state)))

(define (make-restore-frame type
			    elements
			    state
			    dynamic-state
			    fluid-bindings
			    interrupt-mask
			    history
			    previous-history-offset
			    previous-history-control-point)
  (parser/standard-next
   type
   elements
   (make-parser-state dynamic-state
		      fluid-bindings
		      interrupt-mask
		      history
		      previous-history-offset
		      previous-history-control-point
		      (parser-state/element-stream state)
		      (parser-state/n-elements state)
		      (parser-state/next-control-point state))))

(define (parser/restore-dynamic-state type elements state)
  (make-restore-frame type elements state
		      ;; Possible problem: the dynamic state really
		      ;; consists of all of the state spaces in
		      ;; existence.  Probably we should have some
		      ;; mechanism for keeping track of them all.
		      (let ((dynamic-state (vector-ref elements 1)))
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
		      (vector-ref elements 1)
		      (parser-state/interrupt-mask state)
		      (parser-state/history state)
		      (parser-state/previous-history-offset state)
		      (parser-state/previous-history-control-point state)))

(define (parser/restore-interrupt-mask type elements state)
  (make-restore-frame type elements state
		      (parser-state/dynamic-state state)
		      (parser-state/fluid-bindings state)
		      (vector-ref elements 1)
		      (parser-state/history state)
		      (parser-state/previous-history-offset state)
		      (parser-state/previous-history-control-point state)))

(define (parser/restore-history type elements state)
  (make-restore-frame type elements state
		      (parser-state/dynamic-state state)
		      (parser-state/fluid-bindings state)
		      (parser-state/interrupt-mask state)
		      (history-transform (vector-ref elements 1))
		      (vector-ref elements 2)
		      (vector-ref elements 3)))

;;;; Stack Frame Types

(define-structure (stack-frame-type
		   (constructor make-stack-frame-type
				(code subproblem?
				      history-subproblem?
				      length parser))
		   (conc-name stack-frame-type/))
  (code false read-only true)
  (subproblem? false read-only true)
  (history-subproblem? false read-only true)
  (properties (make-1d-table) read-only true)
  (length false read-only true)
  (parser false read-only true))

(define (microcode-return/code->type code)
  (if (not (< code (vector-length stack-frame-types)))
      (error "return-code too large" code))
  (vector-ref stack-frame-types code))

(define (return-address->stack-frame-type return-address)
  (cond ((interpreter-return-address? return-address)
	 (let ((code (return-address/code return-address)))
	   (let ((type (microcode-return/code->type code)))
	     (if (not type)
		 (error "return-code has no type" code))
	     type)))
	((compiled-return-address? return-address)
	 (if (compiled-continuation/return-to-interpreter?
	      return-address)
	     stack-frame-type/return-to-interpreter
	     stack-frame-type/compiled-return-address))
	(else
	 (error "illegal return address" return-address))))

(define (initialize-package!)
  (set! return-address/join-stacklets
	(make-return-address (microcode-return 'JOIN-STACKLETS)))
  (set! return-address/reenter-compiled-code
	(make-return-address (microcode-return 'REENTER-COMPILED-CODE)))
  (set! stack-frame-types (make-stack-frame-types))
  (set! stack-frame-type/hardware-trap
	(vector-ref stack-frame-types (microcode-return 'HARDWARE-TRAP)))
  (set! stack-frame-type/compiled-return-address
	(make-stack-frame-type false
			       true
			       false
			       length/compiled-return-address
			       parser/standard-next))
  (set! stack-frame-type/return-to-interpreter
	(make-stack-frame-type false
			       false
			       false
			       1
			       parser/standard-next))
  (set! word-size
	(let ((initial (system-vector-length (make-bit-string 1 #f))))
	  (let loop ((size 2))
	    (if (= (system-vector-length (make-bit-string size #f))
		   initial)
		(loop (1+ size))
		(-1+ size)))))
  unspecific)

(define stack-frame-types)
(define stack-frame-type/compiled-return-address)
(define stack-frame-type/return-to-interpreter)
(define stack-frame-type/hardware-trap)

(define (make-stack-frame-types)
  (let ((types (make-vector (microcode-return/code-limit) false)))

    (define (stack-frame-type name subproblem?
			      history-subproblem?
			      length parser)
      (let ((code (microcode-return name)))
	(vector-set! types
		     code
		     (make-stack-frame-type code subproblem?
					    history-subproblem?
					    length parser))))

    (define (standard-frame name length #!optional parser)
      (stack-frame-type name
			false
			false
			length
			(if (default-object? parser)
			    parser/standard-next
			    parser)))

    (define (standard-subproblem name length)
      (stack-frame-type name
			true
			true
			length
			parser/standard-next))

    (standard-frame 'RESTORE-TO-STATE-POINT 2 parser/restore-dynamic-state)
    (standard-frame 'RESTORE-FLUIDS 2 parser/restore-fluid-bindings)
    (standard-frame 'RESTORE-INTERRUPT-MASK 2 parser/restore-interrupt-mask)
    (standard-frame 'RESTORE-HISTORY 4 parser/restore-history)
    (standard-frame 'RESTORE-DONT-COPY-HISTORY 4 parser/restore-history)

    (standard-frame 'NON-EXISTENT-CONTINUATION 2)
    (standard-frame 'HALT 2)
    (standard-frame 'JOIN-STACKLETS 2)
    (standard-frame 'POP-RETURN-ERROR 2)
    (standard-frame 'REENTER-COMPILED-CODE 2)
    (standard-frame 'COMPILER-INTERRUPT-RESTART 3)
    (standard-frame 'COMPILER-LINK-CACHES-RESTART 8)

    (standard-subproblem 'IN-PACKAGE-CONTINUE 2)
    (standard-subproblem 'ACCESS-CONTINUE 2)
    (standard-subproblem 'PRIMITIVE-COMBINATION-1-APPLY 2)
    (standard-subproblem 'FORCE-SNAP-THUNK 2)
    (standard-subproblem 'GC-CHECK 2)
    (standard-subproblem 'RESTORE-VALUE 2)
    (standard-subproblem 'ASSIGNMENT-CONTINUE 3)
    (standard-subproblem 'DEFINITION-CONTINUE 3)
    (standard-subproblem 'SEQUENCE-2-SECOND 3)
    (standard-subproblem 'SEQUENCE-3-SECOND 3)
    (standard-subproblem 'SEQUENCE-3-THIRD 3)
    (standard-subproblem 'CONDITIONAL-DECIDE 3)
    (standard-subproblem 'DISJUNCTION-DECIDE 3)
    (standard-subproblem 'COMBINATION-1-PROCEDURE 3)
    (standard-subproblem 'COMBINATION-2-FIRST-OPERAND 3)
    (standard-subproblem 'EVAL-ERROR 3)
    (standard-subproblem 'PRIMITIVE-COMBINATION-2-FIRST-OPERAND 3)
    (standard-subproblem 'PRIMITIVE-COMBINATION-2-APPLY 3)
    (standard-subproblem 'PRIMITIVE-COMBINATION-3-SECOND-OPERAND 3)
    (standard-subproblem 'COMBINATION-2-PROCEDURE 4)
    (standard-subproblem 'REPEAT-DISPATCH 4)
    (standard-subproblem 'PRIMITIVE-COMBINATION-3-FIRST-OPERAND 4)
    (standard-subproblem 'PRIMITIVE-COMBINATION-3-APPLY 4)
    (standard-subproblem 'COMPILER-REFERENCE-RESTART 4)
    (standard-subproblem 'COMPILER-SAFE-REFERENCE-RESTART 4)
    (standard-subproblem 'COMPILER-ACCESS-RESTART 4)
    (standard-subproblem 'COMPILER-UNASSIGNED?-RESTART 4)
    (standard-subproblem 'COMPILER-UNBOUND?-RESTART 4)
    (standard-subproblem 'COMPILER-REFERENCE-TRAP-RESTART 4)
    (standard-subproblem 'COMPILER-SAFE-REFERENCE-TRAP-RESTART 4)
    (standard-subproblem 'COMPILER-UNASSIGNED?-TRAP-RESTART 4)
    (standard-subproblem 'COMPILER-ASSIGNMENT-RESTART 5)
    (standard-subproblem 'COMPILER-DEFINITION-RESTART 5)
    (standard-subproblem 'COMPILER-ASSIGNMENT-TRAP-RESTART 5)
    (standard-subproblem 'MOVE-TO-ADJACENT-POINT 6)
    (standard-subproblem 'COMBINATION-SAVE-VALUE length/combination-save-value)
    (standard-subproblem 'REPEAT-PRIMITIVE length/repeat-primitive)

    (let ((length (length/application-frame 2 0)))
      (standard-subproblem 'COMBINATION-APPLY length)
      (standard-subproblem 'INTERNAL-APPLY length))

    (standard-subproblem 'COMPILER-LOOKUP-APPLY-RESTART
			 (length/application-frame 4 1))

    (let ((length (length/application-frame 4 0)))
      (standard-subproblem 'COMPILER-LOOKUP-APPLY-TRAP-RESTART length)
      (standard-subproblem 'COMPILER-OPERATOR-LOOKUP-TRAP-RESTART length))
    (stack-frame-type 'HARDWARE-TRAP
		      true
		      false
		      length/hardware-trap
		      parser/standard-next)

    types))

;;;; Hardware trap parsing

(define-integrable hardware-trap/frame-size 8)

(define-integrable hardware-trap/signal-index 1)
(define-integrable hardware-trap/signal-name-index 2)
(define-integrable hardware-trap/code-index 3)
(define-integrable hardware-trap/stack-index 4)
(define-integrable hardware-trap/state-index 5)
(define-integrable hardware-trap/pc-info1-index 6)
(define-integrable hardware-trap/pc-info2-index 7)
(define-integrable hardware-trap/extra-info-index 8)

(define (length/hardware-trap stream offset)
  (let ((state (element-stream/ref stream hardware-trap/state-index))
	(stack-recovered?
	 (element-stream/ref stream hardware-trap/stack-index)))
    (if (not stack-recovered?)
	hardware-trap/frame-size
	(let ((after-header (stream-tail stream hardware-trap/frame-size)))
	  (case state
	    ((1)			;primitive
	     (let* ((primitive
		     (element-stream/ref stream hardware-trap/pc-info1-index))
		    (arity (primitive-procedure-arity primitive))
		    (nargs
		     (if (negative? arity)
			 (element-stream/ref stream hardware-trap/pc-info2-index)
			 arity)))
	       (if (return-address? (element-stream/ref after-header nargs))
		   (+ hardware-trap/frame-size nargs)
		   (- (heuristic (stream-tail after-header nargs)
				 (+ hardware-trap/frame-size nargs offset))
		      offset))))
	    ((0 2 3)			;unknown, cc, or probably cc
	     (- (heuristic after-header (+ hardware-trap/frame-size offset))
		offset))
	    (else
	     (error "length/hardware-trap: Unknown state" state)))))))

(define (heuristic stream offset)
  (if (or (stream-null? stream)
	  (and (return-address? (element-stream/head stream))
	       (verify 2 stream offset)))
      offset
      (heuristic (stream-cdr stream) (1+ offset))))

(define (guarantee-hardware-trap-frame frame)
  (if (or (not (stack-frame? frame))
	  (not (eq? (stack-frame/type frame)
		    stack-frame-type/hardware-trap)))
      (error "guarantee-hardware-trap-frame: invalid" frame)))

(define (hardware-trap-frame/print-registers frame)
  (guarantee-hardware-trap-frame frame)
  (let ((block (stack-frame/ref frame hardware-trap/extra-info-index)))
    (if block
	(let ((nregs (- (system-vector-length block) 2)))
	  (print-register block 0 "pc")
	  (print-register block 1 "sp")
	  (let loop ((i 0))
	    (if (< i nregs)
		(begin
		  (print-register block
				  (+ 2 i)
				  (string-append "register "
						 (number->string i)))
		  (loop (1+ i)))))))))

(define (print-register block index name)
  (let ((value
	 (let ((bit-string (bit-string-allocate word-size)))
	   (read-bits! block (* word-size (1+ index)) bit-string)
	   (bit-string->unsigned-integer bit-string))))
    (newline)
    (write-string "  ")
    (write-string name)
    (write-string " = ")
    (write-string (number->string value 16))))

(define word-size)

(define (hardware-trap-frame/print-stack frame)
  (guarantee-hardware-trap-frame frame)
  (let ((elements
	 (let ((elements (stack-frame/elements frame)))
	   (subvector->list elements
			    hardware-trap/frame-size
			    (vector-length elements)))))
    (if (null? elements)
	(begin
	  (newline)
	  (write-string ";; Empty stack"))
	(begin
	  (newline)
	  (write-string ";; Bottom of the stack")
	  (for-each (lambda (element)
		      (newline)
		      (write-string "  ")
		      (write element))
		    (reverse elements))
	  (newline)
	  (write-string ";; Top of the stack")))))

(define (hardware-trap-frame/describe frame long?)
  (guarantee-hardware-trap-frame frame)
  (let ((name (stack-frame/ref frame hardware-trap/signal-name-index))
	(state (stack-frame/ref frame hardware-trap/state-index)))
    (if name
	(begin
	  (write-string "Hardware trap ")
	  (write-string name))
	(write-string "User microcode reset"))
    (if long?
	(case state
	  ((0)				; unknown
	   (write-string " at an unknown location."))
	  ((1)				; primitive
	   (write-string " within ")
	   (write (stack-frame/ref frame hardware-trap/pc-info1-index)))
	  ((2)				; compiled code
	   (write-string " at offset ")
	   (write-string
	    (number->string (stack-frame/ref frame
					     hardware-trap/pc-info2-index)
			    16))
	   (newline)
	   (write-string "within ")
	   (let ((block (stack-frame/ref frame hardware-trap/pc-info1-index)))
	     (write block)
	     (let loop ((info (compiled-code-block/debugging-info block)))
	       (cond ((null? info)
		      false)
		     ((string? info)
		      (begin
			(write-string " (")
			(write-string info)
			(write-string ")")))
		     ((not (pair? info))
		      false)
		     ((string? (car info))
		      (loop (car info)))
		     (else
		      (loop (cdr info)))))))
	  ((3)
	   (write-string " at an unknown compiled code location."))
	  (else
	   (error "hardware-trap/describe: Unknown state" state))))))