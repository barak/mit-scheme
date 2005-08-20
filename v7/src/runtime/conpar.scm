#| -*-Scheme-*-

$Id: conpar.scm,v 14.49 2005/08/20 01:57:26 cph Exp $

Copyright 1988,1989,1990,1991,1992,1993 Massachusetts Institute of Technology
Copyright 1994,1999,2001,2003,2004,2005 Massachusetts Institute of Technology

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

;;;; Continuation Parser
;;; package: (runtime continuation-parser)

(declare (usual-integrations))

;;;; Stack Frames

(define-structure (stack-frame
		   (constructor make-stack-frame
				(type elements dynamic-state
				      block-thread-events?
				      interrupt-mask history
				      previous-history-offset
				      previous-history-control-point
				      offset previous-type %next))
		   (conc-name stack-frame/))
  (type #f read-only #t)
  (elements #f read-only #t)
  (dynamic-state #f read-only #t)
  (block-thread-events? #f read-only #t)
  (interrupt-mask #f read-only #t)
  (history #f read-only #t)
  (previous-history-offset #f read-only #t)
  (previous-history-control-point #f read-only #t)
  (offset #f read-only #t)
  ;; PREVIOUS-TYPE is the stack-frame-type of the frame above this one
  ;; on the stack (closer to the stack's top).  In at least two cases
  ;; we need to know this information.
  (previous-type #f read-only #t)
  ;; %NEXT is either a parser-state object or the next frame.  In the
  ;; former case, the parser-state is used to compute the next frame.
  %next
  (properties (make-1d-table) read-only #t))

(define (stack-frame/reductions stack-frame)
  (let ((history (stack-frame/history stack-frame)))
    (if (eq? history undefined-history)
	'()
	(history-reductions history))))

(define undefined-history
  (list 'UNDEFINED-HISTORY))

(define (stack-frame/next stack-frame)
  (let ((next (stack-frame/%next stack-frame)))
    (if (parser-state? next)
	(let ((next (parse-one-frame next)))
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

(define-integrable (stack-frame/length stack-frame)
  (vector-length (stack-frame/elements stack-frame)))

(define (stack-frame/ref stack-frame index)
  (let ((elements (stack-frame/elements stack-frame)))
    (let ((length (vector-length elements)))
      (if (fix:< index length)
	  (vector-ref elements index)
	  (stack-frame/ref (stack-frame/next stack-frame)
			   (fix:- index length))))))

(define-integrable (stack-frame/return-address stack-frame)
  (stack-frame/ref stack-frame 0))

(define (stack-frame/return-code stack-frame)
  (let ((return-address (stack-frame/return-address stack-frame)))
    (and (interpreter-return-address? return-address)
	 (return-address/code return-address))))

(define-integrable (stack-frame/compiled-code? stack-frame)
  (compiled-return-address? (stack-frame/return-address stack-frame)))

(define (stack-frame/subproblem? stack-frame)
  (if (stack-frame/stack-marker? stack-frame)
      (stack-marker-frame/repl-eval-boundary? stack-frame)
      (stack-frame-type/subproblem? (stack-frame/type stack-frame))))

(define (stack-frame/resolve-stack-address frame address)
  (let loop
      ((frame frame)
       (offset (stack-address->index address (stack-frame/offset frame))))
    (let ((length (stack-frame/length frame)))
      (if (fix:< offset length)
	  (values frame offset)
	  (loop (stack-frame/next frame) (fix:- offset length))))))

(define (stack-frame/skip-non-subproblems stack-frame)
  (let ((type (stack-frame/type stack-frame)))
    (cond ((and (stack-frame/subproblem? stack-frame)
		(not (and (eq? type stack-frame-type/compiled-return-address)
			  (eq? (stack-frame/return-address stack-frame)
			       continuation-return-address))))
	   stack-frame)
	  ((stack-frame/stack-marker? stack-frame)
	   (let loop ((stack-frame stack-frame))
	     (let ((stack-frame (stack-frame/next stack-frame)))
	       (and stack-frame
		    (if (stack-frame/subproblem? stack-frame)
			(stack-frame/next-subproblem stack-frame)
			(loop stack-frame))))))
	  (else
	   (let ((stack-frame (stack-frame/next stack-frame)))
	     (and stack-frame
		  (stack-frame/skip-non-subproblems stack-frame)))))))

(define continuation-return-address)

(define (initialize-special-frames!)
  (set! continuation-return-address
	(let ((stack-frame
	       (call-with-current-continuation
		(lambda (k)
		  k
		  (call-with-current-continuation
		   continuation/first-subproblem)))))
	  (and (eq? (stack-frame/type stack-frame)
		    stack-frame-type/compiled-return-address)
	       (stack-frame/return-address stack-frame))))
  unspecific)

;;;; Parser

(define-structure (parser-state (constructor make-parser-state)
				(conc-name parser-state/))
  (dynamic-state #f read-only #t)
  (block-thread-events? #f read-only #t)
  (interrupt-mask #f read-only #t)
  (history #f read-only #t)
  (previous-history-offset #f read-only #t)
  (previous-history-control-point #f read-only #t)
  (element-stream #f read-only #t)
  (n-elements #f read-only #t)
  (next-control-point #f read-only #t)
  (previous-type #f read-only #t))

(define (continuation->stack-frame continuation)
  (parse-control-point (continuation/control-point continuation)
		       (continuation/dynamic-state continuation)
		       (continuation/block-thread-events? continuation)
		       #f))

(define (parse-control-point control-point dynamic-state block-thread-events?
			     type)
  (let ((element-stream (control-point/element-stream control-point)))
    (parse-one-frame
     (make-parser-state
      dynamic-state
      block-thread-events?
      (control-point/interrupt-mask control-point)
      (let ((history
	     (history-transform (control-point/history control-point))))
	(if (and (stream-pair? element-stream)
		 (eq? return-address/reenter-compiled-code
		      (stream-car element-stream)))
	    history
	    (history-superproblem history)))
      (control-point/previous-history-offset control-point)
      (control-point/previous-history-control-point control-point)
      element-stream
      (control-point/n-elements control-point)
      (control-point/next-control-point control-point)
      type))))

(define (parse-one-frame state)
  (let ((handle-ordinary
	 (lambda (stream)
	   (let ((type
		  (return-address->stack-frame-type
		   (stream-car stream)
		   (let ((type (parser-state/previous-type state)))
		     (and type
			  (1d-table/get (stack-frame-type/properties type)
					allow-extended?-tag
					#f))))))
	     (let ((length
		    (let ((length (stack-frame-type/length type)))
		      (if (exact-nonnegative-integer? length)
			  length
			  (length stream (parser-state/n-elements state))))))
	       ((stack-frame-type/parser type)
		type
		(list->vector (stream-head stream length))
		(make-intermediate-state state
					 length
					 (stream-tail stream length)))))))
	(the-stream (parser-state/element-stream state)))
    (if (stream-pair? the-stream)
	(handle-ordinary the-stream)
	(let ((control-point (parser-state/next-control-point state)))
	  (and control-point
	       (if (fix:> (parser-state/n-elements state) 0)
		   ;; Construct invisible join-stacklets frame.
		   (handle-ordinary
		    (stream return-address/join-stacklets control-point))
		   (parse-control-point
		    control-point
		    (parser-state/dynamic-state state)
		    (parser-state/block-thread-events? state)
		    (parser-state/previous-type state))))))))

;;; MAKE-INTERMEDIATE-STATE is used to construct an intermediate
;;; parser state that is passed to the frame parser.  This
;;; intermediate state is identical to STATE except that it shows
;;; LENGTH items having been removed from the stream.

(define (make-intermediate-state state length stream)
  (let ((previous-history-control-point
	 (parser-state/previous-history-control-point state))
	(new-length
	 (fix:- (parser-state/n-elements state) length)))
    (make-parser-state
     (parser-state/dynamic-state state)
     (parser-state/block-thread-events? state)
     (parser-state/interrupt-mask state)
     (parser-state/history state)
     (let ((previous (parser-state/previous-history-offset state)))
       (if (or previous-history-control-point
	       (fix:>= new-length previous))
	   previous
	   0))
     previous-history-control-point
     stream
     new-length
     (parser-state/next-control-point state)
     (parser-state/previous-type state))))

;;; After each frame parser is done, it either tail recurses into the
;;; parsing loop, or it calls PARSE/STANDARD-NEXT to produces a new
;;; output frame.  The argument STATE is usually what was passed to
;;; the frame parser (i.e. the state that was returned by the previous
;;; call to MAKE-INTERMEDIATE-STATE).  However, several of the parsers
;;; change the values of some of the components of STATE before
;;; calling PARSE/STANDARD-NEXT -- for example, RESTORE-INTERRUPT-MASK
;;; changes the INTERRUPT-MASK component.

(define (parse/standard-next type elements state history? force-pop?)
  (let ((n-elements (parser-state/n-elements state))
	(history-subproblem?
	 (stack-frame-type/history-subproblem? type))
	(history (parser-state/history state))
	(previous-history-offset (parser-state/previous-history-offset state))
	(previous-history-control-point
	 (parser-state/previous-history-control-point state)))
    (make-stack-frame
     type
     elements
     (parser-state/dynamic-state state)
     (parser-state/block-thread-events? state)
     (parser-state/interrupt-mask state)
     (if history?
	 history
	 undefined-history)
     previous-history-offset
     previous-history-control-point
     (fix:+ (vector-length elements) n-elements)
     (parser-state/previous-type state)
     (make-parser-state (parser-state/dynamic-state state)
			(parser-state/block-thread-events? state)
			(parser-state/interrupt-mask state)
			(if (or force-pop? history-subproblem?)
			    (history-superproblem history)
			    history)
			previous-history-offset
			previous-history-control-point
			(parser-state/element-stream state)
			n-elements
			(parser-state/next-control-point state)
			type))))

(define (parser/standard type elements state)
  (parse/standard-next type elements state
		       (and (stack-frame-type/history-subproblem? type)
			    (stack-frame-type/subproblem? type))
		       #f))

(define (parser/standard-compiled type elements state)
  (parse/standard-next
   type elements state
   (let ((stream (parser-state/element-stream state)))
     (and (stream-pair? stream)
	  (eq? (return-address->stack-frame-type (stream-car stream) #t)
	       stack-frame-type/return-to-interpreter)))
   #f))

(define (parser/apply type elements state)
  (let ((valid-history?
	 (not (let ((stream (parser-state/element-stream state)))
		(and (stream-pair? stream)
		     (eq? return-address/reenter-compiled-code
			  (stream-car stream)))))))
    (parse/standard-next type elements state valid-history? valid-history?)))

(define (parser/restore-interrupt-mask type elements state)
  (parser/standard
   type
   elements
   (make-parser-state (parser-state/dynamic-state state)
		      (parser-state/block-thread-events? state)
		      (vector-ref elements 1)
		      (parser-state/history state)
		      (parser-state/previous-history-offset state)
		      (parser-state/previous-history-control-point state)
		      (parser-state/element-stream state)
		      (parser-state/n-elements state)
		      (parser-state/next-control-point state)
		      (parser-state/previous-type state))))

(define (parser/restore-history type elements state)
  (parser/standard
   type
   elements
   (make-parser-state (parser-state/dynamic-state state)
		      (parser-state/block-thread-events? state)
		      (parser-state/interrupt-mask state)
		      (history-transform (vector-ref elements 1))
		      (vector-ref elements 2)
		      (vector-ref elements 3)
		      (parser-state/element-stream state)
		      (parser-state/n-elements state)
		      (parser-state/next-control-point state)
		      (parser-state/previous-type state))))

(define-integrable code/special-compiled/internal-apply 0)
(define-integrable code/special-compiled/restore-interrupt-mask 1)
(define-integrable code/special-compiled/stack-marker 2)
(define-integrable code/special-compiled/compiled-code-bkpt 3)
(define-integrable code/interrupt-restart 4)
(define-integrable code/restore-regs 5)
(define-integrable code/apply-compiled 6)
(define-integrable code/continue-linking 7)

(define (parser/special-compiled type elements state)
  (let ((code (vector-ref elements 1)))
    (cond ((fix:= code code/special-compiled/internal-apply)
	   (parse/standard-next type elements state #f #f))
	  ((fix:= code code/special-compiled/restore-interrupt-mask)
	   (parser/%stack-marker (parser-state/dynamic-state state)
				 (parser-state/block-thread-events? state)
				 (vector-ref elements 2)
				 type elements state))
	  ((fix:= code code/special-compiled/stack-marker)
	   (parser/stack-marker type elements state))
	  ((or (fix:= code code/special-compiled/compiled-code-bkpt)
	       (fix:= code code/interrupt-restart)
	       (fix:= code code/restore-regs)
	       (fix:= code code/apply-compiled)
	       (fix:= code code/continue-linking))
	   (parse/standard-next type elements state #f #f))
	  (else
	   (error "Unknown special compiled frame code:" code)))))

(define (parser/stack-marker type elements state)
  (call-with-values
      (lambda ()
	(if (eq? type stack-frame-type/stack-marker)
	    (values (vector-ref elements 1) (vector-ref elements 2))
	    (values (vector-ref elements 2) (vector-ref elements 3))))
    (lambda (marker-type marker-instance)
      (let ((continue
	     (lambda (dynamic-state block-thread-events? interrupt-mask)
	       (parser/%stack-marker dynamic-state block-thread-events?
				     interrupt-mask type elements state))))
	(cond ((eq? marker-type %translate-to-state-point)
	       (continue (merge-dynamic-state
			  (parser-state/dynamic-state state)
			  marker-instance)
			 (parser-state/block-thread-events? state)
			 (parser-state/interrupt-mask state)))
	      ((eq? marker-type set-interrupt-enables!)
	       (continue (parser-state/dynamic-state state)
			 (parser-state/block-thread-events? state)
			 marker-instance))
	      ((eq? marker-type with-thread-events-blocked)
	       (continue (parser-state/dynamic-state state)
			 marker-instance
			 (parser-state/interrupt-mask state)))
	      (else
	       (continue (parser-state/dynamic-state state)
			 (parser-state/block-thread-events? state)
			 (parser-state/interrupt-mask state))))))))

(define (parser/%stack-marker dynamic-state block-thread-events? interrupt-mask
			      type elements state)
  (parser/standard
   type
   elements
   (make-parser-state dynamic-state
		      block-thread-events?
		      interrupt-mask
		      (parser-state/history state)
		      (parser-state/previous-history-offset state)
		      (parser-state/previous-history-control-point state)
		      (parser-state/element-stream state)
		      (parser-state/n-elements state)
		      (parser-state/next-control-point state)
		      (parser-state/previous-type state))))

(define (stack-frame/stack-marker? stack-frame)
  (or (%stack-frame/stack-marker? stack-frame)
      (and (stack-frame/special-compiled? stack-frame)
	   (let ((code (vector-ref (stack-frame/elements stack-frame) 1)))
	     (or (fix:= code/special-compiled/restore-interrupt-mask code)
		 (fix:= code/special-compiled/stack-marker code))))))

(define (stack-marker-frame/type stack-frame)
  (if (%stack-frame/stack-marker? stack-frame)
      (vector-ref (stack-frame/elements stack-frame) 1)
      (vector-ref (stack-frame/elements stack-frame) 2)))

(define (stack-marker-frame/instance stack-frame)
  (if (%stack-frame/stack-marker? stack-frame)
      (vector-ref (stack-frame/elements stack-frame) 2)
      (vector-ref (stack-frame/elements stack-frame) 3)))

(define-integrable (%stack-frame/stack-marker? stack-frame)
  (eq? stack-frame-type/stack-marker (stack-frame/type stack-frame)))

(define-integrable (stack-frame/special-compiled? stack-frame)
  (eq? stack-frame-type/special-compiled (stack-frame/type stack-frame)))

(define (stack-frame/repl-eval-boundary? stack-frame)
  (and (stack-frame/stack-marker? stack-frame)
       (stack-marker-frame/repl-eval-boundary? stack-frame)))

(define-integrable (stack-marker-frame/repl-eval-boundary? stack-frame)
  (eq? with-repl-eval-boundary (stack-marker-frame/type stack-frame)))

;;;; Unparser

(define (stack-frame->continuation stack-frame)
  (make-continuation (stack-frame->control-point stack-frame)
		     (stack-frame/dynamic-state stack-frame)
		     #f))

(define (stack-frame->control-point stack-frame)
  (with-values (lambda () (unparse/stack-frame stack-frame))
    (lambda (element-stream next-control-point)
      (make-control-point
       (stack-frame/interrupt-mask stack-frame)
       (let ((history (stack-frame/history stack-frame)))
	 (if (eq? history undefined-history)
	     (fixed-objects-item 'DUMMY-HISTORY)
	     (history-untransform history)))
       (stack-frame/previous-history-offset stack-frame)
       (stack-frame/previous-history-control-point stack-frame)
       (if (stack-frame/compiled-code? stack-frame)
	   (cons-stream return-address/reenter-compiled-code
			(cons-stream #f element-stream))
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
		     (values (stream) #f)))))
	(lambda (element-stream next-control-point)
	  (values
	   (let ((elements (stack-frame/elements stack-frame)))
	     (let ((length (vector-length elements)))
	       (let loop ((index 0))
		 (if (fix:< index length)
		     (cons-stream (vector-ref elements index)
				  (loop (fix:+ index 1)))
		     element-stream))))
	   next-control-point)))))

(define return-address/join-stacklets)
(define return-address/reenter-compiled-code)

;;;; Special Frame Lengths

(define (length/combination-save-value stream offset)
  offset
  (fix:+ 3 (system-vector-length (stream-ref stream 1))))

(define ((length/application-frame index missing) stream offset)
  offset
  (fix:+ (fix:+ index 1)
	 (fix:- (object-datum (stream-ref stream index)) missing)))

(define (length/compiled-return-address stream offset)
  (let ((entry (stream-car stream)))
    (let ((frame-size (compiled-continuation/next-continuation-offset entry)))
      (if frame-size
	  (fix:+ frame-size 1)
	  (stack-address->index
	   ;; Search for the dynamic link.  This heuristic compensates
	   ;; for the compiler omitting its location in the object
	   ;; code.
	   (let loop ((s (stream-cdr stream)))
	     (if (not (stream-pair? s))
		 (error "Unable to find dynamic link:" stream))
	     (let ((item (stream-car s)))
	       (if (stack-address? item)
		   item
		   (loop (stream-cdr s)))))
	   offset)))))

(define (length/special-compiled stream offset)
  ;; return address is reflect-to-interface
  offset
  (let* ((code (stream-ref stream 1))
	 (lose
	  (lambda () (error "Unknown special compiled frame code:" code))))
    (cond ((not (fix:fixnum? code))
	   (lose))
	  ((fix:= code code/special-compiled/internal-apply)
	   ;; Very infrequent!
	   (fix:+ 3 (object-datum (stream-ref stream 2))))
	  ((fix:= code code/special-compiled/restore-interrupt-mask)
	   3)
	  ((fix:= code code/special-compiled/stack-marker)
	   4)
	  ((fix:= code code/special-compiled/compiled-code-bkpt)
	   ;; Very infrequent!
	   (let ((fsize
		  (compiled-code-address/frame-size (stream-ref stream 2))))
	     (if (not fsize)
		 5
		 (fix:+ 5 fsize))))
	  ((fix:= code code/interrupt-restart)
	   (let ((homes-saved (object-datum (stream-ref stream 2)))
		 (regs-saved (object-datum (stream-ref stream 3))))
	     ;; The first reg saved is _always_ the continuation,
	     ;; part of the next frame.
	     (fix:- (fix:+
		     ;; Return code, reflect code, homes saved, regs saved,
		     ;; and entry point
		     5
		     (fix:+ homes-saved regs-saved))
		    1)))
	  ((fix:= code code/restore-regs)
	   (fix:+ 3 (object-datum (stream-ref stream 2))))
	  ((fix:= code code/apply-compiled)
	   ;; Stream[2] is code entry point, [3] is frame size
	   (fix:+ 3 (object-datum (stream-ref stream 3))))
	  ((fix:= code code/continue-linking)
	   ;; return code, reflect code, entry size, original count,
	   ;; block, environment, offset, last header offset,sections,
	   ;; return address
	   (fix:- 10 1))
	  (else
	   (lose)))))

(define (length/interrupt-compiled-procedure stream offset)
  offset				; ignored
  (fix:+ (compiled-procedure-frame-size (stream-car stream)) 1))

(define (compiled-code-address/frame-size cc-address)
  (let ((lose (lambda () (error "Unexpected object:" cc-address))))
    (cond ((not (compiled-code-address? cc-address))
	   (lose))
	  ((compiled-return-address? cc-address)
	   (let ((offset
		  (compiled-continuation/next-continuation-offset cc-address)))
	     (and offset
		  (fix:+ offset 1))))
	  ((compiled-procedure? cc-address)
	   (fix:+ (compiled-procedure-frame-size cc-address) 1))
	  (else
	   (lose)))))

(define (verify paranoia-index stream offset)
  (if (or (= paranoia-index 0) (stream-null? stream))
      #t
      (let* ((type (return-address->stack-frame-type (stream-car stream) #f))
	     (length
	      (let ((length (stack-frame-type/length type)))
		(if (exact-nonnegative-integer? length)
		    length
		    (length stream offset))))
	     (ltail (stream-tail* stream length)))
	(and ltail
	     (return-address? (stream-car ltail))
	     (verify (- paranoia-index 1)
		     ltail
		     (fix:+ offset length))))))

(define (stream-tail* stream n)
  (if (or (fix:= n 0) (stream-null? stream))
      stream
      (begin
	(if (not (stream-pair? stream))
	    (error:wrong-type-argument stream "stream" 'STREAM-TAIL*))
	(stream-tail* (stream-cdr stream) (fix:- n 1)))))

;;;; Stack Frame Types

(define-structure (stack-frame-type
		   (constructor make-stack-frame-type
				(code subproblem? history-subproblem? length
				      parser))
		   (conc-name stack-frame-type/))
  (code #f read-only #t)
  (subproblem? #f read-only #t)
  (history-subproblem? #f read-only #t)
  (length #f read-only #t)
  (parser #f read-only #t)
  (properties (make-1d-table) read-only #t))

(define allow-extended?-tag
  (list 'ALLOW-EXTENDED?))

(define (microcode-return/code->type code)
  (if (not (fix:< code (vector-length stack-frame-types)))
      (error:bad-range-argument code 'MICROCODE-RETURN/CODE->TYPE))
  (vector-ref stack-frame-types code))

(define (microcode-return/name->type name)
  (microcode-return/code->type (microcode-return name)))

(define (return-address->stack-frame-type return-address allow-extended?)
  allow-extended?			; ignored
  (cond ((interpreter-return-address? return-address)
	 (let ((code (return-address/code return-address)))
	   (let ((type (microcode-return/code->type code)))
	     (if (not type)
		 (error "Return code has no type:" code))
	     type)))
	((compiled-return-address? return-address)
	 (cond ((compiled-continuation/return-to-interpreter? return-address)
		stack-frame-type/return-to-interpreter)
	       ((compiled-continuation/reflect-to-interface? return-address)
		stack-frame-type/special-compiled)
	       (else stack-frame-type/compiled-return-address)))
	((compiled-procedure? return-address)
	 stack-frame-type/interrupt-compiled-procedure)
	((compiled-expression? return-address)
	 stack-frame-type/interrupt-compiled-expression)
	(else
	 (error:bad-range-argument return-address
				   'RETURN-ADDRESS->STACK-FRAME-TYPE))))

(define (initialize-package!)
  (set! return-address/join-stacklets
	(make-return-address (microcode-return 'JOIN-STACKLETS)))
  (set! return-address/reenter-compiled-code
	(make-return-address (microcode-return 'REENTER-COMPILED-CODE)))
  (set! stack-frame-types (make-stack-frame-types))
  (set! stack-frame-type/hardware-trap
	(microcode-return/name->type 'HARDWARE-TRAP))
  (set! stack-frame-type/stack-marker
	(microcode-return/name->type 'STACK-MARKER))
  (set! stack-frame-type/compiled-return-address
	(make-stack-frame-type #f #t #f length/compiled-return-address
			       parser/standard-compiled))
  (set! stack-frame-type/return-to-interpreter
	(make-stack-frame-type #f #f #t 1 parser/standard))
  (set! stack-frame-type/special-compiled
	(make-stack-frame-type #f #t #f length/special-compiled
			       parser/special-compiled))
  (set! stack-frame-type/interrupt-compiled-procedure
	(make-stack-frame-type #f #t #f length/interrupt-compiled-procedure
			       parser/standard))
  (set! stack-frame-type/interrupt-compiled-expression
	(make-stack-frame-type #f #t #f 1 parser/standard))
  (set! word-size
	(let ((b1 (system-vector-length (make-bit-string 1 #f))))
	  (let loop ((size 2))
	    (if (fix:= (system-vector-length (make-bit-string size #f)) b1)
		(loop (fix:+ size 1))
		(fix:- size 1)))))
  (set! continuation-return-address #f)
  unspecific)

(define stack-frame-types)
(define stack-frame-type/compiled-return-address)
(define stack-frame-type/return-to-interpreter)
(define stack-frame-type/special-compiled)
(define stack-frame-type/hardware-trap)
(define stack-frame-type/stack-marker)
(define stack-frame-type/interrupt-compiled-procedure)
(define stack-frame-type/interrupt-compiled-expression)

(define (make-stack-frame-types)
  (let ((types (make-vector (microcode-return/code-limit) #f)))

    (define (stack-frame-type name subproblem? history-subproblem? length
			      parser)
      (let ((code (microcode-return name)))
	(let ((type
	       (make-stack-frame-type code subproblem? history-subproblem?
				      length parser)))
	  (vector-set! types code type)
	  type)))

    (define (standard-frame name length #!optional parser)
      (stack-frame-type name #f #f length
			(if (default-object? parser) parser/standard parser)))

    (define (standard-subproblem name length)
      (stack-frame-type name #t #t length parser/standard))

    (define (non-history-subproblem name length #!optional parser)
      (stack-frame-type name #t #f length
			(if (default-object? parser) parser/standard parser)))

    (standard-frame 'RESTORE-INTERRUPT-MASK 2 parser/restore-interrupt-mask)
    (standard-frame 'RESTORE-HISTORY 4 parser/restore-history)
    (standard-frame 'RESTORE-DONT-COPY-HISTORY 4 parser/restore-history)
    (standard-frame 'STACK-MARKER 3 parser/stack-marker)

    (standard-frame 'NON-EXISTENT-CONTINUATION 2)
    (standard-frame 'HALT 2)
    (standard-frame 'JOIN-STACKLETS 2)
    (standard-frame 'POP-RETURN-ERROR 2)
    (standard-frame 'RESTORE-VALUE 2)

    (standard-subproblem 'IN-PACKAGE-CONTINUE 2)
    (standard-subproblem 'ACCESS-CONTINUE 2)
    (standard-subproblem 'PRIMITIVE-COMBINATION-1-APPLY 2)
    (standard-subproblem 'FORCE-SNAP-THUNK 2)
    (standard-subproblem 'GC-CHECK 2)
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
    (standard-subproblem 'MOVE-TO-ADJACENT-POINT 6)
    (standard-subproblem 'COMBINATION-SAVE-VALUE length/combination-save-value)

    (let ((length (length/application-frame 2 0)))
      (standard-subproblem 'COMBINATION-APPLY length)
      (non-history-subproblem 'INTERNAL-APPLY length parser/apply)
      (non-history-subproblem 'INTERNAL-APPLY-VAL length parser/apply))

    (let ((compiler-frame
	   (lambda (name length)
	     (stack-frame-type name #f #t length parser/standard)))
	  (compiler-subproblem
	   (lambda (name length)
	     (stack-frame-type name #t #t length parser/standard))))

      (let ((length (length/application-frame 4 0)))
	(compiler-subproblem 'COMPILER-LOOKUP-APPLY-TRAP-RESTART length)
	(compiler-subproblem 'COMPILER-OPERATOR-LOOKUP-TRAP-RESTART length))

      (let ((type (compiler-frame 'COMPILER-INTERRUPT-RESTART 3)))
	(1d-table/put! (stack-frame-type/properties type)
		       allow-extended?-tag
		       #t))

      (compiler-frame 'COMPILER-LINK-CACHES-RESTART 8)
      (compiler-frame 'REENTER-COMPILED-CODE 2)

      (compiler-subproblem 'COMPILER-ACCESS-RESTART 4)
      (compiler-subproblem 'COMPILER-ASSIGNMENT-RESTART 5)
      (compiler-subproblem 'COMPILER-ASSIGNMENT-TRAP-RESTART 5)
      (compiler-subproblem 'COMPILER-DEFINITION-RESTART 5)
      (compiler-subproblem 'COMPILER-LOOKUP-APPLY-RESTART
			   (length/application-frame 4 1))
      (compiler-subproblem 'COMPILER-REFERENCE-RESTART 4)
      (compiler-subproblem 'COMPILER-REFERENCE-TRAP-RESTART 4)
      (compiler-subproblem 'COMPILER-SAFE-REFERENCE-RESTART 4)
      (compiler-subproblem 'COMPILER-SAFE-REFERENCE-TRAP-RESTART 4)
      (compiler-subproblem 'COMPILER-UNASSIGNED?-RESTART 4)
      (compiler-subproblem 'COMPILER-UNASSIGNED?-TRAP-RESTART 4)
      (compiler-subproblem 'COMPILER-UNBOUND?-RESTART 4)

      (compiler-subproblem 'COMPILER-ERROR-RESTART 3))

    (non-history-subproblem 'HARDWARE-TRAP length/hardware-trap)
    types))

;;;; Hardware trap parsing

(define-integrable hardware-trap/frame-size 9)

(define-integrable hardware-trap/signal-index 1)
(define-integrable hardware-trap/signal-name-index 2)
(define-integrable hardware-trap/code-index 3)
(define-integrable hardware-trap/stack-index 4)
(define-integrable hardware-trap/state-index 5)
(define-integrable hardware-trap/pc-info1-index 6)
(define-integrable hardware-trap/pc-info2-index 7)
(define-integrable hardware-trap/extra-info-index 8)

(define (length/hardware-trap stream offset)
  (let ((state (stream-ref stream hardware-trap/state-index))
	(stack-recovered? (stream-ref stream hardware-trap/stack-index)))
    (if (not stack-recovered?)
	hardware-trap/frame-size
	(let ((after-header (stream-tail stream hardware-trap/frame-size)))
	  (case state
	    ((1)
	     ;; primitive
	     (let* ((primitive
		     (stream-ref stream hardware-trap/pc-info1-index))
		    (arity (primitive-procedure-arity primitive))
		    (nargs
		     (if (< arity 0)
			 (stream-ref stream hardware-trap/pc-info2-index)
			 arity)))
	       (if (return-address? (stream-ref after-header nargs))
		   (+ hardware-trap/frame-size nargs)
		   (- (heuristic (stream-tail after-header nargs)
				 (+ hardware-trap/frame-size nargs offset))
		      offset))))
	    ((0 2 3 4 5)
	     ;; unknown, cc, probably cc, builtin, or utility
	     (- (heuristic after-header (+ hardware-trap/frame-size offset))
		offset))
	    (else
	     (error "Unknown state:" state)))))))

(define (heuristic stream offset)
  (if (or (stream-null? stream)
	  (and (return-address? (stream-car stream))
	       (verify 2 stream offset)))
      offset
      (heuristic (stream-cdr stream) (+ offset 1))))

(define (hardware-trap-frame? frame)
  (and (stack-frame? frame)
       (eq? (stack-frame/type frame)
	    stack-frame-type/hardware-trap)))

(define-guarantee hardware-trap-frame "hardware-trap frame")

(define (hardware-trap-frame/code frame)
  (guarantee-hardware-trap-frame frame 'hardware-trap-frame/code)
  (let ((code (stack-frame/ref frame hardware-trap/code-index)))
    (cond ((pair? code) (cdr code))
	  ((string? code) code)
	  (else #f))))

(define (hardware-trap-frame/print-registers frame)
  (guarantee-hardware-trap-frame frame 'hardware-trap-frame/print-registers)
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
		  (loop (+ i 1)))))))))

(define (print-register block index name)
  (let ((value
	 (let ((bit-string (bit-string-allocate word-size)))
	   (read-bits! block (* word-size (+ index 1)) bit-string)
	   (bit-string->unsigned-integer bit-string))))
    (newline)
    (write-string "  ")
    (write-string name)
    (write-string " = ")
    (write-string (number->string value 16))))

(define word-size)

(define (hardware-trap-frame/print-stack frame)
  (guarantee-hardware-trap-frame frame 'hardware-trap-frame/print-stack)
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

(define (write-hex value)
  (if (< value #x10)
      (write value)
      (begin
	(write-string "#x")
	(write-string (number->string value #x10)))))

(define (hardware-trap-frame/describe frame long?)
  (guarantee-hardware-trap-frame frame 'hardware-trap-frame/describe)
  (let ((name (stack-frame/ref frame hardware-trap/signal-name-index))
	(state (stack-frame/ref frame hardware-trap/state-index)))
    (if (not name)
	(write-string "User microcode reset")
	(let ((code (stack-frame/ref frame hardware-trap/code-index)))
	  (write-string "Hardware trap ")
	  (write-string name)
	  (write-string " (")
	  (if (and (pair? code) (cdr code))
	      (write-string (cdr code))
	      (begin
		(write-string "code = ")
		(write-hex (if (pair? code)
			       (car code)
			       code))))
	  (write-string ")")))
    (if long?
	(case state
	  ((0)				; unknown
	   (write-string " at an unknown location."))
	  ((1)				; primitive
	   (write-string " within ")
	   (write (stack-frame/ref frame hardware-trap/pc-info1-index)))
	  ((2)				; compiled code
	   (write-string " at offset ")
	   (write-hex (stack-frame/ref frame hardware-trap/pc-info2-index))
	   (newline)
	   (write-string "within ")
	   (let ((block (stack-frame/ref frame hardware-trap/pc-info1-index)))
	     (write block)
	     (call-with-values
		 (lambda () (compiled-code-block/filename-and-index block))
	       (lambda (filename index)
		 index
		 (if filename
		     (begin
		       (write-string " (")
		       (write-string filename)
		       (write-string ")")))))))
	  ((3)				; probably compiled-code
	   (write-string " at an unknown compiled-code location."))
	  ((4)				; builtin (i.e. hook)
	   (let* ((index (stack-frame/ref frame hardware-trap/pc-info1-index))
		  (name ((ucode-primitive builtin-index->name 1) index)))
	     (if name
		 (begin
		   (write-string " in assembly-language utility ")
		   (write-string name))
		 (begin
		   (write-string " in unknown assembly-language utility ")
		   (write-hex index)))))
	  ((5)				; utility
	   (let* ((index (stack-frame/ref frame hardware-trap/pc-info1-index))
		  (name ((ucode-primitive utility-index->name 1) index)))
	     (if name
		 (begin
		   (write-string " in compiled-code utility ")
		   (write-string name))
		 (begin
		   (write-string " in unknown compiled-code utility ")
		   (write-hex index)))))
	  (else
	   (error "Unknown state:" state))))))