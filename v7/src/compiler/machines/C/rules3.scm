#| -*-Scheme-*-

$Id: rules3.scm,v 1.3 1993/10/27 22:10:14 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Invocations and Entries
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Invocations

(define (pop-return)
  (use-pop-return!)
  (LAP ,@(clear-map!)
       "POP_RETURN();\n\t"))

(define-rule statement
  (POP-RETURN)
  (pop-return))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation				;ignore
  (let ()
    (use-invoke-interface! 2)
    (LAP ,@(clear-map!)
	 "{\n\t  SCHEME_OBJECT procedure = *stack_pointer++;\n\t"
	 "  INVOKE_INTERFACE_2 (" ,code:compiler-apply ", procedure, "
	 ,frame-size ");\n\t}\n\t")))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation		;ignore
  (LAP ,@(clear-map!)
       "goto " ,label ";\n\t"))

(define-rule statement
  (INVOCATION:COMPUTED-JUMP (? frame-size) (? continuation))
  frame-size continuation		;ignore
  (pop-return))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation				;ignore
  (let ()
    (use-invoke-interface! 2)
    (LAP ,@(clear-map!)
	 "{\n\t  SCHEME_OBJECT * procedure_address = &current_block["
	 ,(label->offset label)
	 "];\n\t  INVOKE_INTERFACE_2 (" ,code:compiler-lexpr-apply
	 ", procedure_address, " ,number-pushed ");\n\t}\n\t")))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation				;ignore
  ;; Destination address is at TOS; pop it into second-arg
  (let ()
    (use-invoke-interface! 2)
    (LAP ,@(clear-map!)
	 "{n\t SCHEME_OBJECT procedure = *stack_pointer++;\n\t  "
	 "SCHEME_OBJECT * procedure_address = (OBJECT_ADDRESS (procedure));\n\t"
	 "  INVOKE_INTERFACE_2 (" ,code:compiler-lexpr-apply
	 ", procedure_address, " ,number-pushed ");\n\t}\n\t")))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (begin
    (use-jump-execute-chache!)
    (LAP ,@(clear-map!)
	 "JUMP_EXECUTE_CHACHE (" ,(free-uuo-link-label name frame-size) ");\n\t")))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (begin
    (use-jump-execute-chache!)
    (LAP ,@(clear-map!)
	 "JUMP_EXECUTE_CHACHE (" ,(global-uuo-link-label name frame-size) ");\n\t")))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size)
			      (? continuation)
			      (REGISTER (? extension)))
  continuation				;ignore
  (let ((extension (standard-source! extension 'SCHEME_OBJECT*)))
    (use-invoke-interface! 3)
    (LAP ,@(clear-map!)
	 "INVOKE_INTERFACE_3 (" ,code:compiler-cache-reference-apply
	 ", " ,extension ", current_block, " ,frame-size ");\n\t")))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size)
		     (? continuation)
		     (REGISTER (? environment))
		     (? name))
  continuation				;ignore
  (let ((environment (standard-source! environment 'SCHEME_OBJECT)))
    (use-invoke-interface! 3)
    (LAP ,@(clear-map!)
	 "INVOKE_INTERFACE_3 (" ,code:compiler-lookup-apply
	 ", " ,environment ", current_block[" ,(object->offset name) "]"
	 ", " ,frame-size ");\n\t")))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation				;ignore
  (cond ((eq? primitive compiled-error-procedure)
	 (use-invoke-interface! 1)
	 (LAP ,@(clear-map!)
	      "INVOKE_INTERFACE_1 (" ,code:compiler-error ", "
	      ,frame-size ");\n\t"))
	(else
	 (let ((arity (primitive-procedure-arity primitive)))
	   (cond ((= arity (-1+ frame-size))
		  (use-invoke-primitive!)
		  (LAP ,@(clear-map!)
		       "INVOKE_PRIMITIVE (current_block["
		       ,(object->offset primitive) "], "
		       ,arity
		       ");\n\t"))
		 #|
		 ((= arity -1)
		  (LAP ,@(clear-map!)
		       "INVOKE_INTERFACE_2 (" ,code:compiler-apply
		       ", (current_block[" ,(object->offset primitive) "]"
		       ", " ,frame-size ");\n\t"))
		 |#
		 (else
		  (if (not (= arity -1))
		      (error "Wrong number of arguments to primitive"
			     primitive (-1+ frame-size)))
		  (use-invoke-interface! 2)
		  (LAP ,@(clear-map!)
		       "INVOKE_INTERFACE_2 (" ,code:compiler-apply
		       ", current_block[" ,(object->offset primitive) "]"
		       ", " ,frame-size ");\n\t")))))))

(define (invoke-special-primitive code)
  (use-invoke-interface! 0)
  (LAP ,@(clear-map!)
       "INVOKE_INTERFACE_0 (" ,code ");\n\t"))

(let-syntax
    ((define-special-primitive-invocation
       (macro (name)
	 `(DEFINE-RULE STATEMENT
	    (INVOCATION:SPECIAL-PRIMITIVE
	     (? FRAME-SIZE)
	     (? CONTINUATION)
	     ,(make-primitive-procedure name true))
	    FRAME-SIZE CONTINUATION
	    (invoke-special-primitive
	     ,(symbol-append 'CODE:COMPILER- name))))))
  (define-special-primitive-invocation &+)
  (define-special-primitive-invocation &-)
  (define-special-primitive-invocation &*)
  (define-special-primitive-invocation &/)
  (define-special-primitive-invocation &=)
  (define-special-primitive-invocation &<)
  (define-special-primitive-invocation &>)
  (define-special-primitive-invocation 1+)
  (define-special-primitive-invocation -1+)
  (define-special-primitive-invocation zero?)
  (define-special-primitive-invocation positive?)
  (define-special-primitive-invocation negative?))

;;;; Invocation Prefixes

;;; (INVOCATION-PREFIX:MOVE-FRAME-UP frame-size address)

;;; Move the topmost <frame-size> words of the stack downward so that
;;; the bottommost of these words is at location <address>, and set
;;; the stack pointer to the topmost of the moved words.  That is,
;;; discard the words between <address> and SP+<frame-size>, close the
;;; resulting gap by shifting down the words from above the gap, and
;;; adjust SP to point to the new topmost word.

(define-rule statement
  ;; Move up 0 words back to top of stack : a No-Op
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER ,regnum:stack-pointer))
  (LAP))

(define-rule statement
  ;; Move <frame-size> words back to dynamic link marker
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER (? new-frame)))
  (let ((new-frame (standard-source! new-frame 'SCHEME_OBJECT*)))
    (move-frame-up frame-size new-frame "")))

(define (move-frame-up frame-size new-frame pfx)
  (case frame-size
    ((0)
     (LAP ,pfx "stack_pointer = " ,new-frame ";\n\t"))
    ((1)
     (LAP ,pfx "*--" ,new-frame " = stack_pointer[0];\n\t"
	  ,pfx "stack_pointer = " ,new-frame ";\n\t"))
    ((2)
     (LAP ,pfx "*--" ,new-frame " = stack_pointer[1];\n\t"
	  ,pfx "*--" ,new-frame " = stack_pointer[0];\n\t"
	  ,pfx "stack_pointer = " ,new-frame ";\n\t"))
    ((3)
     (LAP ,pfx "*--" ,new-frame " = stack_pointer[2];\n\t"
	  ,pfx "*--" ,new-frame " = stack_pointer[1];\n\t"
	  ,pfx "*--" ,new-frame " = stack_pointer[0];\n\t"
	  ,pfx "stack_pointer = " ,new-frame ";\n\t"))
    (else
     (LAP ,pfx "{\n\t  SCHEME_OBJECT * frame_top = &stack_pointer["
	  ,frame-size "];\n\t"
	  ,pfx "SCHEME_OBJECT * new_frame = " ,new-frame ";\n\t"
	  ,pfx "  long frame_size = " ,frame-size ";\n\t"
	  ,pfx "  while ((--frame_size) >= 0)"
	  ,pfx "    *--new_frame = *--frame_top;\n\t"
	  ,pfx "  stack_pointer = new_frame;\n\t"
	  ,pfx "}\n\t"))))

;;; DYNAMIC-LINK instructions have a <frame-size>, <new frame end>,
;;; and <current dynamic link> as arguments.  They pop the stack by
;;; removing the lesser of the amount needed to move the stack pointer
;;; back to the <new frame end> or <current dynamic link>.  The last
;;; <frame-size> words on the stack (the stack frame for the procedure
;;; about to be called) are then put back onto the newly adjusted
;;; stack.

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (REGISTER (? choice-1))
				  (REGISTER (? choice-2)))
  (let ((choice-1 (standard-source! choice-1 'SCHEME_OBJECT*))
	(choice-2 (standard-source! choice-2 'SCHEME_OBJECT*)))
    (LAP "{\n\t  SCHEME_OBJECT * new_frame;\n\t"
	 "  new_frame = ((" ,choice-1 " <= " ,choice-2 ") ? "
	 ,choice-1 " : " ,choice-2 ");\n\t"
	 ,@(move-frame-up frame-size "new_frame" "  ")
	 "}\n\t")))

;;; Entry point types

(define-integrable (make-code-word min max)
  (+ (* #x100 min) max))

(define (make-procedure-code-word min max)
  ;; The "min" byte must be less than #x80; the "max" byte may not
  ;; equal #x80 but can take on any other value.
  (if (or (negative? min) (>= min #x80))
      (error "MAKE-PROCEDURE-CODE-WORD: minimum out of range" min))
  (if (>= (abs max) #x80)
      (error "MAKE-PROCEDURE-CODE-WORD: maximum out of range" max))
  (make-code-word min (if (negative? max) (+ #x100 max) max)))

(define expression-code-word
  (make-code-word #xff #xff))

(define internal-entry-code-word
  (make-code-word #xff #xfe))

(define internal-continuation-code-word
  (make-code-word #xff #xfc))

(define (continuation-code-word label)
  (frame-size->code-word
   (if label
       (rtl-continuation/next-continuation-offset (label->object label))
       0)
   internal-continuation-code-word))

(define (internal-procedure-code-word rtl-proc)
  ;; represented as return addresses so the debugger will
  ;; not barf when it sees them (on the stack if interrupted).
  (frame-size->code-word
   (rtl-procedure/next-continuation-offset rtl-proc)
   internal-entry-code-word))

(define (frame-size->code-word offset default)
  (cond ((not offset)
	 default)
	((< offset #x2000)
	 ;; This uses up through (#xff #xdf).
	 (let ((qr (integer-divide offset #x80)))
	   (make-code-word (+ #x80 (integer-divide-remainder qr))
			   (+ #x80 (integer-divide-quotient qr)))))
	(else
	 (error "Unable to encode continuation offset" offset))))

;;;; Procedure headers

;;; The following calls MUST appear as the first thing at the entry
;;; point of a procedure.  They assume that the register map is clear
;;; and that no register contains anything of value.
;;;
;;; The only reason that this is true is that no register is live
;;; across calls.  If that were not true, then we would have to save
;;; any such registers on the stack so that they would be GC'ed
;;; appropriately.
;;;
;;; The only exception is the dynamic link register, handled
;;; specially.  Procedures that require a dynamic link use a different
;;; interrupt handler that saves and restores the dynamic link
;;; register.

(define (simple-procedure-header code-word label e-label code)
  (declare-block-label! code-word label e-label)
  (let ((block-label (label->offset label)))
    (use-interrupt-check!)
    (LAP ,@(if (not e-label)
	       (LAP)
	       (label-statement e-label))
	 ,@(label-statement label)
	 "INTERRUPT_CHECK ("  ,code  ", (" ,block-label "));\n\t")))

(define (dlink-procedure-header code-word label e-label)
  (declare-block-label! code-word label e-label)
  (let ((block-label (label->offset label)))
    (use-dlink-interrupt-check!)
    (LAP ,@(if (not e-label)
	       (LAP)
	       (label-statement e-label))
	 ,@(label-statement label)
	 "DLINK_INTERRUPT_CHECK ("
	 ,code:compiler-interrupt-dlink
	 ", ("  ,block-label "));\n\t")))

(define-rule statement
  (CONTINUATION-ENTRY (? internal-label))
  (declare-block-label! (continuation-code-word internal-label)
			internal-label #f)
  (label-statement internal-label))

(define-rule statement
  (CONTINUATION-HEADER (? internal-label))
  (simple-procedure-header (continuation-code-word internal-label)
			   internal-label
			   #f
			   code:compiler-interrupt-continuation))

(define-rule statement
  (IC-PROCEDURE-HEADER (? internal-label))
  (simple-procedure-header expression-code-word
			   internal-label
			   (rtl-procedure/external-label
			    (label->object internal-label))
			   code:compiler-interrupt-ic-procedure))

(define-rule statement
  (OPEN-PROCEDURE-HEADER (? internal-label))
  (let* ((rtl-proc (label->object internal-label))
	 (external-label (rtl-procedure/external-label rtl-proc)))
    ((if (rtl-procedure/dynamic-link? rtl-proc)
	 dlink-procedure-header 
	 (lambda (code-word label external-label)
	   (simple-procedure-header code-word label external-label
				    code:compiler-interrupt-procedure)))
     (internal-procedure-code-word rtl-proc)
     internal-label external-label)))

(define-rule statement
  (PROCEDURE-HEADER (? internal-label) (? min) (? max))
  (simple-procedure-header (make-procedure-code-word min max)
			   internal-label
			   (rtl-procedure/external-label
			    (label->object internal-label))
			   code:compiler-interrupt-procedure))

;;;; Closures.

;; Magic for compiled entries.

(define-integrable (label-statement label)
  (lap:make-label-statement label))

(define-rule statement
  (CLOSURE-HEADER (? internal-label) (? nentries) (? entry))
  entry
  (if (zero? nentries)
      (error "Closure header for closure with no entries!"
	     internal-label))
  (let ((rtl-proc (label->object internal-label)))
    (let ((external-label (rtl-procedure/external-label rtl-proc)))
      (declare-block-label! (internal-procedure-code-word rtl-proc)
			    #f external-label)
      (use-closure-interrupt-check!)
      (LAP ,@(label-statement external-label)
	   "CLOSURE_HEADER (" ,(label->offset external-label) ");\n\t"
	   ,@(label-statement internal-label)
	   "CLOSURE_INTERRUPT_CHECK ("
	   ,(number->string code:compiler-interrupt-closure)
	   ");\n\t"))))

(define (build-gc-offset-word offset code-word)
  (let ((encoded-offset (quotient offset 2)))
    (if (eq? endianness 'LITTLE)
	(+ (* encoded-offset #x10000) code-word)
	(+ (* code-word #x10000) encoded-offset))))

(define (write-closure-entry internal-label min max offset)
  (let ((external-label
	 (rtl-procedure/external-label (label->object internal-label))))
    (LAP "WRITE_LABEL_DESCRIPTOR (free_pointer, 0x"
	 ,(number->string (make-procedure-code-word min max) 16) ", "
	 ,offset ");\n\t"
	 "free_pointer[0] = (MAKE_LABEL_WORD (current_C_proc, "
	 ,(label->dispatch-tag external-label)
	 "));\n\t"
	 "free_pointer[1] = ((SCHEME_OBJECT) (&current_block["
	 ,(label->offset external-label) "]));\n\t")))

(define (cons-closure target label min max nvars)
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP "* free_pointer = (MAKE_OBJECT (" ,(ucode-type manifest-closure) ", "
	 ,(+ closure-entry-size nvars) "));\n\t"
	 "free_pointer += 2;\n\t"
	 ,target " = free_pointer;\n\t"
	 ,@(write-closure-entry label min max 2)
	 "free_pointer += " ,(+ nvars 2) ";\n\t")))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
			(? min) (? max) (? nvars)))
  (cons-closure target procedure-label min max nvars))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-MULTICLOSURE (? nentries) (? nvars) (? entries)))
  ;; entries is a vector of all the entry points
  (case nentries
    ((0)
     (let ((dest (standard-target! target 'SCHEME_OBJECT*)))
       (LAP ,dest " = free_pointer;\n\t"
	    "*free_pointer = (MAKE_OBJECT (" ,(ucode-type manifest-vector)
	    ", " ,nvars "));\n\t"
	    "free_pointer += " ,(+ nvars 1) ";\n\t")))
    ((1)
     (let ((entry (vector-ref entries 0)))
       (cons-closure target (car entry) (cadr entry) (caddr entry) nvars)))
    (else
     (cons-multiclosure target nentries nvars (vector->list entries)))))

(define (cons-multiclosure target nentries nvars entries)
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP "* free_pointer = (MAKE_OBJECT (" ,(ucode-type manifest-closure) ", "
	 ,(1+ (+ (* nentries closure-entry-size) nvars)) "));\n\t"
	 "free_pointer += 2;\n\t"
	 "WRITE_LABEL_DESCRIPTOR (free_pointer, " ,nentries ", 0);\n\t"
	 "free_pointer += 1;\n\t"
	 ,target " = free_pointer;\n\t"
	 ,@(reduce-right
	    (lambda (lap1 lap2)
	      (LAP ,@lap1 ,@lap2))
	    (LAP)
	    (map (lambda (entry offset)
		   (let ((label (car entry))
			 (min (cadr entry))
			 (max (caddr entry)))
		     (LAP ,@(write-closure-entry label min max offset)
			  "free_pointer += 3;\n\t")))
		 entries (make-multiclosure-offsets nentries)))
	 "free_pointer += " ,(- nvars 1) ";\n\t")))
	 
(define (make-multiclosure-offsets nentries)
  (let generate ((x nentries)
		 (offset 3))
    (if (= 0 x)
	'()
	(cons offset
	      (generate (-1+ x)
			(+ offset closure-entry-size))))))

;;;; Entry Header
;;; This is invoked by the top level of the LAP generator.

(define (generate/quotation-header environment-label
				   free-ref-offset n-sections)
  (let ((label (generate-label)))
    (declare-block-label! (continuation-code-word false) false label)
    (use-invoke-interface! 4)
    (LAP "current_block[" ,environment-label
	 "] = register_block[REGBLOCK_ENV];\n\t"
	 "INVOKE_INTERFACE_4 (" ,code:compiler-link
	 ", &current_block[" ,(label->offset label) "]"
	 ",\n\t\t\t\tcurrent_block"
	 ",\n\t\t\t\t&current_block[" ,free-ref-offset "]"
	 ",\n\t\t\t\t" ,n-sections ");\n\t"
	 ,@(label-statement label))))

(define (generate/remote-link code-block-label
			      environment-offset
			      free-ref-offset
			      n-sections)
  (let ((label (generate-label)))
    (add-remote-link! code-block-label)
    (declare-block-label! (continuation-code-word false) false label)
    (use-invoke-interface! 4)
    (LAP "{\n\t  SCHEME_OBJECT * subblock = (OBJECT_ADDRESS (current_block["
	 ,code-block-label "]));\n\t  "
	 "subblock[" ,environment-offset
	 "] = register_block[REGBLOCK_ENV];\n\t  "
	 "INVOKE_INTERFACE_4 (" ,code:compiler-link
	 ", &current_block[" ,(label->offset label) "]"
	 ",\n\t\t\t\t  subblock"
	 ",\n\t\t\t\t  &subblock[" ,free-ref-offset "]"
	 ",\n\t\t\t\t"  ,n-sections ");\n\t}\n\t"
	 ,@(label-statement label))))

(define (add-remote-link! label)
  (if (not *purification-root-object*)
      (set! *purification-root-object*
	    (cons *purification-root-marker* '())))
  (set-cdr! *purification-root-object*
	    (cons (object-label-value label)
		  (cdr *purification-root-object*)))
  unspecific)

(define *purification-root-marker*
  (intern "#[PURIFICATION-ROOT]"))

(define (generate/remote-links n-code-blocks code-blocks-label n-sections)
  (define-integrable max-line-width 80)

  (define (sections->c-sections mul? posn n-sections)
    (cond ((not (null? n-sections))
	   (let* ((val (number->string (car n-sections)))
		  (next (+ posn (+ 2 (string-length val)))))
	     (if (>= (1+ next) max-line-width)
		 (LAP ",\n\t\t" ,val
		      ,@(sections->c-sections true
					      (+ 16 (string-length val))
					      (cdr n-sections)))
		 (LAP ", " ,val
		      ,@(sections->c-sections mul? next (cdr n-sections))))))
	  ((or mul? (>= (+ posn 2) max-line-width))
	   (LAP "\n\t      "))
	  (else
	   (LAP))))

  (let ((label (generate-label))
	(done (generate-label)))
    (set! *purification-root-object*
	  (cons *purification-root-marker*
		(object-label-value code-blocks-label)))
    (declare-block-label! (continuation-code-word false) false label)
    (use-invoke-interface! 4)
    (LAP "*--stack_pointer = (LONG_TO_UNSIGNED_FIXNUM (1L));\n\t"
	 ,@(label-statement label)
	 "{\n\t  "
	 "static CONST short sections []\n\t    = {\t0"
	 ,@(sections->c-sections false 17 (vector->list n-sections))
	 "};\n\t  "
	 "long counter = (OBJECT_DATUM (* stack_pointer));\n\t  "
	 "SCHEME_OBJECT blocks, * subblock;\n\t  "
	 "short section;\n\t\n\t  "
	 "if (counter > " ,n-code-blocks "L)\n\t    goto " ,done ";\n\t  "
	 "blocks = current_block[" ,code-blocks-label "];\n\t  "
	 "subblock = (OBJECT_ADDRESS (MEMORY_REF (blocks, counter)));\n\t  "
	 "subblock[(OBJECT_DATUM (subblock[0]))]\n\t  "
	 "  = register_block[REGBLOCK_ENV];\n\t  "
	 "section = sections[counter];\n\t  "
	 "counter += 1;\n\t  "
	 "*stack_pointer = (LONG_TO_UNSIGNED_FIXNUM (counter));\n\t  "
	 "INVOKE_INTERFACE_4 (" ,code:compiler-link
	 ", &current_block[" ,(label->offset label) "]"
	 ",\n\t\t\t\t  subblock"
	 ",\n\t\t\t\t  (subblock"
	 "\n\t\t\t\t   + (2 + (OBJECT_DATUM (subblock[1]))))"
	 ",\n\t\t\t\t  section);\n\t}\n\t"
	 ,@(label-statement done)
	 "stack_pointer += 1;\n\t")))

#|
(define (generate/constants-block constants references assignments uuo-links
				  global-links static-vars)
  (let ((constant-info
	 (declare-constants 0 (transmogrifly uuo-links)
	   (declare-constants 1 references
	     (declare-constants 2 assignments
	       (declare-constants 3 (transmogrifly global-links)
		 (declare-constants false
		     (map (lambda (pair)
			    (cons false (cdr pair)))
			  static-vars)
		   (declare-constants false constants
		     (cons false (LAP))))))))))
    (let ((free-ref-label (car constant-info))
	  (constants-code (cdr constant-info))
	  (debugging-information-label (allocate-constant-label))
	  (environment-label (allocate-constant-label))
	  (n-sections
	   (+ (if (null? uuo-links) 0 1)
	      (if (null? references) 0 1)
	      (if (null? assignments) 0 1)
	      (if (null? global-links) 0 1))))
      (values
       (LAP ,@constants-code
	    ;; Place holder for the debugging info filename
	    (SCHEME-OBJECT ,debugging-information-label DEBUGGING-INFO)
	    ;; Place holder for the load time environment if needed
	    (SCHEME-OBJECT ,environment-label
			   ,(if (null? free-ref-label) 0 'ENVIRONMENT)))
       environment-label
       free-ref-label
       n-sections))))

(define (declare-constants tag constants info)
  (define (inner constants)
    (if (null? constants)
	(cdr info)
	(let ((entry (car constants)))
	  (LAP (SCHEME-OBJECT ,(cdr entry) ,(car entry))
	       ,@(inner (cdr constants))))))
  (if (and tag (not (null? constants)))
      (let ((label (allocate-constant-label)))
	(cons label
	      (inner
	       `((,(let ((datum (length constants)))
		     (if (> datum #xffff)
			 (error "datum too large" datum))
		     (+ (* tag #x10000) datum))
		  . ,label)
		 ,@constants))))
      (cons (car info) (inner constants))))

(define (transmogrifly uuos)
  (define (inner name assoc)
    (if (null? assoc)
	(transmogrifly (cdr uuos))
	;; produces ((name . label) (0 . label) ... (frame-size . label) ...)
        ;; where the (0 . label) is repeated to fill out the size required
        ;; as specified in machin.scm
	`((,name . ,(cdar assoc))		; uuo-label
	  (,(caar assoc) .			; frame-size
			 ,(allocate-constant-label))
	  ,@(inner name (cdr assoc)))))
  (if (null? uuos)
      '()
      ;; caar is name, cdar is alist of frame sizes
      (inner (caar uuos) (cdar uuos))))
|#

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
