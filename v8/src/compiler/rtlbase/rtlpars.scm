#| -*-Scheme-*-

$Id: rtlpars.scm,v 1.4 1999/01/02 02:52:56 cph Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

;;;; RTL parser
;;; package: (compiler rtl-parser)

(declare (usual-integrations))

(define label-like-statements
  '(LABEL RETURN-ADDRESS PROCEDURE TRIVIAL-CLOSURE CLOSURE EXPRESSION))

(define jump-like-statements
  ;; JUMPC is special.
  ;; Also missing some other INVOCATION:s and INTERPRETER-CALL:s
  ;; but the new compiler never uses them.
  '(JUMP
    POP-RETURN INVOCATION:NEW-APPLY
    INVOCATION:REGISTER INVOCATION:PROCEDURE
    INVOCATION:UUO-LINK INVOCATION:GLOBAL-LINK
    INVOCATION:PRIMITIVE INVOCATION:APPLY
    INVOCATION:SPECIAL-PRIMITIVE
    INTERPRETER-CALL:CACHE-REFERENCE
    INTERPRETER-CALL:CACHE-ASSIGNMENT))

(define (internal-error message . more)
  (apply error "rtl->rtl-graph internal error:" message more))

(define *rgraphs*)
(define *expressions*)
(define *procedures*)
(define *continuations*)

(define (rtl->rtl-graph rtl-program)
  ;; (values expression procedures continuations rgraphs)
  (fluid-let ((*rgraphs* '())
	      (*expressions* '())
	      (*procedures* '())
	      (*continuations* '()))
    (let ((labels->segments (parse-rtl rtl-program)))
      (hash-table/for-each labels->segments reformat!)
      (hash-table/for-each labels->segments
			   (lambda (label slot)
			     label	; ignored
			     (link-up! slot labels->segments)))
      (hash-table/for-each labels->segments rgraphify!/1)
      (hash-table/for-each labels->segments rgraphify!/2)
      (hash-table/for-each labels->segments rgraphify!/3)
      (values (cond ((null? *expressions*)
		     (if *procedure-result?*
			 false
			 (internal-error "No expression found")))
		    ((not (null? (cdr *expressions*)))
		     (internal-error "Too many expressions found"))
		    (else
		     (car *expressions*)))
	      *procedures*
	      *continuations*
	      *rgraphs*))))

;;; The following procedures solve a union/find problem.
;;; They use the bblock-live-at-entry field temporarily to associate
;;; a bblock with its set.  The field is cleared at the end.

(define (rgraphify!/1 label slot)
  label					; ignored
  (if (not (eq? (car slot) 'EMPTY))
      (let ((bblock (caddr slot)))
	(set-bblock-live-at-entry! bblock (list false bblock)))))

(define (rgraphify!/2 label slot)
  label					; ignored
  (if (not (eq? (car slot) 'EMPTY))
      (let* ((bblock (caddr slot))
	     (set (bblock-live-at-entry bblock))
	     (to-bash set)
	     (unify!
	      (lambda (bblock*)
		(let ((set* (bblock-live-at-entry bblock*)))
		  (if (not (eq? set* set))
		      (let ((set** (cdr set*)))
			(for-each (lambda (bblock**)
				    (set-bblock-live-at-entry! bblock** set))
				  set**)
			(append! to-bash set**)
			(set! to-bash set**)))))))
	(for-each (lambda (edge)
		    (unify! (edge-left-node edge)))
		  (node-previous-edges bblock)))))

(define (rgraphify!/3 label slot)
  label					; ignored
  (if (not (eq? (car slot) 'EMPTY))
      (let* ((bblock (caddr slot))
	     (set (bblock-live-at-entry bblock)))
	(if (not (car set))
	    (set-car! set (->rgraph (cdr set))))
	(classify! bblock (car set))
	(set-bblock-live-at-entry! bblock false))))

(define (->rgraph bblocks)
  (let* ((max-reg
	  (fold-right (lambda (bblock max-reg)
			(max (bblock->max-reg bblock)
			     max-reg))
		      (- number-of-machine-registers 1)
		      bblocks))
	 (rgraph (make-rgraph (+ max-reg 1))))
    (set-rgraph-bblocks! rgraph bblocks)
    (set! *rgraphs* (cons rgraph *rgraphs*))
    rgraph))

(define (bblock->max-reg bblock)
  (let loop ((insts (bblock-instructions bblock))
	     (max-reg -1))
    (if (not insts)
	max-reg
	(loop (rinst-next insts)
	      (max max-reg
		   (let walk ((rtl (rinst-rtl insts)))
		     (cond ((not (pair? rtl))
			    max-reg)
			   ((eq? (car rtl) 'REGISTER)
			    (cadr rtl))
			   ((eq? (car rtl) 'CONSTANT)
			    max-reg)
			   (else
			    (max (walk (car rtl)) (walk (cdr rtl)))))))))))

(define (reformat! label slot)
  (define (->rinsts stmts)
    (let loop ((stmts stmts)
	       (next false))
      (if (null? stmts)
	  next
	  (loop (cdr stmts)
		(make-rtl-instruction* (car stmts) next)))))

  (let* ((stmts (cadr slot))
	 (result
	  (cond ((null? stmts)
		 (internal-error "Null segment" label))
		((not (eq? (caar stmts) 'JUMP))
		 (list (let ((stmt (car stmts)))
			 (cond ((eq? (car stmt) 'INVOCATION:SPECIAL-PRIMITIVE)
				(caddr stmt))
			       ((memq (car stmt)
				      '(INTERPRETER-CALL:CACHE-REFERENCE
					INTERPRETER-CALL:CACHE-ASSIGNMENT))
				(cadr stmt))
			       (else
				false)))
		       (make-sblock (->rinsts stmts))))
		((and (not (null? (cdr stmts)))
		      (eq? (car (cadr stmts)) 'JUMPC))
		 (let ((jump-inst (car stmts))
		       (jumpc-inst (cadr stmts)))
		   (let ((jump-label (cadr jump-inst))
			 (jumpc-label (caddr jumpc-inst))
			 (predicate (cadr jumpc-inst))
			 (finish
			  (lambda (predicate preference trueb falseb)
			    (list (list trueb falseb)
				  (pnode/prefer-branch!
				   (make-pblock
				    (->rinsts (cons predicate (cddr stmts))))
				   preference)))))
		     (cond ((not (pair? predicate))
			    (finish predicate
				    'NEITHER
				    jumpc-label
				    jump-label))
			   ((eq? 'UNPREDICTABLE (car predicate))
			    (finish (cadr predicate)
				    'NEITHER
				    jumpc-label
				    jump-label))
			   ((eq? 'NOT (car predicate))
			    (finish (cadr predicate)
				    'ALTERNATIVE
				    jump-label
				    jumpc-label))
			   (else
			    (finish predicate
				    'CONSEQUENT
				    jumpc-label
				    jump-label))))))
		(else
		 (list (cadr (car stmts))
		       (make-sblock (->rinsts (cdr stmts))))))))
    (set-car! slot
	      (if (bblock-instructions (cadr result))
		  'BBLOCK
		  'EMPTY))
    (set-cdr! slot result)
    #| (set-bblock-label! (cadr result) label) |#
    unspecific))

(define (link-up! slot labels->segments)
  (define (find-bblock label seen)
    (let ((desc (hash-table/get labels->segments label false)))
      (if (not desc)
	  (internal-error "Missing label" label))
      (if (and (eq? (car desc) 'EMPTY)
	       (not (memq label seen)))
	  (find-bblock (cadr desc) (cons label seen))
	  (caddr desc))))

  (if (not (eq? (car slot) 'EMPTY))
      (let ((next (cadr slot))
	    (bblock (caddr slot)))
	(cond ((not next))
	      ((not (pair? next))
	       (create-edge! bblock
			     set-snode-next-edge!
			     (find-bblock next '())))
	      (else
	       (create-edge! bblock
			     set-pnode-consequent-edge!
			     (find-bblock (car next) '()))
	       (create-edge! bblock
			     set-pnode-alternative-edge!
			     (find-bblock (cadr next) '())))))))

(define-macro (%push! object collection)
  `(begin (set! ,collection (cons ,object ,collection))
	  unspecific))

(define (classify! bblock rgraph)
  ;; Most of the fields are meaningless for the new headers
  ;; since the information is explicit in the RTL (e.g. INTERRUPT-CHECK:)
  (let* ((gen-edge
	  (lambda ()
	    (let ((edge (create-edge! false false bblock)))
	      (add-rgraph-entry-edge! rgraph edge)
	      edge)))
	 (insts (bblock-instructions bblock))
	 (rtl (rinst-rtl insts)))
    (case (car rtl)
      ((RETURN-ADDRESS)
       (%push! (let ((label (cadr rtl))
		     (debinfo (caddr rtl)))
		 (make-rtl-continuation
		  rgraph		; rgraph
		  label			; label
		  (gen-edge)		; entry edge
		  false			; next continuation offset
		  debinfo		; debugging info
		  ))
	       *continuations*))
      ((PROCEDURE CLOSURE TRIVIAL-CLOSURE)
       (let* ((label (cadr rtl))
	      (debinfo (caddr rtl))
	      (proc
	       (make-rtl-procedure
		rgraph			; rgraph
		label			; label
		(gen-edge)		; entry edge
		label			; name
		false			; nrequired
		false			; noptional
		false			; rest
		(not (eq? (car rtl) 'PROCEDURE)) ; closure?
		false			; dynamic link?
		(car rtl)		; type
		debinfo			; debugging info
		false			; next continuation offset
		false			; stack leaf?
		)))
	 (set-rtl-procedure/%external-label! proc label)
	 (%push! proc *procedures*)))
      ((EXPRESSION)
       (%push! (let ((label (cadr rtl))
		     (debinfo (caddr rtl)))
		 (make-rtl-expr
		  rgraph		; rgraph
		  label			; label
		  (gen-edge)		; entry edge
		  debinfo		; debugging info
		  ))
	       *expressions*)))))

(define make-labels->segments
  (let ((symbol-hash-mod
	 (lambda (symbol modulus)
	   (string-hash-mod (symbol-name symbol) modulus))))
    (strong-hash-table/constructor symbol-hash-mod eq?)))

(define (parse-rtl rtl-program)
  (cond ((null? rtl-program)
	 (internal-error "Empty program"))
	((not (memq (caar rtl-program) label-like-statements))
	 (internal-error "Program does not start with label" rtl-program)))
  (let ((labels->segments (make-labels->segments)))

    (define (found-one label stmts)
      (hash-table/put! labels->segments
		       label
		       (list 'STATEMENTS stmts)))

    (define (profile-count rtl-program)
      (if compiler:generate-profiling-instructions?
	  (cons '(PROFILE-COUNT) rtl-program)
	  rtl-program))
	
    (let loop ((program (cdr rtl-program))
	       (label   (cadr (car rtl-program)))
	       (segment (if (eq? (caar rtl-program) 'LABEL)
			    '()
			    (list (car rtl-program))))
	       (count-needed? #T))
      (if (null? program)
	  (begin
	    (if (not (null? segment))
		(internal-error "Last segment falls through"
				(reverse segment)))))
      (let ((stmt (car program)))
	(cond ((memq (car stmt) jump-like-statements)
	       (found-one label
			  (cons stmt (if count-needed?
					 (profile-count segment)
					 segment)))
	       (if (not (null? (cdr program)))
		   (let ((next (cadr program)))
		     (if (not (memq (car next) label-like-statements))
			 (internal-error "No label following jump"
					 program))
		     (loop (cddr program)
			   (cadr next)
			   (if (eq? (car next) 'LABEL)
			       '()
			       (list next))
			   #T))))
	      ((eq? (car stmt) 'JUMPC)
	       (if (null? (cdr program))
		   (internal-error "Last segment falls through when false"
				   (reverse (cons stmt segment))))
	       (let ((next (cadr program)))
		 (if (eq? 'JUMP (car next))
		     (loop (cdr program)
			   label
			   (cons stmt (profile-count segment))
			   #F)
		     (let ((label (generate-label)))
		       (loop (cons `(LABEL ,label) (cdr program))
			     label
			     (cons stmt (profile-count segment))
			     #T)))))
	      ((memq (car stmt) label-like-statements)
	       (if (not (eq? (car stmt) 'LABEL))
		   (internal-error "Falling through to non-label label"
				   (car stmt)))
	       (found-one label (cons `(JUMP ,(cadr stmt)) 
				      (profile-count segment)))
	       (loop (cdr program)
		     (cadr stmt)
		     '()
		     #T))
	      (else
	       (loop (cdr program)
		     label
		     (cons stmt segment)
		     count-needed?)))))
    labels->segments))