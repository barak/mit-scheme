#| -*-Scheme-*-

$Id: asstop.scm,v 1.1 1992/10/19 19:11:52 jinx Exp $

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

;;;; Assembler and Linker top level
;;; package: (compiler top-level)

(declare (usual-integrations))

;;;; Exports to the compiler

(define (compiler-file-output object pathname)
  (fasdump object pathname))

(define (compiled-scode->procedure scode environment)
  (scode-eval scode environment))

;;; State variables for the assembler and linker

;; First set: phase/rtl-generation
;; Last used: phase/link
(define *block-label*)

;; First set: phase/lap-generation
;; Last used: phase/info-generation-2
(define *external-labels*)

;; First set: phase/assemble
;; Last used: phase/link
(define *label-bindings*)
(define *code-vector*)
(define *entry-points*)

;; First set: phase/link
;; Last used: result of compilation
(define *result*)

(define (assemble&link info-output-pathname)
  (phase/assemble)
  (if info-output-pathname
      (phase/info-generation-2 info-output-pathname))
  (phase/link)
  *result*)

(define (wrap-lap entry-label some-lap)
  (LAP ,@(if *procedure-result?*
	     (LAP (ENTRY-POINT ,entry-label))
	     (lap:make-entry-point entry-label *block-label*))
       ,@some-lap))

(define (bind-assembler&linker-variables thunk)
  (fluid-let ((*block-label*)
	      (*external-labels*)
	      (*end-of-block-code*)
	      (*next-constant*)
	      (*interned-constants*)
	      (*interned-variables*)
	      (*interned-assignments*)
	      (*interned-uuo-links*)
	      (*interned-global-links*)
	      (*interned-static-variables*)
	      (*label-bindings*)
	      (*code-vector*)
	      (*entry-points*)
	      (*result*))
    (thunk)))

(define (assembler&linker-reset!)
  (set! *block-label*)
  (set! *external-labels*)
  (set! *end-of-block-code*)
  (set! *next-constant*)
  (set! *interned-constants*)
  (set! *interned-variables*)
  (set! *interned-assignments*)
  (set! *interned-uuo-links*)
  (set! *interned-global-links*)
  (set! *interned-static-variables*)
  (set! *label-bindings*)
  (set! *code-vector*)
  (set! *entry-points*)
  (set! *result*)
  unspecific)

(define (initialize-back-end!)
  (set! *block-label* (generate-label))
  (set! *external-labels* '())
  (set! *end-of-block-code* (LAP))
  (set! *next-constant* 0)
  (set! *interned-constants* '())
  (set! *interned-variables* '())
  (set! *interned-assignments* '())
  (set! *interned-uuo-links* '())
  (set! *interned-global-links* '())
  (set! *interned-static-variables* '())
  unspecific)

;;;; Assembler and linker

(define (phase/assemble)
  (compiler-phase
   "Assembly"
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

(define (phase/link)
  (compiler-phase
   "Linkification"
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

;;;; Dumping the assembler's symbol table to the debugging file...

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

(define (recursive-compilation-results)
  (sort *recursive-compilation-results*
	(lambda (x y)
	  (< (vector-ref x 0)
	     (vector-ref y 0)))))

;;; Various ways of dumping an info file

(define (compiler:dump-inf-file binf pathname)
  (fasdump binf pathname true)
  (announce-info-files pathname))

(define (compiler:dump-bif/bsm-files binf pathname)
  (let ((bif-path (pathname-new-type pathname "bif"))
	(bsm-path (pathname-new-type pathname "bsm")))
    (let ((bsm (split-inf-structure! binf bsm-path)))
      (fasdump binf bif-path true)
      (fasdump bsm bsm-path true))
    (announce-info-files bif-path bsm-path)))
  
(define (compiler:dump-bci/bcs-files binf pathname)
  (load-option 'COMPRESS)
  (let ((bci-path (pathname-new-type pathname "bci"))
	(bcs-path (pathname-new-type pathname "bcs")))
    (let ((bsm (split-inf-structure! binf bcs-path)))
      (call-with-temporary-filename
	(lambda (bif-name)
	  (fasdump binf bif-name true)
	  (compress bif-name bci-path)))
      (call-with-temporary-filename
	(lambda (bsm-name)
	  (fasdump bsm bsm-name true)
	  (compress bsm-name bcs-path))))
    (announce-info-files bci-path bcs-path)))
  
(define (compiler:dump-bci-file binf pathname)
  (load-option 'COMPRESS)
  (let ((bci-path (pathname-new-type pathname "bci")))
    (split-inf-structure! binf false)
    (call-with-temporary-filename
      (lambda (bif-name)
	(fasdump binf bif-name true)
	(compress bif-name bci-path)))
    (announce-info-files bci-path)))

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

(define compiler:dump-info-file
  compiler:dump-bci-file)

;;;; LAP->CODE
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