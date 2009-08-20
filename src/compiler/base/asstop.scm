#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Assembler and Linker top level
;;; package: (compiler top-level)

(declare (usual-integrations))

;;;; Exports to the compiler

(define (compiler:compiled-code-pathname-type)
  (if compiler:cross-compiling? "moc" "com"))

(define (compiler-file-output object pathname)
  (fasdump object pathname #t))

(define (compiler-output->procedure scode environment)
  (scode-eval scode environment))

(define (compiler-output->compiled-expression cexp)
  cexp)

(define (compile-scode/file/hook input-pathname output-pathname action)
  input-pathname output-pathname
  (action))

(define (compile-scode/no-file/hook action)
  (action))

(define (compile-scode/recursive/hook action)
  (action))

;;; Global variables for the assembler and linker

(define *recursive-compilation-results*)

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

(define (bind-assembler&linker-top-level-variables thunk)
  (fluid-let ((*recursive-compilation-results* '()))
    (thunk)))

(define (bind-assembler&linker-variables thunk)
  (fluid-let ((*block-associations*)
	      (*block-label*)
	      (*external-labels*)
	      (*end-of-block-code*)
	      (*next-constant*)
	      (*interned-assignments*)
	      (*interned-constants*)
	      (*interned-global-links*)
	      (*interned-static-variables*)
	      (*interned-uuo-links*)
	      (*interned-variables*)
	      (*label-bindings*)
	      (*code-vector*)
	      (*entry-points*)
	      (*result*))
    (thunk)))

(define (assembler&linker-reset!)
  (set! *recursive-compilation-results* '())
  (set! *block-associations*)
  (set! *block-label*)
  (set! *external-labels*)
  (set! *end-of-block-code*)
  (set! *next-constant*)
  (set! *interned-assignments*)
  (set! *interned-constants*)
  (set! *interned-global-links*)
  (set! *interned-static-variables*)
  (set! *interned-uuo-links*)
  (set! *interned-variables*)
  (set! *label-bindings*)
  (set! *code-vector*)
  (set! *entry-points*)
  (set! *result*)
  unspecific)

(define (initialize-back-end!)
  (set! *block-associations* '())
  (set! *block-label* (generate-label))
  (set! *external-labels* '())
  (set! *end-of-block-code* '())
  (set! *next-constant* 0)
  (set! *interned-assignments* '())
  (set! *interned-constants* '())
  (set! *interned-global-links* '())
  (set! *interned-static-variables* '())
  (set! *interned-uuo-links* '())
  (set! *interned-variables* '())
  unspecific)

;;;; Assembler and linker

(define (phase/assemble)
  (compiler-phase
   "Assembly"
   (lambda ()
     (receive (count code-vector labels bindings)
	 (assemble *block-label* (last-reference *lap*))
       (set! *code-vector* code-vector)
       (set! *entry-points* labels)
       (set! *label-bindings* bindings)
       (if compiler:show-phases?
	   (write-notification-line
	    (lambda (port)
	      (write-string "Branch tensioning done in " port)
	      (write (+ count 1) port)
	      (write-string " iteration" port)
	      (if (> count 0) (write-string "s" port))
	      (write-string "." port))))))))

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
     ((ucode-primitive declare-compiled-code-block 1) *code-vector*)
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
		(vector 'DEBUGGING-INFO-WRAPPER
			2
			#f
			#f
			#f
			info))
	       ((eq? pathname 'RECURSIVE) ; recursive compilation
		(set! *recursive-compilation-results*
		      (cons (vector *recursive-compilation-number*
				    info
				    *code-vector*
				    *tl-bound*
				    *tl-free*)
			    *recursive-compilation-results*))
		(vector 'DEBUGGING-INFO-WRAPPER
			2
			*debugging-key*
			*info-output-filename*
			*recursive-compilation-number*
			#f))
	       (else
		(compiler:dump-info-file
		 (vector 'DEBUGGING-FILE-WRAPPER
			 2
			 *debugging-key*
			 (list->vector
			  (cons info
				(map (lambda (other) (vector-ref other 1))
				     (recursive-compilation-results)))))
		 pathname)
		(vector 'DEBUGGING-INFO-WRAPPER
			2
			*debugging-key*
			*info-output-filename*
			0
			#f))))))))

(define (recursive-compilation-results)
  (sort *recursive-compilation-results*
    (lambda (x y)
      (fix:< (vector-ref x 0) (vector-ref y 0)))))

;;; Various ways of dumping an info file

(define (compiler:dump-inf-file binf pathname)
  (compiler-file-output binf pathname))

(define (compiler:dump-bif/bsm-files binf pathname)
  (let ((bif-path (pathname-new-type pathname "bif"))
	(bsm-path (pathname-new-type pathname "bsm")))
    (let ((bsm (split-inf-structure! binf bsm-path)))
      (compiler-file-output binf bif-path)
      (compiler-file-output bsm bsm-path))))
  
(define (compiler:dump-bci/bcs-files binf pathname)
  (let ((bci-path (pathname-new-type pathname "bci"))
	(bcs-path (pathname-new-type pathname "bcs")))
    (let ((bsm (split-inf-structure! binf bcs-path)))
      (dump-compressed binf bci-path)
      (dump-compressed bsm bcs-path))))

(define (compiler:dump-bci-file binf pathname)
  (let ((bci-path (pathname-new-type pathname "bci")))
    (split-inf-structure! binf #f)
    (dump-compressed binf bci-path)))

(define (dump-compressed object path)
  (call-with-temporary-filename
    (lambda (temp)
      (compiler-file-output object temp)
      (compress temp path))))

(define compiler:dump-info-file
  compiler:dump-bci-file)

(define (compile-data-from-file scode output-pathname)
  scode output-pathname
  (error "Illegal operation:" 'COMPILE-DATA-FROM-FILE))

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
     (set! *interned-assignments* '())
     (set! *interned-constants* '())
     (set! *interned-global-links* '())
     (set! *interned-static-variables* '())
     (set! *interned-uuo-links* '())
     (set! *interned-variables* '())
     (set! *block-label* (generate-label))
     (set! *external-labels* '())
     (set! *ic-procedure-headers* '())
     (phase/assemble)
     (phase/link)
     *result*)))

(define (canonicalize-label-name name)
  ;; The Scheme assembler allows any Scheme symbol as a label
  name)