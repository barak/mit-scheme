#| -*-Scheme-*-

$Id: 1d1fb50ed278f31587c2b33b65d54434264513db $

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

;;;; Miscellaneous Global Definitions
;;; package: (runtime miscellaneous-global)

(declare (usual-integrations))

;;;; Primitive Operators

(define-primitives
  error-procedure
  get-interrupt-enables set-interrupt-enables! with-interrupt-mask
  get-fixed-objects-vector with-history-disabled
  (primitive-procedure-arity 1)
  (primitive-procedure-documentation 1)

  ;; Environment
  lexical-reference lexical-assignment local-assignment
  lexical-unassigned? lexical-unbound? lexical-unreferenceable?

  ;; Pointers
  (object-type 1)
  (object-datum 1)
  (object-type? 2)
  (object-new-type object-set-type 2)
  make-non-pointer-object
  eq?

  ;; Cells
  make-cell cell? cell-contents set-cell-contents!

  ;; System Compound Datatypes
  system-pair-cons system-pair?
  system-pair-car system-pair-set-car!
  system-pair-cdr system-pair-set-cdr!

  hunk3-cons
  system-hunk3-cxr0 system-hunk3-set-cxr0!
  system-hunk3-cxr1 system-hunk3-set-cxr1!
  system-hunk3-cxr2 system-hunk3-set-cxr2!

  (system-list->vector system-list-to-vector)
  (system-subvector->list system-subvector-to-list)
  system-vector?
  (system-vector-length system-vector-size)
  system-vector-ref
  system-vector-set!)

(define (host-big-endian?)
  host-big-endian?-saved)

(define host-big-endian?-saved)

(define (initialize-package!)
  ;; Assumptions:
  ;; * Word length is 32 or 64 bits.
  ;; * Type codes are at most 8 bits.
  ;; * Zero is a non-pointer type code.
  (set! host-big-endian?-saved
	(case (object-datum
	       (vector-ref
		(object-new-type (ucode-type vector)
				 "\000\001\002\000\000\003\004\000")
		1))
	  ((#x00010200 #x0001020000030400) #t)
	  ((#x00020100 #x0004030000020100) #f)
	  (else (error "Unable to determine endianness of host."))))
  unspecific)

;;;; Potpourri

(define (identity-procedure x) x)
(define (null-procedure . args) args '())
(define (false-procedure . args) args #f)
(define (true-procedure . args) args #t)

;; This definition is replaced later in the boot sequence.
(define apply (ucode-primitive apply 2))

(define (eval expression environment)
  (extended-scode-eval (syntax expression environment) environment))

(define (scode-eval scode environment)
  (hook/scode-eval scode environment))

(define hook/scode-eval
  (ucode-primitive scode-eval))

(define-integrable (system-hunk3-cons type cxr0 cxr1 cxr2)
  (object-new-type type (hunk3-cons cxr0 cxr1 cxr2)))

(define (limit-interrupts! limit-mask)
  (set-interrupt-enables! (fix:and limit-mask (get-interrupt-enables))))

(define (object-component-binder get-component set-component!)
  (lambda (object new-value thunk)
    (let ((old-value))
      (shallow-fluid-bind
       (lambda ()
	 (set! old-value (get-component object))
	 (set-component! object new-value)
	 (set! new-value #f)
	 unspecific)
       thunk
       (lambda ()
	 (set! new-value (get-component object))
	 (set-component! object old-value)
	 (set! old-value #f)
	 unspecific)))))

(define (bind-cell-contents! cell new-value thunk)
  (let ((old-value))
    (shallow-fluid-bind
     (lambda ()
       (set! old-value (cell-contents cell))
       (set-cell-contents! cell new-value)
       (set! new-value)
       unspecific)
     thunk
     (lambda ()
       (set! new-value (cell-contents cell))
       (set-cell-contents! cell old-value)
       (set! old-value)
       unspecific))))

(define (values . objects)
  (lambda (receiver)
    (apply receiver objects)))

(define (call-with-values thunk receiver)
  ((thunk) receiver))

(define with-values call-with-values)

(define (write-to-string object #!optional max)
  (if (or (default-object? max) (not max))
      (with-output-to-string (lambda () (write object)))
      (with-output-to-truncated-string max (lambda () (write object)))))

(define (pa procedure)
  (guarantee-procedure procedure 'PA)
  (cond ((procedure-lambda procedure)
	 => (lambda (scode)
	      (pp (unsyntax-lambda-list scode))))
	((and (primitive-procedure? procedure)
	      (primitive-procedure-documentation procedure))
	 => (lambda (documentation)
	      (display documentation)
	      (newline)))
	(else
	 (display "No documentation or debugging info for ")
	 (display procedure)
	 (display ".")
	 (newline))))

(define (pwd)
  (working-directory-pathname))

(define (cd #!optional pathname)
  (set-working-directory-pathname!
    (if (default-object? pathname)
        (user-homedir-pathname)
 	pathname)))

(define (show-time thunk)
  (let ((process-start (process-time-clock))
	(process-start/nogc (runtime))
	(real-start (real-time-clock)))
    (let ((value (thunk)))
      (let* ((process-end (process-time-clock))
	     (process-end/nogc (runtime))
	     (real-end (real-time-clock))
	     (process-time (- process-end process-start))
	     (process-time/nogc
	      (round->exact (* 1000 (- process-end/nogc process-start/nogc)))))
	(write-notification-line
	 (lambda (port)
	   (write-string "process time: " port)
	   (write process-time port)
	   (write-string " (" port)
	   (write process-time/nogc port)
	   (write-string " RUN + " port)
	   (write (- process-time process-time/nogc) port)
	   (write-string " GC); real time: " port)
	   (write (- real-end real-start) port))))
      value)))

(define (wait-interval ticks)
  (let ((end (+ (real-time-clock) ticks)))
    (let wait-loop ()
      (if (< (real-time-clock) end)
	  (wait-loop)))))

(define (exit #!optional integer)
  (hook/exit (if (default-object? integer) #f integer)))

(define (default/exit integer)
  (if (prompt-for-confirmation "Kill Scheme")
      (%exit integer)))

(define hook/exit default/exit)

(define (%exit #!optional integer)
  (event-distributor/invoke! event:before-exit)
  (if (or (default-object? integer)
	  (not integer))
      ((ucode-primitive exit 0))
      ((ucode-primitive exit-with-value 1) integer)))

(define (quit)
  (hook/quit))

(define (%quit)
  (with-absolutely-no-interrupts (ucode-primitive halt))
  unspecific)

(define default/quit %quit)
(define hook/quit default/quit)

(define user-initial-environment
  (*make-environment system-global-environment
		     (vector lambda-tag:unnamed)))

(define user-initial-prompt
  "]=>")

(define (link-variables target-environment target-name
			source-environment source-name)
  ((ucode-primitive link-variables 4) (->environment target-environment)
				      target-name
				      (->environment source-environment)
				      source-name))

(define (environment-link-name target-environment source-environment name)
  ;; Obsolete; for backwards compatibility.
  (link-variables target-environment name source-environment name))

(define (unbind-variable environment name)
  ((ucode-primitive unbind-variable 2) (->environment environment) name))

(define (object-gc-type object)
  (%encode-gc-type ((ucode-primitive object-gc-type 1) object)))

(define (type-code->gc-type code)
  (%encode-gc-type ((ucode-primitive type->gc-type 1) code)))

(define (%encode-gc-type t)
  (if (not (and (fix:fixnum? t)
		(fix:>= t -4)
		(fix:<= t 4)))
      (error "Illegal GC-type value:" t))
  (vector-ref '#(COMPILED-ENTRY VECTOR GC-INTERNAL UNDEFINED NON-POINTER
				CELL PAIR TRIPLE QUADRUPLE)
	      (fix:+ t 4)))

(define (object-non-pointer? object)
  (case (object-gc-type object)
    ((NON-POINTER) #t)
    ((GC-INTERNAL)
     (or (object-type? (ucode-type manifest-nm-vector) object)
	 (and (object-type? (ucode-type reference-trap) object)
	      (<= (object-datum object) trap-max-immediate))))
    (else #f)))

(define (object-pointer? object)
  (case (object-gc-type object)
    ((CELL PAIR TRIPLE QUADRUPLE VECTOR COMPILED-ENTRY) #t)
    ((GC-INTERNAL)
     (or (object-type? (ucode-type broken-heart) object)
	 (and (object-type? (ucode-type reference-trap) object)
	      (> (object-datum object) trap-max-immediate))))
    (else #f)))

(define (non-pointer-type-code? code)
  (case (type-code->gc-type code)
    ((NON-POINTER) #t)
    ((GC-INTERNAL) (fix:= (ucode-type manifest-nm-vector) code))
    (else #f)))

(define (pointer-type-code? code)
  (case (type-code->gc-type code)
    ((CELL PAIR TRIPLE QUADRUPLE VECTOR COMPILED-ENTRY) #t)
    ((GC-INTERNAL) (fix:= (ucode-type broken-heart) code))
    (else #f)))

(define (undefined-value? object)
  ;; Note: the unparser takes advantage of the fact that objects
  ;; satisfying this predicate also satisfy:
  ;; (object-type? (ucode-type constant) object)
  (or (eq? object unspecific)
      (eq? object (object-new-type (ucode-type constant) 2))))

(define unspecific
  (object-new-type (ucode-type constant) 1))

(define (obarray->list #!optional obarray)
  (let ((obarray
	 (if (default-object? obarray)
	     (fixed-objects-item 'OBARRAY)
	     obarray)))
    (let per-bucket
	((index (fix:- (vector-length obarray) 1))
	 (accumulator '()))
      (if (fix:< index 0)
	  accumulator
	  (let per-symbol
	      ((bucket (vector-ref obarray index))
	       (accumulator accumulator))
	    (if (pair? bucket)
		(per-symbol (cdr bucket) (cons (car bucket) accumulator))
		(per-bucket (fix:- index 1) accumulator)))))))

(define (impurify object)
  object)

(define (fasdump object filename #!optional quiet? dump-option)
  (let ((filename (->namestring (merge-pathnames filename)))
	(quiet? (if (default-object? quiet?) #f quiet?))
	(dump-option (if (default-object? dump-option) #f dump-option)))
    (let ((do-it
	   (lambda ()
	     (let loop ()
	       (if (not ((ucode-primitive primitive-fasdump)
			 object filename dump-option))
		   (begin
		     (with-simple-restart 'RETRY "Try again."
		       (lambda ()
			 (error "FASDUMP: Object is too large to be dumped:"
				object)))
		     (loop)))))))
    (if quiet?
	(do-it)
	(with-notification (lambda (port)
			     (write-string "Dumping " port)
			     (write (enough-namestring filename) port))
	  do-it)))))

;;;; Hook lists

(define-record-type <hook-list>
    (%make-hook-list hooks)
    hook-list?
  (hooks hook-list-hooks set-hook-list-hooks!))

(define (make-hook-list)
  (%make-hook-list '()))

(define (guarantee-hook-list object caller)
  (if (not (hook-list? object))
      (error:not-hook-list object caller)))

(define (error:not-hook-list object caller)
  (error:wrong-type-argument object "hook list" caller))

(define (append-hook-to-list hook-list key hook)
  (guarantee-hook-list hook-list 'APPEND-HOOK-TO-LIST)
  (let loop ((alist (hook-list-hooks hook-list)) (prev #f))
    (if (pair? alist)
	(loop (cdr alist)
	      (if (eq? (caar alist) key)
		  (begin
		    (if prev
			(set-cdr! prev (cdr alist))
			(set-hook-list-hooks! hook-list (cdr alist)))
		    prev)
		  alist))
	(let ((tail (list (cons key hook))))
	  (if prev
	      (set-cdr! prev tail)
	      (set-hook-list-hooks! hook-list tail))))))

(define (remove-hook-from-list hook-list key)
  (guarantee-hook-list hook-list 'REMOVE-HOOK-FROM-LIST)
  (let loop ((alist (hook-list-hooks hook-list)) (prev #f))
    (if (pair? alist)
	(loop (cdr alist)
	      (if (eq? (caar alist) key)
		  (begin
		    (if prev
			(set-cdr! prev (cdr alist))
			(set-hook-list-hooks! hook-list (cdr alist)))
		    prev)
		  alist)))))

(define (hook-in-list? hook-list key)
  (guarantee-hook-list hook-list 'HOOK-IN-LIST?)
  (if (assq key (hook-list-hooks hook-list)) #t #f))

(define (run-hooks-in-list hook-list . arguments)
  (guarantee-hook-list hook-list 'RUN-HOOKS-IN-LIST)
  (for-each (lambda (p)
	      (apply (cdr p) arguments))
	    (hook-list-hooks hook-list)))