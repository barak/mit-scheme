#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/global.scm,v 14.5 1988/08/05 20:47:24 cph Exp $

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

;;;; Miscellaneous Global Definitions
;;; package: ()

(declare (usual-integrations))

;;;; Primitive Operators

(define-primitives
  scode-eval force error-procedure
  set-interrupt-enables! enable-interrupts! with-interrupt-mask
  get-fixed-objects-vector with-history-disabled
  primitive-procedure-arity

  ;; Environment
  lexical-reference lexical-assignment local-assignment
  lexical-unassigned? lexical-unbound? lexical-unreferenceable?
  environment-link-name

  ;; Pointers
  (object-type 1)
  (object-gc-type 1)
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

;;;; Potpourri

(define (identity-procedure x) x)
(define (null-procedure . args) args '())
(define (false-procedure . args) args false)
(define (true-procedure . args) args true)

(define (apply f . args)
  ((ucode-primitive apply)
   f
   (if (null? args)
       '()
       (let loop ((first-element (car args)) (rest-elements (cdr args)))
	 (if (null? rest-elements)
	     first-element
	     (cons first-element
		   (loop (car rest-elements) (cdr rest-elements))))))))

(define (eval expression environment)
  (scode-eval (syntax expression system-global-syntax-table) environment))
(define-integrable (system-hunk3-cons type cxr0 cxr1 cxr2)
  (object-new-type type (hunk3-cons cxr0 cxr1 cxr2)))

(define (bind-cell-contents! cell new-value thunk)
  (let ((old-value))
    (dynamic-wind (lambda ()
		    (set! old-value (cell-contents cell))
		    (set-cell-contents! cell new-value)
		    (set! new-value))
		  thunk
		  (lambda ()
		    (set! new-value (cell-contents cell))
		    (set-cell-contents! cell old-value)
		    (set! old-value)))))

(define (values . objects)
  (lambda (receiver)
    (apply receiver objects)))

(define-integrable (with-values thunk receiver)
  ((thunk) receiver))

(define (write-to-string object #!optional max)
  (if (default-object? max) (set! max false))
  (if (not max)
      (with-output-to-string
       (lambda ()
	 (write object)))
      (with-output-to-truncated-string max
	(lambda ()
	  (write object)))))

(define (pa procedure)
  (if (not (compound-procedure? procedure))
      (error "Must be a compound procedure" procedure))  (pp (unsyntax-lambda-list (procedure-lambda procedure))))

(define (pwd)
  (working-directory-pathname))

(define (cd pathname)
  (set-working-directory-pathname! pathname))

;; Compatibility.
(define %pwd pwd)
(define %cd cd)

(define (show-time thunk)
  (let ((process-start (process-time-clock))
	(real-start (real-time-clock)))
    (let ((value (thunk)))
      (let ((process-end (process-time-clock))
	    (real-end (real-time-clock)))
	(newline)
	(write-string "process time: ")
	(write (- process-end process-start))
	(write-string "; real time: ")
	(write (- real-end real-start)))
      value)))

(define (wait-interval ticks)
  (let ((end (+ (real-time-clock) ticks)))
    (let wait-loop ()
      (if (< (real-time-clock) end)
	  (wait-loop)))))

(define-integrable (future? object)
  ((ucode-primitive primitive-type? 2) (ucode-type future) object))

(define (exit)
  (if (prompt-for-confirmation "Kill Scheme? ")      (%exit)))

(define (%exit)
  (close-all-open-files)
  ((ucode-primitive exit)))

(define (quit)
  (with-absolutely-no-interrupts (ucode-primitive halt))
  unspecific)

(define syntaxer/default-environment
  (let () (the-environment)))

(define user-initial-environment
  (let () (the-environment)))

(define user-initial-prompt
  "]=>")
(define (copy-program exp)
  (if (not (object-type? (ucode-type compiled-entry) exp))
      (error "COPY-PROGRAM: Can only copy compiled programs" exp))
  (let* ((original (compiled-code-address->block exp))
	 (block
	  (object-new-type
	   (ucode-type compiled-code-block)
	   (vector-copy (object-new-type (ucode-type vector) original))))
	 (end (system-vector-length block)))

    (define (map-entry entry)
      (with-absolutely-no-interrupts
       (lambda ()
	 ((ucode-primitive primitive-object-set-type)
	  (object-type entry)
	  (+ (compiled-code-address->offset entry)
	     (object-datum block))))))

    (let loop ((n (1+ (object-datum (system-vector-ref block 0)))))
      (if (< n end)
	  (begin
	    (if (lambda? (system-vector-ref block n))
		(lambda-components (system-vector-ref block n)
		  (lambda (name required optional rest auxiliary declarations
				body)
		    (if (and (object-type? (ucode-type compiled-entry) body)
			     (eq? original
				  (compiled-code-address->block body)))
			(system-vector-set!
			 block
			 n
			 (make-lambda name required optional rest auxiliary
				      declarations (map-entry body)))))))
	    (loop (1+ n)))))
    (map-entry exp)))

(define-integrable (object-non-pointer? object)
  (zero? (object-gc-type object)))

(define-integrable (object-pointer? object)
  (not (object-non-pointer? object)))

(define (impurify object)
  (if (and (object-pointer? object) (object-pure? object))
      ((ucode-primitive primitive-impurify) object))
  object)

(define (fasdump object filename)
  (let ((filename (canonicalize-output-filename filename))
	(port (cmdl/output-port (nearest-cmdl))))
    (newline port)
    (write-string "FASDumping " port)    (write filename port)
    (if (not ((ucode-primitive primitive-fasdump) object filename false))
	(error "FASDUMP: Object is too large to be dumped" object))
    (write-string " -- done" port))
  object)

(define (undefined-value? object)
  ;; Note: the unparser takes advantage of the fact that objects
  ;; satisfying this predicate also satisfy:
  ;; (object-type? (microcode-type 'TRUE) object)
  (or (eq? object undefined-conditional-branch)
      ;; same as `undefined-conditional-branch'.
      ;; (eq? object *the-non-printing-object*)
      ;; (eq? object unspecific)
      (eq? object (microcode-object/unassigned))))

(define *the-non-printing-object*
  (object-new-type (ucode-type true) 1))

(define unspecific
  (object-new-type (ucode-type true) 1))