#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/process.scm,v 1.2 1990/03/16 22:43:33 cph Exp $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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

;;;; Subprocess support
;;; package: (runtime subprocesses)

(declare (usual-integrations))

(define (initialize-package!)
  unspecific)

(let-syntax
    ((define-special-primitives
       (macro names
	 `(DEFINE-PRIMITIVES
	    ,@(map (lambda (name)
		     (list (symbol-append 'prim- name)
			   name))
		   names)))))
  (define-special-primitives
    create-process
    process-get-pid
    process-get-input-channel
    process-get-output-channel
    process-get-status-flags
    process-char-ready?))

(let-syntax
    ((define-process-primitives
       (macro names
	 `(BEGIN ,@(map (lambda (name)
			  `(BEGIN
			     (DEFINE (,name PROCESS)
			       (,(symbol-append 'prim- name)
				(PROCESS/MICROCODE-PROCESS PROCESS)))))
			names)))))
  (define-process-primitives
    process-get-pid
    process-get-input-channel
    process-get-output-channel
    process-get-status-flags))

(define-structure (process
		   (conc-name process/)
		   (constructor make-process
				(command-string microcode-process)))
  (command-string false read-only true)		;original command
  (microcode-process false read-only true) 	;index into microcode
						;process table
  (to-port false)				;port to write to process
  (from-port false)				;port to read from process
  )

(define (create-process command-string)
  (let* ((prim-process ((ucode-primitive create-process) command-string))
	 (process (make-process command-string prim-process)))
    (set-process/to-port! process (open-process-output process))
    (set-process/from-port! process (open-process-input process))
    process))

(define (kill-process process)
  ((ucode-primitive kill-process) (process/microcode-process process)))

(define (delete-process process)
  (close-output-port (process/to-port process))
  (kill-process process))