;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/comman.scm,v 1.66 1991/04/21 00:49:23 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Commands and Variables

(declare (usual-integrations))

(define-named-structure "Command"
  name
  description
  interactive-specification
  procedure)

(unparser/set-tagged-vector-method!
 %command-tag
 (unparser/standard-method 'COMMAND
   (lambda (state command)
     (unparse-object state (command-name command)))))

(define (command-name-string command)
  (editor-name/internal->external (symbol->string (command-name command))))

(define (editor-name/internal->external string)
  string)

(define (editor-name/external->internal string)
  string)

(define (make-command name description specification procedure)
  (let ((command
	 (let ((name (symbol->string name)))
	   (or (string-table-get editor-commands name)
	       (let ((command (%make-command)))
		 (string-table-put! editor-commands name command)
		 command)))))
    (vector-set! command command-index:name name)
    (vector-set! command command-index:description description)
    (vector-set! command command-index:interactive-specification specification)
    (vector-set! command command-index:procedure procedure)
    command))

(define editor-commands (make-string-table 500))

(define (name->command name)
  (let ((name (canonicalize-name name)))
    (or (string-table-get editor-commands (symbol->string name))
	(letrec ((command
		  (make-command
		   name
		   "undefined command"
		   '()
		   (lambda ()
		     (editor-error "Undefined command: "
				   (command-name-string command))))))
	  command))))

(define (->command object)
  (if (command? object) object (name->command object)))

(define-named-structure "Variable"
  name
  description
  value
  buffer-local?
  initial-value
  assignment-daemons
  value-validity-test)

(unparser/set-tagged-vector-method!
 %variable-tag
 (unparser/standard-method 'VARIABLE
   (lambda (state variable)
     (unparse-object state (variable-name variable)))))

(define (variable-name-string variable)
  (editor-name/internal->external (symbol->string (variable-name variable))))

(define (make-variable name description value buffer-local?)
  (let ((variable
	 (let ((name (symbol->string name)))
	   (or (string-table-get editor-variables name)
	       (let ((variable (%make-variable)))
		 (string-table-put! editor-variables name variable)
		 variable)))))
    (vector-set! variable variable-index:name name)
    (vector-set! variable variable-index:description description)
    (vector-set! variable variable-index:value value)
    (vector-set! variable variable-index:buffer-local? buffer-local?)
    (vector-set! variable variable-index:initial-value value)
    (vector-set! variable variable-index:assignment-daemons '())
    (vector-set! variable variable-index:value-validity-test false)
    variable))

(define-integrable (%%set-variable-value! variable value)
  (vector-set! variable variable-index:value value))

(define-integrable (make-variable-buffer-local! variable)
  (vector-set! variable variable-index:buffer-local? true))

(define (define-variable-value-validity-test variable test)
  (vector-set! variable variable-index:value-validity-test test))

(define (check-variable-value-validity! variable value)
  (if (not (variable-value-valid? variable value))
      (error:datum-out-of-range value)))

(define (variable-value-valid? variable value)
  (or (not (variable-value-validity-test variable))
      ((variable-value-validity-test variable) value)))

(define (add-variable-assignment-daemon! variable daemon)
  (let ((daemons (variable-assignment-daemons variable)))
    (if (not (memq daemon daemons))
	(vector-set! variable
		     variable-index:assignment-daemons
		     (cons daemon daemons)))))

(define (invoke-variable-assignment-daemons! variable)
  (do ((daemons (variable-assignment-daemons variable) (cdr daemons)))
      ((null? daemons))
    ((car daemons) variable)))

(define editor-variables (make-string-table 50))

(define (name->variable name)
  (let ((name (canonicalize-name name)))
    (or (string-table-get editor-variables (symbol->string name))
	(make-variable name "" false false))))

(define (->variable object)
  (if (variable? object) object (name->variable object)))

(define-integrable (%set-variable-value! variable value)
  (%%set-variable-value! variable value)
  (invoke-variable-assignment-daemons! variable))

(define (set-variable-value! variable value)
  (if (variable-buffer-local? variable)
      (define-variable-local-value! (current-buffer) variable value)
      (begin
	(check-variable-value-validity! variable value)
	(without-interrupts
	 (lambda ()
	   (%set-variable-value! variable value))))))

(define (with-variable-value! variable new-value thunk)
  (let ((old-value))
    (dynamic-wind (lambda ()
		    (set! old-value (variable-value variable))
		    (set-variable-value! variable new-value)
		    (set! new-value)
		    unspecific)
		  thunk
		  (lambda ()
		    (set! new-value (variable-value variable))
		    (set-variable-value! variable old-value)
		    (set! old-value)
		    unspecific))))