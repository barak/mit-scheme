#| -*-Scheme-*-

$Id: comman.scm,v 1.84 2000/06/15 00:43:40 cph Exp $

Copyright (c) 1986, 1989-2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Commands and Variables

(declare (usual-integrations))

(define-structure (command
		   (constructor %make-command ())
		   (print-procedure
		    (unparser/standard-method 'COMMAND
		      (lambda (state command)
			(unparse-object state (command-name command))))))
  name
  %description
  interactive-specification
  procedure)

(define (command-description command)
  (let ((desc (command-%description command)))
    (if (description? desc)
	desc
	(let ((new
	       (->doc-string (symbol->string (command-name command)) desc)))
	  (if new
	      (set-command-%description! command new))
	  new))))

(define (command-name-string command)
  (editor-name/internal->external (symbol->string (command-name command))))

(define (editor-name/internal->external string)
  string)

(define (editor-name/external->internal string)
  string)

(define (make-command name description specification procedure)
  (let* ((sname (symbol->string name))
	 (command
	  (or (string-table-get editor-commands sname)
	      (let ((command (%make-command)))
		(string-table-put! editor-commands sname command)
		command))))
    (set-command-name! command name)
    (set-command-%description! command (doc-string->posn sname description))
    (set-command-interactive-specification! command specification)
    (set-command-procedure! command procedure)
    command))

(define editor-commands
  (make-string-table 500))

(define (name->command name #!optional if-undefined)
  (let ((name (canonicalize-name name)))
    (or (string-table-get editor-commands (symbol->string name))
	(case (if (default-object? if-undefined) 'INTERN if-undefined)
	  ((#F) #f)
	  ((ERROR) (error "Undefined command:" name))
	  ((INTERN)
	   (letrec ((command
		     (make-command
		      name
		      "undefined command"
		      '()
		      (lambda () (editor-error "Undefined command:" name)))))
	     command))
	  (else
	   (error:bad-range-argument if-undefined 'NAME->COMMAND))))))

(define (->command object)
  (if (command? object)
      object
      (name->command object)))

(define (copy-command new-name command)
  (make-command new-name
		(command-%description command)
		(command-interactive-specification command)
		(command-procedure command)))

(define-structure (variable
		   (constructor %make-variable ())
		   (print-procedure
		    (unparser/standard-method 'VARIABLE
		      (lambda (state variable)
			(unparse-object state (variable-name variable))))))
  name
  %description
  %value
  buffer-local?
  initial-value
  %default-value
  assignment-daemons
  value-validity-test
  value-normalization)

(define (variable-description variable)
  (let ((desc (variable-%description variable)))
    (if (description? desc)
	desc
	(let ((new
	       (->doc-string (symbol->string (variable-name variable)) desc)))
	  (if new
	      (set-variable-%description! variable new))
	  new))))

(define-integrable variable-value variable-%value)
(define-integrable variable-default-value variable-%default-value)
(define-integrable define-variable-value-validity-test
  set-variable-value-validity-test!)

(define (variable-name-string variable)
  (editor-name/internal->external (symbol->string (variable-name variable))))

(define (make-variable name description value buffer-local?)
  (let* ((sname (symbol->string name))
	 (variable
	  (or (string-table-get editor-variables sname)
	      (let ((variable (%make-variable)))
		(string-table-put! editor-variables sname variable)
		variable))))
    (set-variable-name! variable name)
    (set-variable-%description! variable (doc-string->posn sname description))
    (set-variable-%value! variable value)
    (set-variable-buffer-local?! variable buffer-local?)
    (set-variable-initial-value! variable value)
    (set-variable-%default-value! variable value)
    (set-variable-assignment-daemons! variable '())
    (set-variable-value-validity-test! variable #f)
    (set-variable-value-normalization! variable #f)
    variable))

(define-integrable (make-variable-buffer-local! variable)
  (set-variable-buffer-local?! variable #t))

(define (normalize-variable-value variable value)
  (if (and (variable-value-validity-test variable)
	   (not ((variable-value-validity-test variable) value)))
      (editor-error "Invalid value for " (variable-name-string variable)
		    ": " value))
  (if (variable-value-normalization variable)
      ((variable-value-normalization variable) value)
      value))

(define (add-variable-assignment-daemon! variable daemon)
  (let ((daemons (variable-assignment-daemons variable)))
    (if (not (memq daemon daemons))
	(set-variable-assignment-daemons! variable (cons daemon daemons)))))

(define (invoke-variable-assignment-daemons! buffer variable)
  (if within-editor?
      (do ((daemons (variable-assignment-daemons variable) (cdr daemons)))
	  ((null? daemons))
	((car daemons) buffer variable))))

(define editor-variables
  (make-string-table 50))

(define (name->variable name #!optional if-undefined)
  (let ((name (canonicalize-name name)))
    (or (string-table-get editor-variables (symbol->string name))
	(case (if (default-object? if-undefined) 'INTERN if-undefined)
	  ((#F) #f)
	  ((ERROR) (error "Undefined variable:" name))
	  ((INTERN) (make-variable name "" #f #f))
	  (else (error:bad-range-argument if-undefined 'NAME->VARIABLE))))))

(define (->variable object)
  (if (variable? object)
      object
      (name->variable object)))

(define (variable-permanent-local! variable)
  (hash-table/put! permanent-local-variables variable #t))

(define (variable-permanent-local? variable)
  (hash-table/get permanent-local-variables variable #f))

(define permanent-local-variables
  (make-eq-hash-table))