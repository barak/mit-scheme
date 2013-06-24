#| -*-Scheme-*-

$Id: tscript.scm,v 1.4 1999/06/21 03:46:49 cph Exp $

Copyright (c) 1990, 1999 Massachusetts Institute of Technology

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

;;;; Transcript File
;;; package: (runtime transcript)

(declare (usual-integrations))

(define-structure (encap-state
		   (conc-name encap-state/)
		   (constructor make-encap-state ()))
  (transcript-port #f))

(define (transcriptable-port? object)
  (and (encapsulated-port? object)
       (encap-state? (encapsulated-port/state object))))

(define (encap/tport encap)
  (encap-state/transcript-port (encapsulated-port/state encap)))

(define (set-encap/tport! encap tport)
  (set-encap-state/transcript-port! (encapsulated-port/state encap) tport))

(define (make-transcriptable-port port)
  (make-encapsulated-port port (make-encap-state)
    (lambda (name operation)
      (let ((entry (assq name duplexed-operations)))
	(if entry
	    (and (cadr entry)
		 ((cadr entry) operation))
	    operation)))))

(define (transcript-on filename)
  (let ((encap (nearest-cmdl/port)))
    (if (not (transcriptable-port? encap))
	(error "Transcript not supported for this REPL."))
    (if (encap/tport encap)
	(error "transcript already turned on"))
    (set-encap/tport! encap (open-output-file filename))))

(define (transcript-off)
  (let ((encap (nearest-cmdl/port)))
    (if (not (transcriptable-port? encap))
	(error "Transcript not supported for this REPL."))
    (let ((tport (encap/tport encap)))
      (if tport
	  (begin
	    (set-encap/tport! encap #f)
	    (close-port tport))))))

(define duplexed-operations)

(define (initialize-package!)
  (set! duplexed-operations
	(let ((input-char
	       (lambda (operation)
		 (lambda (encap . arguments)
		   (let ((char (apply operation encap arguments))
			 (tport (encap/tport encap)))
		     (if (and tport (char? char))
			 (write-char char tport))
		     char))))
	      (input-expr
	       (lambda (operation)
		 (lambda (encap . arguments)
		   (let ((expr (apply operation encap arguments))
			 (tport (encap/tport encap)))
		     (if tport
			 (write expr tport))
		     expr))))
	      (duplex
	       (lambda (toperation)
		 (lambda (operation)
		   (lambda (encap . arguments)
		     (apply operation encap arguments)
		     (let ((tport (encap/tport encap)))
		       (if tport
			   (apply toperation tport arguments))))))))
	  `((READ-CHAR ,input-char)
	    (PROMPT-FOR-COMMAND-CHAR ,input-char)
	    (PROMPT-FOR-EXPRESSION ,input-expr)
	    (PROMPT-FOR-COMMAND-EXPRESSION ,input-expr)
	    (READ ,input-expr)
	    (DISCARD-CHAR #f)
	    (DISCARD-CHARS #f)
	    (READ-STRING #f)
	    (READ-SUBSTRING #f)
	    (WRITE-CHAR ,(duplex output-port/write-char))
	    (WRITE-SUBSTRING ,(duplex output-port/write-substring))
	    (FRESH-LINE ,(duplex output-port/fresh-line))
	    (FLUSH-OUTPUT ,(duplex output-port/flush-output))
	    (DISCRETIONARY-FLUSH-OUTPUT
	     ,(duplex output-port/discretionary-flush)))))
  unspecific)