#| -*-Scheme-*-

$Id: desenv.scm,v 4.4 1999/01/02 06:06:43 cph Exp $

Copyright (c) 1987, 1999 Massachusetts Institute of Technology

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

;;;; Environment Design

(declare (usual-integrations))

;;;; Frame Layout

#|

Layout of stack frames.  The top of each frame is where the frame
pointer points to, which is the most recently pushed item in the
frame (i.e. the item closest to the top of stack).  There are two
kinds of frames, depending on what kind of procedure this is.

Open procedure frame:

+-------+-------+-------+-------+
|	   Auxiliary 1		|
+-------+-------+-------+-------+
:		:		:
+-------+-------+-------+-------+
|	   Auxiliary M		|
+-------+-------+-------+-------+
|	    Argument 1		|
+-------+-------+-------+-------+
:		:		:
+-------+-------+-------+-------+
|	    Argument N		|
+-------+-------+-------+-------+
|	  Rest Argument		|	(omitted if none)
+-------+-------+-------+-------+
|    Pointer to parent frame	|	(omitted if known)
+-------+-------+-------+-------+

Closed procedure frame:

+-------+-------+-------+-------+
|	   Auxiliary 1		|
+-------+-------+-------+-------+
:		:		:
+-------+-------+-------+-------+
|	   Auxiliary M		|
+-------+-------+-------+-------+
|	     Operator		|	(omitted if not needed)
+-------+-------+-------+-------+
|	    Argument 1		|
+-------+-------+-------+-------+
:		:		:
+-------+-------+-------+-------+
|	    Argument N		|
+-------+-------+-------+-------+
|	  Rest Argument		|	(omitted if none)
+-------+-------+-------+-------+

|#

(define (design-environment-frames! blocks)
  (for-each (lambda (block)
	      (enumeration-case block-type (block-type block)
		((IC)
		 (if (rvalue/procedure? (block-procedure block))
		     (setup-ic-block-offsets! block)))
		((STACK)
		 (for-each (lambda (variable)
			     (if (variable-assigned? variable)
				 (set-variable-in-cell?! variable true)))
			   (block-bound-variables block))
		 (setup-stack-block-offsets! block))
		((CONTINUATION)
		 (set-block-frame-size!
		  block
		  (continuation/frame-size (block-procedure block))))
		((CLOSURE) 'DONE)
		(else
		 (error "Illegal block type" block))))
	    blocks))

(define (setup-ic-block-offsets! block)
  (let ((procedure (block-procedure block)))
    (setup-variable-offsets!
     (procedure-names procedure)
     (setup-variable-offset!
      (procedure-rest procedure)
      (setup-variable-offsets!
       (procedure-optional procedure)
       (setup-variable-offsets! (cdr (procedure-required procedure))
				ic-block-first-parameter-offset))))))

(define (setup-stack-block-offsets! block)
  (let ((procedure (block-procedure block)))
    (set-block-frame-size!
     block
     (let ((offset
	    (setup-variable-offset!
	     (procedure-rest procedure)
	     (setup-variable-offsets!
	      (procedure-optional procedure)
	      (setup-variable-offsets!
	       (cdr (procedure-required procedure))
	       (let ((offset
		      (setup-variable-offsets! (procedure-names procedure) 0)))
		 (if (and (procedure/closure? procedure)
			  (closure-procedure-needs-operator? procedure))
		     (begin (set-procedure-closure-offset! procedure offset)
			    (1+ offset))
		     offset)))))))
       (if (or (procedure/closure? procedure)
	       (not (stack-block/static-link? block)))
	   offset
	   (1+ offset))))))

(define (setup-variable-offsets! variables offset)
  (if (null? variables)
      offset
      (if (variable-register (car variables))
	  (setup-variable-offsets! (cdr variables) offset)
	  (begin (set-variable-normal-offset! (car variables) offset)
		 (setup-variable-offsets! (cdr variables) (1+ offset))))))

(define (setup-variable-offset! variable offset)
  (if (and variable (not (variable-register variable)))
      (begin (set-variable-normal-offset! variable offset)
	     (1+ offset))
      offset))



