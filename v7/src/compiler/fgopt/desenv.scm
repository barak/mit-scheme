#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/desenv.scm,v 4.2 1989/06/07 20:47:04 markf Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/desenv.scm,v 4.2 1989/06/07 20:47:04 markf Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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



