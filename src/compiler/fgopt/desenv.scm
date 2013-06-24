#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; Environment Design

(declare (usual-integrations))

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
				 (set-variable-in-cell?! variable #t)))
			   (block-bound-variables block))
		 (setup-stack-block-offsets! block))
		((CONTINUATION)
		 (set-block-frame-size!
		  block
		  (continuation/frame-size (block-procedure block))))
		((CLOSURE)
		 unspecific)
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
		     (begin
		       (set-procedure-closure-offset! procedure offset)
		       (+ offset 1))
		     offset)))))))
       (if (or (procedure/closure? procedure)
	       (not (stack-block/static-link? block)))
	   offset
	   (+ offset 1))))))

(define (setup-variable-offsets! variables offset)
  (if (pair? variables)
      (if (variable-register (car variables))
	  (setup-variable-offsets! (cdr variables) offset)
	  (begin
	    (set-variable-normal-offset! (car variables) offset)
	    (setup-variable-offsets! (cdr variables) (+ offset 1))))
      offset))

(define (setup-variable-offset! variable offset)
  (if (and variable (not (variable-register variable)))
      (begin
	(set-variable-normal-offset! variable offset)
	(+ offset 1))
      offset))



