#| -*- Scheme -*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/mc68ktgl.scm,v 1.2 1991/10/25 06:49:53 cph Exp $

Copyright (c) 1991 Massachusetts Institute of Technology

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

(declare (usual-integrations))

(define (mc68k/toggle-closure-format #!optional new-format)
  (case (if (default-object? new-format)
	    (if (eq? MC68K/closure-format 'MC68020)
		'MC68040
		'MC68020)
	    new-format)
    ((MC68020)
     (set! closure-first-offset MC68020/closure-first-offset)
     (set! closure-object-first-offset MC68020/closure-object-first-offset)
     (set! closure-entry-distance MC68020/closure-entry-distance)
     (set! closure-environment-adjustment
	   MC68020/closure-environment-adjustment)
     (set! generate/closure-header MC68020/closure-header)
     (set! generate/cons-closure MC68020/cons-closure)
     (set! generate/cons-multiclosure MC68020/cons-multiclosure)
     (set! mc68k/closure-format 'MC68020))
    ((MC68040)
     (set! closure-first-offset MC68040/closure-first-offset)
     (set! closure-object-first-offset MC68040/closure-object-first-offset)
     (set! closure-entry-distance MC68040/closure-entry-distance)
     (set! closure-environment-adjustment
	   MC68040/closure-environment-adjustment)
     (set! generate/closure-header MC68040/closure-header)
     (set! generate/cons-closure MC68040/cons-closure)
     (set! generate/cons-multiclosure MC68040/cons-multiclosure)
     (set! mc68k/closure-format 'MC68040))
    (else
     (error "Unknown closure format:" new-format)))
  MC68K/closure-format)