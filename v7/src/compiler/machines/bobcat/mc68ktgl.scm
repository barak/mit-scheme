#| -*- Scheme -*-

$Id: mc68ktgl.scm,v 1.3 1999/01/02 06:06:43 cph Exp $

Copyright (c) 1991, 1999 Massachusetts Institute of Technology

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