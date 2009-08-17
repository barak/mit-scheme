#| -*-Scheme-*-

$Id: 9a376e0c9cedece5c03fb80f8155daa5a95c82df $

Copyright (c) 1995, 1999 Massachusetts Institute of Technology

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

;;;; RTL Instruction Scheduling
;;; package: (compiler rtl-optimizer instruction-scheduling)

(declare (usual-integrations))

(define (rtl-instruction-scheduling rgraphs)
  (for-each (lambda (rgraph)
	      (fluid-let ((*current-rgraph* rgraph))
		(for-each schedule-bblock (rgraph-bblocks rgraph))))
	    rgraphs))

(define (schedule-bblock bblock)
  bblock
  unspecific)