#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Environment Model
;;; package: (scode-optimizer)

(declare (usual-integrations)
	 (integrate-external "object"))

(define (variable/make&bind! block name)
  (guarantee-symbol name 'variable/make&bind!)
  (or (%block/lookup-name block name)
      (%variable/make&bind! block name)))

(define (%variable/make&bind! block name)
  (let ((variable (variable/make block name '())))
    (set-block/bound-variables! block
				(cons variable (block/bound-variables block)))
    variable))

(define (block/lookup-name block name intern?)
  (guarantee-symbol name 'block/lookup-name)
  (let search ((block block))
    (or (%block/lookup-name block name)
	(if (block/parent block)
	    (search (block/parent block))
	    (and intern? (%variable/make&bind! block name))))))

(define (%block/lookup-name block name)
  (find-matching-item (block/bound-variables block)
		      (lambda (variable)
			(eq? (variable/name variable) name))))

(define (block/limited-lookup block name limit)
  (guarantee-symbol name 'block/limited-lookup)
  (let search ((block block))
    (and (not (eq? block limit))
	 (or (%block/lookup-name block name)
	     (and (block/parent block)
		  (search (block/parent block)))))))

(define (block/lookup-names block names intern?)
  (map (lambda (name)
	 (block/lookup-name block name intern?))
       names))

(define (block/for-each-bound-variable block procedure)
  (for-each procedure (block/bound-variables block)))

(define (block/unsafe! block)
  (if (block/safe? block)
      (begin
	(set-block/safe?! block false)
	(if (block/parent block)
	    (block/unsafe! (block/parent block))))))