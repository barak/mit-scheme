#| -*-Scheme-*-

$Id: genenv.scm,v 1.7 2007/01/05 21:19:29 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Environment hacking for 6.001

(declare (usual-integrations))

(define build-environment)

(define make-unassigned-object
  microcode-object/unassigned)

(let ()
  (define (get-values descriptors frame receiver)
    (define (inner descriptors names values unref)
      (define (do-next name-here name-there)
	(if (or (not (symbol? name-there))
		(lexical-unreferenceable? frame name-there))
	    (inner (cdr descriptors)
		   (cons name-here names)
		   (cons (make-unassigned-object)
			 values)
		   (if (not (symbol? name-there))
		       unref
		       (cons name-here unref)))
	    (inner (cdr descriptors)
		   (cons name-here names)
		   (cons (lexical-reference frame name-there)
			 values)
		   unref)))

      (if (null? descriptors)
	  (receiver (reverse! names)
		    (reverse! values)
		    (reverse! unref))
	  (let ((this (car descriptors)))
	    (cond ((not (pair? this))
		   (do-next this this))
		  ((null? (cdr this))
		   (do-next (car this) (car this)))
		  (else
		   (do-next (car this) (cdr this)))))))
    (inner descriptors '() '() '()))

  (set! build-environment
	(named-lambda (build-environment names source-frame
					 #!optional parent-frame
					 process receiver)
	  (get-values names source-frame
	    (lambda (names values unreferenceable)
	      (if (default-object? receiver)
		  unreferenceable
		  (receiver
		   (apply (scode-eval (make-lambda lambda-tag:make-environment
						   names
						   '()
						   '()
						   '()
						   '()
						   (make-the-environment))
				      (if (default-object? parent-frame)
					  source-frame
					  parent-frame))
			  (map (if (default-object? process)
				   unmap-reference-trap
				   (lambda (x)
				     (unmap-reference-trap (process x))))
			       values))
		   unreferenceable))))))
  42)