#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Boot-time sequencing
;;; package: (runtime)

(declare (usual-integrations))

(define (boot-sequencer)
  (let ((befores '())
	(afters-head '())
	(afters-tail '())
	(actions-head '())
	(actions-tail '())
	(triggered? #f))

    (define (add-before! seq)
      (if triggered?
	  (error "Boot sequencer already triggered:" this))
      (if (not (or (seq 'triggered?)
		   (let loop ((scan befores))
		     (and (pair? scan)
			  (or (eqv? seq (car scan))
			      (loop (cdr scan)))))))
	  (begin
	    (set! befores (cons seq befores))
	    (seq '%add-after! this))))

    (define (add-after! seq)
      (let ((p (list seq)))
	(if (null? afters-tail)
	    (set! afters-head p)
	    (set-cdr! afters-tail p))
	(set! afters-tail p)
	unspecific))

    (define (add-action! action)
      (if triggered?
	  (error "Boot sequencer already triggered:" this))
      (let ((p (list action)))
	(if (null? actions-tail)
	    (set! actions-head p)
	    (set-cdr! actions-tail p))
	(set! actions-tail p)
	unspecific))

    (define (trigger!)
      (if (not (null? befores))
	  (error "Boot sequencer not ready:" this))
      (set! triggered? 'started)
      (for-each (lambda (action) (action))
		actions-head)
      (set! actions-head '())
      (set! actions-tail '())
      (set! triggered? 'finished)
      (let ((ready
	     (filter! (lambda (seq) (seq '%remove-before! this))
		      afters-head)))
	(set! afters-head '())
	(set! afters-tail '())
	(for-each (lambda (seq) (seq 'trigger!))
		  ready)))

    (define (remove-before! seq)
      (and (pair? befores)
	   (begin
	     (set! befores (delq! seq befores))
	     (null? befores))))

    (define (this operator . args)
      (case operator
	((add-before!) (apply add-before! args))
	((add-action!) (apply add-action! args))
	((trigger!) (apply trigger! args))
	((triggered?) triggered?)
	;; Private:
	((%add-after!) (apply add-after! args))
	((%remove-before!) (apply remove-before! args))
	(else (error "Unknown operator:" operator))))

    this))