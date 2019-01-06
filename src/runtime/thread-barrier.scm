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

;;;; Thread barriers
;;; package: (runtime thread barrier)

(declare (usual-integrations))

(define-structure (thread-barrier
		   (constructor %make-thread-barrier (count current condvar))
		   (conc-name thread-barrier.))
  (lock (make-thread-mutex) read-only #t)
  (condvar #f read-only #t)
  (count #f read-only #t)
  current
  (generation 0))

(define (make-thread-barrier count #!optional name)
  (guarantee exact-positive-integer? count 'make-thread-barrier)
  (let ((current count)
        (condvar
         (make-condition-variable
          (if (default-object? name) "thread barrier" name))))
    (%make-thread-barrier count current condvar)))

(define (thread-barrier-wait barrier)
  (guarantee thread-barrier? barrier 'thread-barrier-wait)
  (let ((lock (thread-barrier.lock barrier))
	(condvar (thread-barrier.condvar barrier)))
    (with-thread-mutex-lock lock
      (lambda ()
	(let ((count (thread-barrier.count barrier))
	      (current (thread-barrier.current barrier))
	      (generation (thread-barrier.generation barrier)))
	  (assert (< 0 current))
	  (assert (<= current count))
	  (let ((next (- current 1)))
	    (if (zero? next)
		(begin
		  (set-thread-barrier.current! barrier count)
		  (set-thread-barrier.generation! barrier (+ 1 generation))
		  (condition-variable-broadcast! condvar)
		  #t)
		(begin
		  (set-thread-barrier.current! barrier next)
		  (do () ((< generation (thread-barrier.generation barrier)))
		    (condition-variable-wait! condvar lock))
		  #f))))))))