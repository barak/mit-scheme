#| -*-Scheme-*-

$Id: strout.scm,v 14.31 2008/02/02 02:02:53 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; String Output Ports (SRFI-6)
;;; package: (runtime string-output)

(declare (usual-integrations))

(define (open-output-string)
  (let ((port
	 (receive (sink extract extract! position) (make-accumulator-sink)
	   (make-generic-i/o-port #f
				  sink
				  accumulator-output-port-type
				  extract
				  extract!
				  position))))
    (port/set-coding port 'ISO-8859-1)
    (port/set-line-ending port 'NEWLINE)
    port))

(define (get-output-string port)
  ((port/operation port 'EXTRACT-OUTPUT) port))

(define (get-output-string! port)
  ((port/operation port 'EXTRACT-OUTPUT!) port))

(define (call-with-output-string generator)
  (let ((port (open-output-string)))
    (generator port)
    (get-output-string port)))

(define (with-output-to-string thunk)
  (call-with-output-string
    (lambda (port)
      (with-output-to-port port thunk))))

(define-structure (astate (type vector)
			  (initial-offset 4) ;must match "genio.scm"
			  (constructor #f))
  extract
  extract!
  position)

(define accumulator-output-port-type)
(define (initialize-package!)
  (set! accumulator-output-port-type
	(make-port-type
	 `((EXTRACT-OUTPUT
	    ,(lambda (port)
	       (output-port/flush-output port)
	       ((astate-extract (port/state port)))))
	   (EXTRACT-OUTPUT!
	    ,(lambda (port)
	       (output-port/flush-output port)
	       ((astate-extract! (port/state port)))))
	   (POSITION
	    ,(lambda (port)
	       (output-port/flush-output port)
	       ((astate-position (port/state port)))))
	   (WRITE-SELF
	    ,(lambda (port output-port)
	       port
	       (write-string " to string" output-port))))
	 (generic-i/o-port-type #f #t)))
  unspecific)

(define (make-accumulator-sink)
  (let ((chars #f)
	(index 0))

    (define (write-substring string start end)
      (let ((n (fix:+ index (fix:- end start))))
	(cond ((not chars)
	       (set! chars (new-chars 128 n)))
	      ((fix:> n (string-length chars))
	       (set! chars
		     (let ((new (new-chars (string-length chars) n)))
		       (substring-move! chars 0 index new 0)
		       new))))
	(substring-move! string start end chars index)
	(set! index n)
	(fix:- end start)))

    (define (new-chars start min-length)
      (make-string
       (let loop ((n start))
	 (if (fix:>= n min-length)
	     n
	     (loop (fix:+ n n))))))

    (values (make-non-channel-port-sink
	     (lambda (string start end)
	       (without-interrupts
		(lambda ()
		  (write-substring string start end)))))
	    (lambda ()
	      (without-interrupts
	       (lambda ()
		 (if chars
		     (string-head chars index)
		     (make-string 0)))))
	    (lambda ()
	      (without-interrupts
	       (lambda ()
		 (if chars
		     (let ((s chars))
		       (set-string-maximum-length! s index)
		       (set! chars #f)
		       (set! index 0)
		       s)
		     (make-string 0)))))
	    (lambda ()
	      (without-interrupts
	       (lambda ()
		 index))))))