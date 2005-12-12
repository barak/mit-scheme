#| -*-Scheme-*-

$Id: strott.scm,v 14.14 2005/12/12 21:55:23 cph Exp $

Copyright 1988,1993,1999,2004,2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; String output ports (truncated)
;;; package: (runtime truncated-string-output)

(declare (usual-integrations))

(define (call-with-truncated-output-string limit generator)
  (call-with-current-continuation
   (lambda (k)
     (let ((port
	    (make-port output-string-port-type
		       (receive (sink extract extract!)
			   (make-accumulator-sink limit k)
			 (make-gstate #f sink 'TEXT extract extract!)))))
       (generator port)
       (cons #f (get-output-string port))))))

(define (with-output-to-truncated-string max thunk)
  (call-with-truncated-output-string max
    (lambda (port)
      (with-output-to-port port thunk))))

(define-structure (astate (type vector)
			  (initial-offset 4) ;must match "genio.scm"
			  (constructor #f))
  extract
  extract!)

(define output-string-port-type)
(define (initialize-package!)
  (set! output-string-port-type
	(make-port-type
	 `((EXTRACT-OUTPUT
	    ,(lambda (port)
	       (output-port/flush-output port)
	       ((astate-extract (port/state port)))))
	   (EXTRACT-OUTPUT!
	    ,(lambda (port)
	       (output-port/flush-output port)
	       ((astate-extract! (port/state port)))))
	   (WRITE-SELF
	    ,(lambda (port output-port)
	       port
	       (write-string " to string (truncating)" output-port))))
	 (generic-i/o-port-type #f #t)))
  unspecific)

(define (make-accumulator-sink limit k)
  (let ((chars #f)
	(index 0))

    (define (normal-case string start end n)
      (cond ((not chars)
	     (set! chars (new-chars 128 n)))
	    ((fix:> n (string-length chars))
	     (let ((new (new-chars (string-length chars) n)))
	       (substring-move! chars 0 index new 0)
	       (set! chars new))))
      (substring-move! string start end chars index)
      (set! index n)
      (fix:- end start))

    (define (new-chars start min-length)
      (make-string
       (let loop ((n start))
	 (cond ((fix:>= n limit) limit)
	       ((fix:>= n min-length) n)
	       (else (loop (fix:+ n n)))))))

    (define (limit-case string start)
      (let ((s
	     (cond ((not chars) (make-string limit))
		   ((fix:> limit (string-length chars))
		    (let ((s (make-string limit)))
		      (substring-move! chars 0 index s 0)
		      s))
		   (else chars))))
	(substring-move! string start (fix:+ start (fix:- limit index))
			 s index)
	(set! chars #f)
	(set! index 0)
	(k (cons #t s))))

    (values (make-non-channel-sink
	     (lambda (string start end)
	       (without-interrupts
		(lambda ()
		  (let ((n (fix:+ index (fix:- end start))))
		    (if (fix:<= n limit)
			(normal-case string start end n)
			(limit-case string start)))))))
	    (lambda ()
	      (if chars
		  (string-head chars index)
		  (make-string 0)))
	    (lambda ()
	      (without-interrupts
	       (lambda ()
		 (if chars
		     (let ((s chars))
		       (set! chars #f)
		       (set! index 0)
		       (set-string-maximum-length! s index)
		       s)
		     (make-string 0))))))))