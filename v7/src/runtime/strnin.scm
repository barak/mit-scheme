#| -*-Scheme-*-

$Id: strnin.scm,v 14.18 2006/06/21 03:00:11 cph Exp $

Copyright 1988,1990,1993,1999,2003,2004 Massachusetts Institute of Technology
Copyright 2005,2006 Massachusetts Institute of Technology

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

;;;; String Input Ports (SRFI-6)
;;; package: (runtime string-input)

(declare (usual-integrations))

(define (with-input-from-string string thunk)
  (with-input-from-port (open-input-string string) thunk))

(define (open-input-string string #!optional start end)
  (guarantee-string string 'OPEN-INPUT-STRING)
  (let* ((end
	  (if (or (default-object? end) (not end))
	      (string-length string)
	      (guarantee-substring-end-index end (string-length string)
					     'OPEN-INPUT-STRING)))
	 (start
	  (if (or (default-object? start) (not start))
	      0
	      (guarantee-substring-start-index start end 'OPEN-INPUT-STRING))))
    (make-port input-string-port-type
	       (make-gstate (make-string-source string start end)
			    #f
			    'ISO-8859-1
			    'NEWLINE))))

(define (call-with-input-string string procedure)
  (let ((port (open-input-string string)))
    (let ((value (procedure port)))
      (close-input-port port)
      value)))

(define (make-string-source string start end)
  (let ((index start))
    (make-non-channel-source
     (lambda ()
       (fix:< index end))
     (lambda (string* start* end*)
       (let ((n
	      (fix:min (fix:- end index)
		       (fix:- end* start*))))
	 (let ((limit (fix:+ index n)))
	   (substring-move! string index limit string* start*)
	   (set! index limit))
	 n)))))

(define input-string-port-type)
(define (initialize-package!)
  (set! input-string-port-type
	(make-port-type
	 `((WRITE-SELF
	    ,(lambda (port output-port)
	       port
	       (write-string " from string" output-port))))
	 (generic-i/o-port-type #t #f)))
  unspecific)