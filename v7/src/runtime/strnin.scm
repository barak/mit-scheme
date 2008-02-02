#| -*-Scheme-*-

$Id: strnin.scm,v 14.23 2008/02/02 02:02:51 cph Exp $

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

;;;; String Input Ports (SRFI-6)
;;; package: (runtime string-input)

(declare (usual-integrations))

(define (with-input-from-string string thunk)
  (with-input-from-port (open-input-string string) thunk))

(define (open-input-string string #!optional start end)
  (guarantee-string string 'OPEN-INPUT-STRING)
  (let ((port
	 (let* ((end
		 (if (or (default-object? end) (not end))
		     (string-length string)
		     (guarantee-substring-end-index end (string-length string)
						    'OPEN-INPUT-STRING)))
		(start
		 (if (or (default-object? start) (not start))
		     0
		     (guarantee-substring-start-index start end
						      'OPEN-INPUT-STRING))))
	   (make-generic-i/o-port (make-string-source string start end)
				  #f
				  input-string-port-type))))
    (port/set-coding port 'ISO-8859-1)
    (port/set-line-ending port 'NEWLINE)
    port))

(define (call-with-input-string string procedure)
  (let ((port (open-input-string string)))
    (let ((value (procedure port)))
      (close-input-port port)
      value)))

(define (make-string-source string start end)
  (let ((index start))
    (make-non-channel-port-source
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