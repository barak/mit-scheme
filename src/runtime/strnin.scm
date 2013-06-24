#| -*-Scheme-*-

$Id: strnin.scm,v 14.8 1999/02/24 21:36:21 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; String I/O Ports
;;; package: (runtime string-input)

(declare (usual-integrations))

(define (initialize-package!)
  (set! input-string-port-type
	(make-port-type `((CHAR-READY? ,operation/char-ready?)
			  (DISCARD-CHAR ,operation/discard-char)
			  (DISCARD-CHARS ,operation/discard-chars)
			  (PEEK-CHAR ,operation/peek-char)
			  (WRITE-SELF ,operation/write-self)
			  (READ-CHAR ,operation/read-char)
			  (READ-STRING ,operation/read-string))
			#f))
  unspecific)

(define (with-input-from-string string thunk)
  (with-input-from-port (string->input-port string) thunk))

(define (string->input-port string #!optional start end)
  (let ((end
	 (if (default-object? end)
	     (string-length string)
	     (check-index end (string-length string) 'STRING->INPUT-PORT))))
    (make-port
     input-string-port-type
     (make-input-string-state string
			      (if (default-object? start)
				  0
				  (check-index start end 'STRING->INPUT-PORT))
			      end))))

(define (check-index index limit procedure)
  (if (not (exact-nonnegative-integer? index))
      (error:wrong-type-argument index "exact non-negative integer" procedure))
  (if (not (<= index limit))
      (error:bad-range-argument index procedure))
  index)

(define input-string-port-type)

(define-structure (input-string-state (type vector)
				      (conc-name input-string-state/))
  (string #f read-only #t)
  start
  (end #f read-only #t))

(define-integrable (input-port/string port)
  (input-string-state/string (port/state port)))

(define-integrable (input-port/start port)
  (input-string-state/start (port/state port)))

(define-integrable (set-input-port/start! port index)
  (set-input-string-state/start! (port/state port) index))

(define-integrable (input-port/end port)
  (input-string-state/end (port/state port)))

(define (operation/char-ready? port interval)
  interval
  (fix:< (input-port/start port) (input-port/end port)))

(define (operation/peek-char port)
  (if (fix:< (input-port/start port) (input-port/end port))
      (string-ref (input-port/string port) (input-port/start port))
      (make-eof-object port)))

(define (operation/discard-char port)
  (set-input-port/start! port (fix:+ (input-port/start port) 1)))

(define (operation/read-char port)
  (let ((start (input-port/start port)))
    (if (fix:< start (input-port/end port))
	(begin
	  (set-input-port/start! port (fix:+ start 1))
	  (string-ref (input-port/string port) start))
	(make-eof-object port))))

(define (operation/read-string port delimiters)
  (let ((start (input-port/start port))
	(end (input-port/end port)))
    (if (fix:< start end)
	(let ((string (input-port/string port)))
	  (let ((index
		 (or (substring-find-next-char-in-set string
						      start
						      end
						      delimiters)
		     end)))
	    (set-input-port/start! port index)
	    (substring string start index)))
	(make-eof-object port))))

(define (operation/discard-chars port delimiters)
  (let ((start (input-port/start port))
	(end (input-port/end port)))
    (if (fix:< start end)
	(set-input-port/start!
	 port
	 (or (substring-find-next-char-in-set (input-port/string port)
					      start
					      end
					      delimiters)
	     end)))))

(define (operation/write-self port output-port)
  port
  (write-string " from string" output-port))