#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/Attic/strnin.scm,v 14.2 1988/06/13 11:51:51 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; String I/O Ports
;;; package: (runtime string-input)

(declare (usual-integrations))

(define (initialize-package!)
  (set! input-string-template
	(make-input-port `((CHAR-READY? ,operation/char-ready?)
			   (DISCARD-CHAR ,operation/discard-char)
			   (DISCARD-CHARS ,operation/discard-chars)
			   (PEEK-CHAR ,operation/peek-char)
			   (PRINT-SELF ,operation/print-self)
			   (READ-CHAR ,operation/read-char)
			   (READ-STRING ,operation/read-string))
			 false)))

(define (with-input-from-string string thunk)
  (with-input-from-port (string->input-port string) thunk))

(define (string->input-port string #!optional start end)
  (input-port/copy input-string-template
		   (make-input-string-state
		    string
		    (if (default-object? start) 0 start)
		    (if (default-object? end) (string-length string) end))))

(define input-string-template)

(define-structure (input-string-state (type vector)
				      (conc-name input-string-state/))
  (string false read-only true)
  start
  (end false read-only true))

(define-integrable (input-port/string port)
  (input-string-state/string (input-port/state port)))

(define-integrable (input-port/start port)
  (input-string-state/start (input-port/state port)))

(define-integrable (set-input-port/start! port index)
  (set-input-string-state/start! (input-port/state port) index))

(define-integrable (input-port/end port)
  (input-string-state/end (input-port/state port)))

(define (operation/char-ready? port interval)
  interval
  (< (input-port/start port) (input-port/end port)))

(define (operation/peek-char port)
  (and (< (input-port/start port) (input-port/end port))
       (string-ref (input-port/string port) (input-port/start port))))

(define (operation/discard-char port)
  (set-input-port/start! port (1+ (input-port/start port))))

(define (operation/read-char port)
  (let ((start (input-port/start port)))
    (and (< start (input-port/end port))
	 (begin (set-input-port/start! port (1+ start))
		(string-ref (input-port/string port) start)))))

(define (operation/read-string port delimiters)
  (let ((start (input-port/start port))
	(end (input-port/end port)))
    (and (< start end)
	 (let ((string (input-port/string port)))
	   (let ((index
		  (or (substring-find-next-char-in-set string
						       start
						       end
						       delimiters)
		      end)))
	     (set-input-port/start! port index)
	     (substring string start index))))))

(define (operation/discard-chars port delimiters)
  (let ((start (input-port/start port))
	(end (input-port/end port)))
    (if (< start end)
	(set-input-port/start!
	 port
	 (or (substring-find-next-char-in-set (input-port/string port)
					      start
					      end
					      delimiters)
	     end)))))

(define (operation/print-self state port)
  port
  (unparse-string state "from string"))