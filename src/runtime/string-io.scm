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

;;;; String I/O Ports (SRFI-6)
;;; package: (runtime string-i/o-port)

(declare (usual-integrations))

;;;; Input

;; obsolete
(define (with-input-from-string string thunk)
  (parameterize ((current-input-port (open-input-string string)))
    (thunk)))

(define (call-with-input-string string procedure)
  (procedure (open-input-string string)))

(define (open-input-string string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'open-input-string))
	 (start (fix:start-index start end 'open-input-string)))
    (make-textual-port string-input-type
		       (make-istate string start end start 0))))

(define-record-type <istate>
    (make-istate string start end next line-number)
    istate?
  (string istate-string)
  (start istate-start)
  (end istate-end)
  (next istate-next set-istate-next!)
  (line-number istate-line-number set-istate-line-number!))

(define-deferred string-input-type
  (make-textual-port-type `((char-ready? ,string-in/char-ready?)
			    (eof? ,string-in/eof?)
			    (input-line ,string-in/input-line)
			    (peek-char ,string-in/peek-char)
			    (position ,string-in/position)
			    (read-char ,string-in/read-char)
			    (read-substring ,string-in/read-substring)
			    (unread-char ,string-in/unread-char)
			    (write-self ,string-in/write-self))
			  #f))

(define (string-in/char-ready? port)
  port
  #t)

(define (string-in/eof? port)
  (let ((ss (textual-port-state port)))
    (not (fix:< (istate-next ss) (istate-end ss)))))

(define (string-in/position port)
  (let ((ss (textual-port-state port)))
    (istate-next ss)))

(define (string-in/peek-char port)
  (let ((ss (textual-port-state port)))
    (if (fix:< (istate-next ss) (istate-end ss))
	(string-ref (istate-string ss) (istate-next ss))
	(eof-object))))

(define (string-in/read-char port)
  (let ((ss (textual-port-state port)))
    (if (fix:< (istate-next ss) (istate-end ss))
	(let ((char (string-ref (istate-string ss) (istate-next ss))))
	  (set-istate-next! ss (fix:+ (istate-next ss) 1))
	  (if (char=? char #\newline)
	      (set-istate-line-number! ss (fix:+ 1 (istate-line-number ss))))
	  char)
	(eof-object))))

(define (string-in/input-line port)
  (istate-line-number (textual-port-state port)))

(define (string-in/read-substring port string start end)
  (let ((ss (textual-port-state port)))
    (let ((string* (istate-string ss))
	  (start* (istate-next ss))
	  (end* (istate-end ss)))
      (if (fix:= start* end*)
	  0
	  (let ((n (fix:min (fix:- end start) (fix:- end* start*))))
	    (string-copy! string start string* start* (fix:+ start* n))
	    (set-istate-next! ss (fix:+ start* n))
	    n)))))

(define (string-in/unread-char port char)
  (let ((ss (textual-port-state port)))
    (if (not (fix:< (istate-start ss) (istate-next ss)))
	(error "No char to unread:" port))
    (let ((prev (fix:- (istate-next ss) 1)))
      (if (not (char=? char (string-ref (istate-string ss) prev)))
	  (error "Unread char incorrect:" char))
      (set-istate-next! ss prev))))

(define (string-in/write-self port output-port)
  port
  (write-string " from string" output-port))

;;;; Output

(define (get-output-string port)
  ((textual-port-operation port 'extract-output) port))

(define (get-output-string! port)
  ((textual-port-operation port 'extract-output!) port))

(define (call-with-output-string generator)
  (let ((port (open-output-string)))
    (generator port)
    (get-output-string port)))

(define (call-with-truncated-output-string limit generator)
  (let ((port (open-output-string)))
    (let ((truncated? (call-with-truncated-output-port limit port generator)))
      (cons truncated? (get-output-string port)))))

;; deprecated
(define (with-output-to-string thunk)
  (call-with-output-string
    (lambda (port)
      (parameterize ((current-output-port port))
	(thunk)))))

;; deprecated
(define (with-output-to-truncated-string limit thunk)
  (call-with-truncated-output-string limit
    (lambda (port)
      (parameterize ((current-output-port port))
	(thunk)))))

(define (open-output-string)
  (make-textual-port string-output-type (make-ostate (string-builder) 0)))

(define-record-type <ostate>
    (make-ostate builder column)
    ostate?
  (builder ostate-builder)
  (column ostate-column set-ostate-column!))

(define-deferred string-output-type
  (make-textual-port-type `((write-char ,string-out/write-char)
			    (write-substring ,string-out/write-substring)
			    (extract-output ,string-out/extract-output)
			    (extract-output! ,string-out/extract-output!)
			    (output-column ,string-out/output-column)
			    (position ,string-out/position)
			    (write-self ,string-out/write-self))
			  #f))

(define (string-out/write-char port char)
  (let ((os (textual-port-state port)))
    ((ostate-builder os) char)
    (set-ostate-column! os (new-column char (ostate-column os)))
    1))

(define (string-out/write-substring port string start end)
  (let ((os (textual-port-state port))
	(n (fix:- end start)))
    ((ostate-builder os) (string-copy string start end))
    (update-column-for-substring! os string start end)
    n))

(define (string-out/extract-output port)
  ((ostate-builder (textual-port-state port))))

(define (string-out/extract-output! port)
  (let* ((os (textual-port-state port))
	 (builder (ostate-builder os))
	 (output (builder)))
    (builder 'reset!)
    (set-ostate-column! os 0)
    output))

(define (string-out/output-column port)
  (ostate-column (textual-port-state port)))

(define (string-out/position port)
  ((ostate-builder (textual-port-state port)) 'count))

(define (string-out/write-self port output-port)
  port
  (write-string " to string" output-port))

(define (new-column char column)
  (case char
    ((#\newline) 0)
    ((#\tab) (fix:+ column (fix:- 8 (fix:remainder column 8))))
    (else (fix:+ column 1))))

(define (update-column-for-substring! os string start end)
  (letrec
      ((loop
	(lambda (i column)
	  (if (fix:< i end)
	      (loop (fix:+ i 1)
		    (new-column (string-ref string i) column))
	      (set-ostate-column! os column)))))
    (let ((nl (string-find-previous-char string #\newline start end)))
      (if nl
	  (loop (fix:+ nl 1) 0)
	  (loop start (ostate-column os))))))