;;; -*-Scheme-*-
;;;
;;; $Id: test-imap.scm,v 1.1 2000/04/22 05:12:26 cph Exp $
;;;
;;; Copyright (c) 2000 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Manually interact with IMAP server

(declare (usual-integrations))

(define (open-imap-socket host user password)
  (let ((port (open-tcp-stream-socket host "imap2")))
    (let ((line (read-line port)))
      (write-string line)
      (newline)
      (let ((conn (make-imap-connection port)))
	(imap-command conn "LOGIN" user password)
	conn))))

(define (close-imap-socket conn)
  (close-port (imap-connection-port conn)))

(define (imap-command conn command . arguments)
  (let ((tag (apply send-imap-command conn command arguments))
	(port (imap-connection-port conn)))
    (let loop ()
      (let ((response (imap:read-server-response port)))
	(if (not (eof-object? response))
	    (begin
	      (pp response)
	      (if (not (and (memq (car response) '(OK NO BAD))
			    (equal? tag (cadr response))))
		  (loop))))))))

(define (send-imap-command conn command . arguments)
  (let ((tag (next-imap-command-tag conn))
	(port (imap-connection-port conn)))
    (let ((command
	   (decorated-string-append "" " " "" (cons* tag command arguments))))
      (write-string command port)
      (newline port)
      (write-string command)
      (newline))
    (flush-output port)
    tag))

(define (resynchronize-imap-socket conn tag)
  (let ((prefix (string-append tag " "))
	(port (imap-connection-port conn)))
    (let loop ()
      (let ((line (read-line port)))
	(if (not (eof-object? line))
	    (begin
	      (write-string line)
	      (newline)
	      (if (not (string-prefix? prefix line))
		  (loop))))))))

(define (next-imap-command-tag conn)
  (let ((n (imap-connection-sequence-number conn)))
    (set-imap-connection-sequence-number! conn (+ n 1))
    (string-append "A" (string-pad-left (number->string n) 4 #\0))))

(define-structure (imap-connection (constructor make-imap-connection (port)))
  (port #f read-only #t)
  (sequence-number 0))