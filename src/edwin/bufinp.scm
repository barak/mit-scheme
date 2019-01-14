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

;;;; Buffer Input Ports

(declare (usual-integrations))

(define (with-input-from-mark mark thunk #!optional receiver)
  (let ((port (make-buffer-input-port mark (group-end mark))))
    (let ((value
	   (parameterize ((current-input-port port))
	     (thunk))))
      (if (default-object? receiver)
	  value
	  (receiver value (input-port/mark port))))))

(define (with-input-from-region region thunk)
  (parameterize ((current-input-port
		  (make-buffer-input-port (region-start region)
					  (region-end region))))
    (thunk)))

(define (call-with-input-mark mark procedure)
  (procedure (make-buffer-input-port mark (group-end mark))))

(define (call-with-input-region region procedure)
  (procedure
   (make-buffer-input-port (region-start region) (region-end region))))

(define (make-buffer-input-port start end)
  ;; This uses indices, so it can only be used locally
  ;; where there is no buffer-modification happening.
  (make-port buffer-input-port-type
	     (make-bstate (mark-group start)
			  (mark-index start)
			  (mark-index end)
			  (mark-index start))))

(define (input-port/mark port)
  (let ((operation (textual-port-operation port 'BUFFER-MARK)))
    (if (not operation)
	(error:bad-range-argument port 'INPUT-PORT/MARK))
    (operation port)))

(define-structure bstate
  (group #f read-only #t)
  (start #f read-only #t)
  (end #f read-only #t)
  (index #f))

(define buffer-input-port-type
  (make-port-type
   `((BUFFER-MARK
      ,(lambda (port)
	(let ((state (port/state port)))
	  (make-mark (bstate-group state)
		     (bstate-index state)))))
     (CHAR-READY?
      ,(lambda (port)
	 (let ((state (port/state port)))
	   (fix:< (bstate-index state)
		  (bstate-end state)))))
     (PEEK-CHAR
      ,(lambda (port)
	 (let ((state (port/state port)))
	   (let ((index (bstate-index state)))
	     (if (fix:< index (bstate-end state))
		 (group-right-char (bstate-group state) index)
		 (eof-object))))))
     (READ-CHAR
      ,(lambda (port)
	 (let ((state (port/state port)))
	   (let ((index (bstate-index state)))
	     (if (fix:< index (bstate-end state))
		 (let ((char (group-right-char (bstate-group state) index)))
		   (set-bstate-index! state (fix:+ index 1))
		   char)
		 (eof-object))))))
     (UNREAD-CHAR
      ,(lambda (port char)
	 (let ((state (port/state port)))
	   (let ((index (bstate-index state)))
	     (if (fix:<= index (bstate-start state))
		 (error "No character to unread:" port))
	     (if (not (char=? (group-left-char (bstate-group state) index)
			      char))
		 (error "Incorrect char being unread:" char))
	     (set-bstate-index! state (fix:- index 1))))))
     (WRITE-SELF
      ,(lambda (port output)
	 (write-string " from buffer at " output)
	 (write (input-port/mark port) output))))
   #f))