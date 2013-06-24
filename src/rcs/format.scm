#| -*-Scheme-*-

$Id: format.scm,v 1.3 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1987, 1991, 1999 Massachusetts Institute of Technology

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

;;;; RCS Format

(declare (usual-integrations))

(define (rcs/format rcstext)
  (let ((head (rcstext/head rcstext)))
    (write-string "head:            ")
    (write-string (delta/number head))
    (write-string "\nlocks:         ")
    (if (null? (rcstext/locks rcstext))
	(write-string "  ")
	(for-each format/lock (rcstext/locks rcstext)))
    (write-string ";")
    (if (rcstext/strict? rcstext)
	(write-string "  strict"))
    (write-string "\naccess list:   ")
    (for-each format/user (rcstext/access rcstext))
    (write-string "\nsymbolic names:")
    (for-each format/symbol (rcstext/symbols rcstext))
    (write-string "\ncomment leader:  \"")
    (write-string (rcstext/comment rcstext))
    (write-string "\"")
    (write-string "\ndescription:\n")
    (format/delta-trunk head)
    (format/delta-tree head)
    (write-string "=============================================================================\n")))

(define (format/lock lock)
  (write-string "  ")
  (write-string (car lock))
  (write-string ": ")
  (write-string (delta/number (cdr lock))))

(define (format/user user)
  (write-string "  ")
  (write-string user))

(define (format/symbol symbol)
  (write-string "  ")
  (write-string (car symbol))
  (write-string ": ")
  (write-string (delta/number (cdr symbol))))

(define (format/delta-trunk head)
  (let loop ((delta head))
    (if delta
	(begin
	  (format/delta delta)
	  (loop (delta/next delta))))))

(define (format/delta-tree head)
  (if head
      (begin
	(format/delta-tree (delta/next head))
	(format/delta-forest (delta/branches head)))))

(define (format/delta-forest branches)
  (if (not (null? branches))
      (begin
	(format/delta-forest (cdr branches))
	(format/delta-branch (car branches))
	(format/delta-tree (car branches)))))

(define (format/delta-branch branch)
  (if branch
      (begin
	(format/delta-branch (delta/next branch))
	(format/delta branch))))

(define (format/delta delta)
  (write-string "----------------------------\nrevision ")
  (write-string (delta/number delta))
  (write-string "\ndate: ")
  (format/date (delta/date delta))
  (write-string ";  author: ")
  (write-string (delta/author delta))
  (write-string ";  state: ")
  (write-string (delta/state delta))
  (newline)
  (write-string (delta/log delta)))

(define (format/date date)
  (write-string (date->string date)))