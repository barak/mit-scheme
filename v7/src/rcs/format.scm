#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/rcs/format.scm,v 1.2 1991/01/19 04:21:02 cph Exp $

Copyright (c) 1987, 1991 Massachusetts Institute of Technology

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