;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/regops.scm,v 1.81 1991/03/22 00:32:43 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Region/Mark Operations

(declare (usual-integrations))

(define (region-insert! mark region)
  (let ((string (region->string region))
	(group (mark-group mark))
	(start (mark-index mark)))
    (let ((n (string-length string)))
      (group-insert-substring! group start string 0 n)
      (%make-region (%make-temporary-mark group start false)
		    (%make-temporary-mark group (+ start n) true)))))

(define (region-insert-string! mark string)
  (group-insert-substring! (mark-group mark) (mark-index mark)
			   string 0 (string-length string)))

(define (region-insert-substring! mark string start end)
  (group-insert-substring! (mark-group mark) (mark-index mark)
			   string start end))

(define (region-insert-newline! mark)
  (group-insert-char! (mark-group mark) (mark-index mark) #\newline))

(define (region-insert-char! mark char)
  (group-insert-char! (mark-group mark) (mark-index mark) char))

(define (region->string region)
  (group-extract-string (region-group region)
			(region-start-index region)
			(region-end-index region)))

(define (region-delete! region)
  (group-delete! (region-group region)
		 (region-start-index region)
		 (region-end-index region)))

(define (mark-left-char mark)
  (if (group-start? mark)
      (error "No left char: MARK-LEFT-CHAR" mark))
  (group-left-char (mark-group mark) (mark-index mark)))

(define (mark-right-char mark)
  (if (group-end? mark)
      (error "No right char: MARK-RIGHT-CHAR" mark))
  (group-right-char (mark-group mark) (mark-index mark)))

(define (mark-delete-left-char! mark)
  (if (group-start? mark)
      (error "No left char: MARK-DELETE-LEFT-CHAR!" mark))
  (group-delete-left-char! (mark-group mark) (mark-index mark)))

(define (mark-delete-right-char! mark)
  (if (group-end? mark)
      (error "No right char: MARK-DELETE-RIGHT-CHAR!" mark))
  (group-delete-right-char! (mark-group mark) (mark-index mark)))

;;; **** This is not a great thing to do.  It will screw up any marks
;;; that are within the region, pushing them to either side.
;;; Conceptually we just want the characters to be altered.

(define (region-transform! region operation)
  (let ((m (mark-permanent! (region-start region))))
    (let ((string (operation (region->string region))))
      (region-delete! region)
      (region-insert-string! m string))))

;;;; Clipping

(define (region-clip! region)
  (let ((group (region-group region))
	(start (mark-right-inserting (region-start region)))
	(end (mark-left-inserting (region-end region))))
    (record-clipping! group (mark-index start) (mark-index end))
    (vector-set! group group-index:start-mark start)
    (vector-set! group group-index:end-mark end)
    (vector-set! group group-index:display-start start)
    (vector-set! group group-index:display-end end))
  unspecific)

(define (group-un-clip! group)
  (let ((start (%make-permanent-mark group 0 false))
	(end (%make-permanent-mark group (group-length group) true)))
    (record-clipping! group 0 (group-length group))
    (vector-set! group group-index:start-mark start)
    (vector-set! group group-index:end-mark end)
    (vector-set! group group-index:display-start start)
    (vector-set! group group-index:display-end end))
  unspecific)

(define (with-region-clipped! new-region thunk)
  (let ((group (region-group new-region))
	(old-region))
    (dynamic-wind (lambda ()
		    (set! old-region (group-region group))
		    (region-clip! new-region)
		    (set! new-region)
		    unspecific)
		  thunk
		  (lambda ()
		    (set! new-region (group-region group))
		    (region-clip! old-region)
		    (set! old-region)
		    unspecific))))

(define (without-group-clipped! group thunk)
  (let ((old-region))
    (dynamic-wind (lambda ()
		    (set! old-region (group-region group))
		    (group-un-clip! group))
		  thunk
		  (lambda ()
		    (region-clip! old-region)
		    (set! old-region)
		    unspecific))))

(define (group-clipped? group)
  (not (and (zero? (group-start-index group))
	    (= (group-end-index group) (group-length group)))))

(define (group-unclipped-region group)
  (make-region (make-mark group 0)
	       (make-mark group (group-length group))))