;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/motion.scm,v 1.81 1989/08/14 09:22:53 cph Exp $
;;;
;;;	Copyright (c) 1985, 1989 Massachusetts Institute of Technology
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

;;;; Motion within Groups

(declare (usual-integrations))

;;;; Motion by Characters

(define (limit-mark-motion limit? limit)
  (cond ((eq? limit? 'LIMIT) limit)
	((eq? limit? 'BEEP) (editor-beep) limit)
	((eq? limit? 'FAILURE) (editor-failure) limit)
	((eq? limit? 'ERROR) (editor-error))
	((not limit?) false)
	(else (error "Unknown limit type" limit?))))

(define (mark1+ mark #!optional limit?)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (if (group-end-index? group index)
	(limit-mark-motion (and (not (default-object? limit?)) limit?)
			   (group-end-mark group))
	(make-mark group (fix:1+ index)))))

(define (mark-1+ mark #!optional limit?)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (if (group-start-index? group index)
	(limit-mark-motion (and (not (default-object? limit?)) limit?)
			   (group-start-mark group))
	(make-mark group (fix:-1+ index)))))

(define (region-count-chars region)
  (fix:- (region-end-index region) (region-start-index region)))

(define mark+)
(define mark-)
(let ()

(set! mark+
(named-lambda (mark+ mark n #!optional limit?)
  (let ((limit? (and (not (default-object? limit?)) limit?)))
    (cond ((fix:positive? n) (%mark+ mark n limit?))
	  ((fix:negative? n) (%mark- mark (fix:- 0 n) limit?))
	  (else mark)))))

(set! mark-
(named-lambda (mark- mark n #!optional limit?)
  (let ((limit? (and (not (default-object? limit?)) limit?)))
    (cond ((fix:positive? n) (%mark- mark n limit?))
	  ((fix:negative? n) (%mark+ mark (fix:- 0 n) limit?))
	  (else mark)))))

(define (%mark+ mark n limit?)
  (let ((group (mark-group mark))
	(new-index (fix:+ (mark-index mark) n)))
    (if (fix:> new-index (group-end-index group))
	(limit-mark-motion limit? (group-end-mark group))
	(make-mark group new-index))))

(define (%mark- mark n limit?)
  (let ((group (mark-group mark))
	(new-index (fix:- (mark-index mark) n)))
    (if (fix:< new-index (group-start-index group))
	(limit-mark-motion limit? (group-start-mark group))
	(make-mark group new-index))))

)

;;;; Motion by Lines

;;; Move to the beginning of the Nth line, starting from INDEX in
;;; GROUP, where positive N means down, negative N means up, and zero
;;; N means the current line.  If such a line exists, call IF-OK on
;;; the position (of the line's start), otherwise call IF-NOT-OK on
;;; the limiting mark (the group's start or end) which was exceeded.

(define (move-vertically group index n if-ok if-not-ok)
  (cond ((fix:positive? n)
	 (let ((limit (group-end-index group)))
	   (let loop ((i index) (n n))
	     (let ((j (%find-next-newline group i limit)))
	       (cond ((not j) (if-not-ok (group-end-mark group)))
		     ((fix:= n 1) (if-ok (fix:1+ j)))
		     (else (loop (fix:1+ j) (fix:-1+ n))))))))
	((fix:negative? n)
	 (let ((limit (group-start-index group)))
	   (let loop ((i index) (n n))
	     (let ((j (%find-previous-newline group i limit)))
	       (cond ((fix:zero? n) (if-ok (or j limit)))
		     ((not j) (if-not-ok (group-start-mark group)))
		     (else (loop (fix:-1+ j) (fix:1+ n))))))))
	(else
	 (if-ok (line-start-index group index)))))

(define (line-start-index group index)
  (let ((limit (group-start-index group)))
    (or (%find-previous-newline group index limit)
	limit)))

(define (line-end-index group index)
  (let ((limit (group-end-index group)))
    (or (%find-next-newline group index limit)
	limit)))

(define (line-start-index? group index)
  (or (group-start-index? group index)
      (char=? (group-left-char group index) #\newline)))

(define (line-end-index? group index)
  (or (group-end-index? group index)
      (char=? (group-right-char group index) #\newline)))

(define (line-start mark n #!optional limit?)
  (let ((group (mark-group mark)))
    (move-vertically group (mark-index mark) n
      (lambda (index)
	(make-mark group index))
      (lambda (mark)
	(limit-mark-motion (and (not (default-object? limit?)) limit?)
			   mark)))))

(define (line-end mark n #!optional limit?)
  (let ((group (mark-group mark)))
    (move-vertically group (mark-index mark) n
      (lambda (index)
	(let ((end (%find-next-newline group index (group-end-index group))))
	  (if end
	      (make-mark group end)
	      (group-end-mark group))))
      (lambda (mark)
	(limit-mark-motion (and (not (default-object? limit?)) limit?)
			   mark)))))

(define (line-start? mark)
  (line-start-index? (mark-group mark) (mark-index mark)))

(define (line-end? mark)
  (line-end-index? (mark-group mark) (mark-index mark)))

(define (region-count-lines region)
  (group-count-lines (region-group region)
		     (region-start-index region)
		     (region-end-index region)))

(define (group-count-lines group start end)
  (let loop ((start start) (n 0))
    (if (fix:= start end)
	n
	(let ((i (%find-next-newline group start end))
	      (n (fix:1+ n)))
	  (if (not i)
	      n
	      (loop (fix:1+ i) n))))))

;;;; Motion by Columns

(define (mark-column mark)
  (group-index->column (mark-group mark) (mark-index mark)))

(define (move-to-column mark column)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (make-mark group
	       (group-column->index group
				    (line-start-index group index)
				    (line-end-index group index)
				    0
				    column))))

(define (group-index->column group index)
  (group-column-length group (line-start-index group index) index 0))

(define (group-column-length group start-index end-index start-column)
  (if (fix:= start-index end-index)
      0
      (let ((start (group-index->position group start-index true))
	    (end (group-index->position group end-index false))
	    (gap-start (group-gap-start group))
	    (gap-end (group-gap-end group))
	    (text (group-text group)))
	(if (and (not (fix:> start gap-start))
		 (not (fix:> gap-end end)))
	    (substring-column-length text gap-end end
	      (substring-column-length text start gap-start start-column))
	    (substring-column-length text start end start-column)))))

(define (group-column->index group start-index end-index start-column column)
  (if (fix:= start-index end-index)
      start-index
      (let ((start (group-index->position group start-index true))
	    (end (group-index->position group end-index false))
	    (gap-start (group-gap-start group))
	    (gap-end (group-gap-end group))
	    (text (group-text group)))
	(cond ((not (fix:> end gap-start))
	       (substring-column->index text start end start-column column))
	      ((not (fix:< start gap-end))
	       (fix:- (substring-column->index text start end
					       start-column column)
		      (group-gap-length group)))
	      (else
	       (substring-column->index text start gap-start
					start-column column
		 (lambda (gap-column)
		   (fix:- (substring-column->index text gap-end end
						   gap-column column)
			  (group-gap-length group)))))))))