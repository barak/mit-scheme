;;; -*-Scheme-*-
;;;
;;;	$Id: motion.scm,v 1.85 1993/01/12 10:50:40 cph Exp $
;;;
;;;	Copyright (c) 1985, 1989-93 Massachusetts Institute of Technology
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
	(else (error "Unknown limit type:" limit?))))

(define (mark1+ mark #!optional limit?)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (if (group-end-index? group index)
	(limit-mark-motion (and (not (default-object? limit?)) limit?)
			   (group-end-mark group))
	(make-mark group (fix:+ index 1)))))

(define (mark-1+ mark #!optional limit?)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (if (group-start-index? group index)
	(limit-mark-motion (and (not (default-object? limit?)) limit?)
			   (group-start-mark group))
	(make-mark group (fix:- index 1)))))

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

(define (line-start-index group index)
  (let ((limit (group-start-index group)))
    (let ((index (group-find-previous-char group limit index #\newline)))
      (if index
	  (fix:+ index 1)
	  limit))))

(define (line-end-index group index)
  (let ((limit (group-end-index group)))
    (or (group-find-next-char group index limit #\newline)
	limit)))

(define (line-start-index? group index)
  (or (group-start-index? group index)
      (char=? (group-left-char group index) #\newline)))

(define (line-end-index? group index)
  (or (group-end-index? group index)
      (char=? (group-right-char group index) #\newline)))

(define (line-start mark n #!optional limit?)
  (let ((group (mark-group mark))
	(lose
	 (lambda (mark)
	   (limit-mark-motion (and (not (default-object? limit?)) limit?)
			      mark))))
    (if (fix:> n 0)
	(let ((limit (group-end-index group)))
	  (let loop ((i (mark-index mark)) (n n))
	    (let ((j (group-find-next-char group i limit #\newline)))
	      (cond ((not j) (lose (group-end-mark group)))
		    ((fix:= n 1) (make-mark group (fix:+ j 1)))
		    (else (loop (fix:+ j 1) (fix:- n 1)))))))
	(let ((limit (group-start-index group)))
	  (let loop ((i (mark-index mark)) (n n))
	    (let ((j (group-find-previous-char group limit i #\newline)))
	      (cond ((fix:= n 0) (make-mark group (if j (fix:+ j 1) limit)))
		    ((not j) (lose (group-start-mark group)))
		    (else (loop j (fix:+ n 1))))))))))

(define (line-end mark n #!optional limit?)
  (let ((group (mark-group mark))
	(lose
	 (lambda (mark)
	   (limit-mark-motion (and (not (default-object? limit?)) limit?)
			      mark))))
    (if (fix:< n 0)
	(let ((limit (group-start-index group)))
	  (let loop ((i (mark-index mark)) (n n))
	    (let ((j (group-find-previous-char group limit i #\newline)))
	      (cond ((not j) (lose (group-start-mark group)))
		    ((fix:= n -1) (make-mark group j))
		    (else (loop j (fix:+ n 1)))))))
	(let ((limit (group-end-index group)))
	  (let loop ((i (mark-index mark)) (n n))
	    (let ((j (group-find-next-char group i limit #\newline)))
	      (cond ((fix:= n 0) (make-mark group (or j limit)))
		    ((not j) (lose (group-end-mark group)))
		    (else (loop (fix:+ j 1) (fix:- n 1))))))))))

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
    (cond ((fix:= start end) n)
	  ((group-find-next-char group start end #\newline)
	   => (lambda (i) (loop (fix:+ i 1) (fix:+ n 1))))
	  (else (fix:+ n 1)))))

;;;; Motion by Columns

(define (mark-column mark)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (group-columns group
		   (line-start-index group index)
		   index
		   0
		   (group-tab-width group))))

(define (move-to-column mark column)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (make-mark group
	       (vector-ref (group-column->index group
						(line-start-index group index)
						(group-end-index group)
						0
						column
						(group-tab-width group))
			   0))))