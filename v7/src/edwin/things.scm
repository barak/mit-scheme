;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/things.scm,v 1.78 1989/04/28 22:53:57 cph Rel $
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

;;;; Textual Entities

(declare (usual-integrations))

;;;; Motion Primitives

;;; This file "defines" various kinds of things like lines, pages,
;;; words, etc.  The "definition" of a FOO entity consists of two
;;; procedures, FORWARD-FOO and BACKWARD-FOO, each of which takes
;;; three arguments: [1] a mark to start from, [2] the number of FOOs
;;; to traverse, and [3] a limit for LIMIT-MARK-MOTION.  The value of
;;; the procedure should be either a mark or #F.

;;; If the number is positive, traverse that many FOOs in the given
;;; direction; if negative, in the opposite direction; and zero means
;;; don't move.  It is assumed that no two FOOs overlap; they may or
;;; may not touch one another.  When moving forward, stop to the right
;;; of the rightmost edge of the FOO.  When moving backward, stop to
;;; the left of the leftmost edge.

;;; MAKE-MOTION-PAIR will generate these two procedures, given the
;;; simpler primitives to move forward or backward once.

(define (make-motion-pair forward-one-thing backward-one-thing receiver)
  (define (forward-thing mark n #!optional limit?)
    (let ((limit? (and (not (default-object? limit?)) limit?)))
      (cond ((positive? n) (%forward-thing mark n limit?))
	    ((negative? n) (%backward-thing mark (- n) limit?))
	    (else mark))))

  (define (backward-thing mark n #!optional limit?)
    (let ((limit? (and (not (default-object? limit?)) limit?)))
      (cond ((positive? n) (%backward-thing mark n limit?))
	    ((negative? n) (%forward-thing mark (- n) limit?))
	    (else mark))))

  (define (%forward-thing mark n limit?)
    (let loop ((mark mark) (n n))
      (let ((end (forward-one-thing mark)))
	(cond ((not end) (limit-mark-motion limit? mark))
	      ((= n 1) end)
	      (else (loop end (-1+ n)))))))

  (define (%backward-thing mark n limit?)
    (let loop ((mark mark) (n n))
      (let ((start (backward-one-thing mark)))
	(cond ((not start) (limit-mark-motion limit? mark))
	      ((= n 1) start)
	      (else (loop start (-1+ n)))))))

  (receiver forward-thing backward-thing))

;;;; Generic Operations

(define (move-thing forward-thing argument)
  (set-current-point! (forward-thing (current-point) argument 'FAILURE)))

(define (move-thing-saving-point forward-thing argument)
  (let ((mark (current-point)))
    (push-current-mark! mark)
    (set-current-point! (forward-thing mark argument 'FAILURE))))

(define (mark-thing forward-thing n)
  (push-current-mark! (forward-thing (current-point) n 'ERROR)))

(define (kill-thing forward-thing n)
  (kill-region (forward-thing (current-point) n 'ERROR)))

(define (transpose-things forward-thing n)
  (define (forward-once i)
    i					;ignore
    (let ((m4 (mark-right-inserting (forward-thing (current-point) 1 'ERROR))))
      (set-current-point! m4)
      (let ((m2 (mark-permanent! (forward-thing m4 -1 'ERROR))))
	(let ((m1 (mark-permanent! (forward-thing m2 -1 'ERROR))))
	  (let ((m3 (forward-thing m1 1 'ERROR)))
	    (region-insert! m4 (region-extract! (make-region m1 m3)))
	    (region-insert! m1 (region-extract! (make-region m2 m4))))))))

  (define (backward-once i)
    i					;ignore
    (let ((m2 (mark-permanent! (forward-thing (current-point) -1 'ERROR))))
      (let ((m1 (mark-left-inserting (forward-thing m2 -1 'ERROR))))
	(let ((m3 (forward-thing m1 1 'ERROR))
	      (m4 (mark-right-inserting (forward-thing m2 1 'ERROR))))
	    (region-insert! m4 (region-extract! (make-region m1 m3)))
	    (region-insert! m1 (region-extract! (make-region m2 m4))))
	(set-current-point! m1))))

  (define (special)
    (let ((m1 (normalize (current-point)))
	  (m2 (normalize (current-mark))))
      (cond ((mark< m1 m2)
	     (exchange m1 m2
		       (lambda (m1 m2)
			 (set-current-point! m2)
			 (set-current-mark! m1))))
	    ((mark< m2 m1)
	     (exchange m2 m1
		       (lambda (m2 m1)
			 (set-current-point! m2)
			 (set-current-mark! m1)))))))

  (define (exchange m1 m2 receiver)
    (let ((m1 (mark-right-inserting m1))
	  (m3 (forward-thing m1 1 'ERROR))
	  (m2 (mark-permanent! m2))
	  (m4 (mark-right-inserting (forward-thing m2 1 'ERROR))))
      (region-insert! m4 (region-extract! (make-region m1 m3)))
      (region-insert! m1 (region-extract! (make-region m2 m4)))
      (receiver m4 m1)))

  (define (normalize m)
    (forward-thing (forward-thing m 1 'ERROR) -1 'ERROR))

  (cond ((positive? n) (dotimes n forward-once))
	((negative? n) (dotimes (- n) backward-once))
	(else (special))))

;;;; Horizontal Space

(define (horizontal-space-region mark)
  (make-region (horizontal-space-start mark)
	       (horizontal-space-end mark)))

(define (horizontal-space-start mark)
  (skip-chars-backward " \t" mark (line-start mark 0)))

(define (horizontal-space-end mark)
  (skip-chars-forward " \t" mark (line-end mark 0)))

(define (compute-horizontal-space c1 c2 receiver)
  ;; Compute the number of tabs/spaces required to fill from column C1
  ;; to C2 with whitespace.  It is assumed that C1 >= C2.
  (if (ref-variable indent-tabs-mode)
      (let ((tab-width (ref-variable tab-width)))
	(let ((qr1 (integer-divide c1 tab-width))
	      (qr2 (integer-divide c2 tab-width)))
	  (if (> (integer-divide-quotient qr1) (integer-divide-quotient qr2))
	      (receiver (- (integer-divide-quotient qr1)
			   (integer-divide-quotient qr2))
			(integer-divide-remainder qr1))
	      (receiver 0
			(- (integer-divide-remainder qr1)
			   (integer-divide-remainder qr2))))))
      (receiver 0 (- c2 c1))))

(define (insert-horizontal-space target-column #!optional point)
  (let ((point
	 (if (default-object? point)
	     (current-point)
	     (mark-left-inserting point))))
    (compute-horizontal-space target-column (mark-column point)
      (lambda (n-tabs n-spaces)
	(insert-chars #\Tab n-tabs point)
	(insert-chars #\Space n-spaces point)))))

(define (delete-horizontal-space #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (delete-string (horizontal-space-start point)
		   (horizontal-space-end point))))

(define (region-blank? region)
  (not (skip-chars-forward " \t"
			   (region-start region)
			   (region-end region)
			   false)))

(define (line-blank? mark)
  (not (skip-chars-forward " \t"
			   (line-start mark 0)
			   (line-end mark 0)
			   false)))

(define (find-previous-blank-line mark)
  (let ((start (line-start mark -1)))
    (and start
	 (let loop ((mark start))
	   (cond ((line-blank? mark) mark)
		 ((group-start? mark) false)
		 (else (loop (line-start mark -1))))))))

(define (find-next-blank-line mark)
  (let ((start (line-start mark 1)))
    (and start
	 (let loop ((mark start))
	   (cond ((line-blank? mark) mark)
		 ((group-start? mark) false)
		 (else (loop (line-start mark 1))))))))

(define (find-previous-non-blank-line mark)
  (let ((start (line-start mark -1)))
    (and start
	 (let loop ((mark start))
	   (cond ((not (line-blank? mark)) mark)
		 ((group-start? mark) false)
		 (else (loop (line-start mark -1))))))))

(define (find-next-non-blank-line mark)
  (let ((start (line-start mark 1)))
    (and start
	 (let loop ((mark start))
	   (cond ((not (line-blank? mark)) mark)
		 ((group-start? mark) false)
		 (else (loop (line-start mark 1))))))))

;;;; Indentation

(define (maybe-change-indentation indentation #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (if (not (= indentation (mark-indentation point)))
	(change-indentation indentation point))))

(define (change-indentation indentation point)
  (change-column indentation (line-start point 0)))

(define (current-indentation #!optional point)
  (mark-indentation (if (default-object? point) (current-point) point)))

(define (mark-indentation mark)
  (mark-column (indentation-end mark)))

(define (indentation-end mark)
  (horizontal-space-end (line-start mark 0)))

(define (within-indentation? mark)
  (line-start? (horizontal-space-start mark)))

(define (maybe-change-column column #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (if (not (= column (mark-column point)))
	(change-column column point))))

(define (change-column column point)
  (let ((point (mark-permanent! point)))
    (delete-horizontal-space point)
    (insert-horizontal-space column point)))

;;;; Lines

(define forward-line)
(define backward-line)
(let ((%backward-line
       (lambda (mark n limit?)
	 (line-start mark
		     (if (line-start? mark) (- n) (- 1 n))
		     limit?))))
  (set! forward-line
	(lambda (mark n #!optional limit?)
	  (let ((limit? (and (not (default-object? limit?)) limit?)))
	    (cond ((positive? n) (line-start mark n limit?))
		  ((negative? n) (%backward-line mark (- n) limit?))
		  (else mark)))))
  (set! backward-line
	(lambda (mark n #!optional limit?)
	  (let ((limit? (and (not (default-object? limit?)) limit?)))
	    (cond ((positive? n) (%backward-line mark n limit?))
		  ((negative? n) (line-start mark (- n) limit?))
		  (else mark)))))
  unspecific)