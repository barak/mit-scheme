;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/simple.scm,v 1.42 1992/02/19 00:01:59 cph Exp $
;;;
;;;	Copyright (c) 1985, 1989-92 Massachusetts Institute of Technology
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

;;;; Simple Editing Procedures

(declare (usual-integrations))

(define (insert-char char #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-insert-char! (mark-group point) (mark-index point) char)))

(define (insert-chars char n #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (cond ((= n 1)
	   (group-insert-char! (mark-group point) (mark-index point) char))
	  ((> n 1)
	   (group-insert-substring! (mark-group point) (mark-index point)
				    (make-string n char) 0 n)))))

(define (insert-newline #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-insert-char! (mark-group point) (mark-index point) #\newline)))

(define (insert-newlines n #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (cond ((= n 1)
	   (group-insert-char! (mark-group point) (mark-index point)
			       #\newline))
	  ((> n 1)
	   (group-insert-substring! (mark-group point) (mark-index point)
				    (make-string n #\newline) 0 n)))))

(define (guarantee-newline #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (if (not (line-start? point))
	(insert-newline point))))

(define (guarantee-newlines n #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let loop ((n n) (mark point))
      (if (> n 0)
	  (if (line-start? mark)
	      (loop (- n 1) (mark-1+ mark))
	      (insert-newlines n point))))))

(define (extract-left-char #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group point))
	  (index (mark-index point)))
      (and (not (group-start-index? group index))
	   (group-left-char group index)))))

(define (extract-right-char #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group point))
	  (index (mark-index point)))
      (and (not (group-end-index? group index))
	   (group-right-char group index)))))

(define (delete-left-char #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group point))
	  (index (mark-index point)))
      (if (group-start-index? group index)
	  (editor-error "Attempt to delete past start of buffer")
	  (group-delete-left-char! group index)))))

(define (delete-right-char #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group point))
	  (index (mark-index point)))
      (if (group-end-index? group index)
	  (editor-error "Attempt to delete past end of buffer")
	  (group-delete-right-char! group index)))))

(define (insert-string string #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-insert-string! (mark-group point) (mark-index point) string)))

(define (insert-substring string start end #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-insert-substring! (mark-group point) (mark-index point)
			     string start end)))

(define (insert-region start end #!optional point)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (let ((point (if (default-object? point) (current-point) point)))
    (if (mark~ start point)
	(insert-string (extract-string start end) point)
	(let ((group (mark-group start))
	      (start (mark-index start))
	      (end (mark-index end)))
	  (let ((text (group-text group))
		(gap-start (group-gap-start group))
		(gap-end (group-gap-end group))
		(gap-length (group-gap-length group)))
	    (cond ((<= end gap-start)
		   (group-insert-substring! (mark-group point)
					    (mark-index point)
					    text start end))
		  ((<= gap-start start)
		   (group-insert-substring! (mark-group point)
					    (mark-index point)
					    text
					    (+ start gap-length)
					    (+ end gap-length)))
		  (else
		   (let ((point (mark-left-inserting-copy point)))
		     (group-insert-substring! (mark-group point)
					      (mark-index point)
					      text start gap-start)
		     (group-insert-substring! (mark-group point)
					      (mark-index point)
					      text gap-end
					      (+ end gap-length))
		     (mark-temporary! point)))))))))

(define (extract-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group mark))
	  (index1 (mark-index mark))
	  (index2 (mark-index point)))
      (if (not (eq? group (mark-group point)))
	  (error "Marks not related:" mark point))
      (if (< index1 index2)
	  (group-extract-string group index1 index2)
	  (group-extract-string group index2 index1)))))

(define (delete-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group mark))
	  (index1 (mark-index mark))
	  (index2 (mark-index point)))
      (if (not (eq? group (mark-group point)))
	  (error "Marks not related:" mark point))
      (if (< index1 index2)
	  (group-delete! group index1 index2)
	  (group-delete! group index2 index1)))))

(define (extract-and-delete-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group mark))
	  (index1 (mark-index mark))
	  (index2 (mark-index point)))
      (if (not (eq? group (mark-group point)))
	  (error "Marks not related:" mark point))
      (if (< index1 index2)
	  (group-extract-and-delete-string! group index1 index2)
	  (group-extract-and-delete-string! group index2 index1)))))

(define (downcase-area mark #!optional point)
  (region-transform!
   (make-region mark (if (default-object? point) (current-point) point))
   (lambda (string)
     (string-downcase! string)
     string)))

(define (upcase-area mark #!optional point)
  (region-transform!
   (make-region mark (if (default-object? point) (current-point) point))
   (lambda (string)
     (string-upcase! string)
     string)))

(define (capitalize-area mark #!optional point)
  (region-transform!
   (make-region mark (if (default-object? point) (current-point) point))
   (lambda (string)
     (string-downcase! string)
     (string-set! string 0 (char-upcase (string-ref string 0)))
     string)))

(define (mark-flash mark #!optional type)
  (cond (*executing-keyboard-macro?* unspecific)
	((not mark) (editor-beep))
	((window-mark-visible? (current-window) mark)
	 (with-current-point mark
	   (lambda ()
	     (sit-for 500))))
	(else
	 (temporary-message
	  "Matches "
	  (let ((start (line-start mark 0))
		(end (line-end mark 0)))
	    (case (and (not (default-object? type)) type)
	      ((RIGHT) (extract-string mark end))
	      ((LEFT) (extract-string start mark))
	      (else (extract-string start end))))))))

(define (sit-for interval)
  (let ((time-limit (+ (real-time-clock) interval)))
    (let loop ()
      (if (and (not (keyboard-peek-no-hang))
	       (< (real-time-clock) time-limit))
	  (loop)))))

(define (sleep-for interval)
  (let ((time-limit (+ (real-time-clock) interval)))
    (do ()
	((>= (real-time-clock) time-limit)))))

(define (reposition-window-top mark)
  (if (not (and mark (set-window-start-mark! (current-window) mark false)))
      (editor-beep)))

(define (narrow-to-region mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group mark))
	  (index1 (mark-index mark))
	  (index2 (mark-index point)))
      (if (not (eq? group (mark-group point)))
	  (error "Marks not related:" mark point))
      (if (<= index1 index2)
	  (group-narrow! group index1 index2)
	  (group-narrow! group index2 index1)))))

(define (widen #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-widen! (mark-group point))))