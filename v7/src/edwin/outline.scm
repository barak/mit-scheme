;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/outline.scm,v 1.2 1992/04/17 20:54:59 arthur Exp $
;;;
;;;	Copyright (c) 1992 Massachusetts Institute of Technology
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

;;;; Outline minor mode

(declare (usual-integrations))

(define-variable outline-pattern
  "Regexp describing outline topic beginnings.  The more characters
match, the deeper our level in the outline."
  "^\*+"
  string?)

(define (topic-level outline-pattern mark)
  (and (re-search-backward outline-pattern (line-end mark 0) (group-start mark))
       (- (re-match-end-index 0)
	  (re-match-start-index 0))))

(define (level-start outline-pattern level end start)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (let loop ((end end))
    (and (re-search-backward outline-pattern end start)
	 (if (= (- (re-match-end-index 0)
		   (re-match-start-index 0))
		level)
	     (re-match-start 0)
	     (loop (re-match-start 0))))))

(define (%forward-up-topic start end outline-pattern)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (let ((level (topic-level outline-pattern start)))
	 (and level
	      (let next-topic ((start start))
		(and start
		     (re-search-forward outline-pattern (line-end start 0) end)
		     (let ((found-level
			    (- (re-match-end-index 0)
			       (re-match-start-index 0))))
		       (if (>= found-level level)
			   (next-topic (re-match-end 0))
			   (re-match-start 0)))))))))

(define (%backward-up-topic end start outline-pattern)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (let ((level (topic-level outline-pattern end)))
	 (and level
	      (let previous-topic ((end end))
		(and end
		     (re-search-backward outline-pattern end start)
		     (let ((found-level
			    (- (re-match-end-index 0)
			       (re-match-start-index 0))))
		       (if (>= found-level level)
			   (previous-topic (re-match-start 0))
			   (re-match-start 0)))))))))

(define (%forward-topic start end outline-pattern)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (let ((level (topic-level outline-pattern start)))
	 (and level
	      (let next-topic ((start start))
		(and start
		     (re-search-forward outline-pattern (line-end start 0) end)
		     (let ((found-level
			    (- (re-match-end-index 0)
			       (re-match-start-index 0))))
		       (cond ((= found-level level)
			      (re-match-start 0))
			     ((< found-level level)
			      false)
			     (else (next-topic (re-match-end 0)))))))))))

(define (%backward-topic end start outline-pattern)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (let ((level (topic-level outline-pattern end)))
	 (and level
	      (let previous-topic ((end end))
		(and end
		     (re-search-backward outline-pattern end start)
		     (let ((found-level
			    (- (re-match-end-index 0)
			       (re-match-start-index 0))))
		       (cond ((= found-level level)
			      (re-match-start 0))
			     ((< found-level level)
			      false)
			     (else (previous-topic (re-match-start 0)))))))))))

(define (forward-one-topic mark)
  (%forward-topic mark
		  (group-end mark)
		  (mark-local-ref mark (ref-variable-object outline-pattern))))

(define (backward-one-topic mark)
  (%backward-topic mark
		  (group-start mark)
		  (mark-local-ref mark (ref-variable-object outline-pattern))))

(define (forward-up-one-topic mark)
  (%forward-up-topic
   mark
   (group-end mark)
   (mark-local-ref mark (ref-variable-object outline-pattern))))

(define (backward-up-one-topic mark)
  (%backward-up-topic
   mark
   (group-start mark)
   (mark-local-ref mark (ref-variable-object outline-pattern))))

(define forward-topic)
(define backward-topic)

(make-motion-pair forward-one-topic backward-one-topic
 (lambda (f b)
   (set! forward-topic f)
   (set! backward-topic b)
   unspecific))

(define forward-up-topic)
(define backward-up-topic)

(make-motion-pair forward-up-one-topic backward-up-one-topic
 (lambda (f b)
   (set! forward-up-topic f)
   (set! backward-up-topic b)
   unspecific))

(define (topic-region mark)
  (let ((end (group-end mark))
	(pattern (ref-variable outline-pattern)))
    (let ((level (topic-level pattern mark)))
      (if level
	  (make-region
	   (if (re-search-backward
		pattern (line-end mark 0) (group-start mark))
	       (re-match-start 0)
	       (error "Inconsistency detected."))
	   (or (let next-topic ((start (line-end mark 0)))
		 (and start
		      (re-search-forward pattern (line-end start 0) end)
		      (let ((found-level
			     (- (re-match-end-index 0)
				(re-match-start-index 0))))
			(if (<= found-level level)
			    (re-match-start 0)
			    (next-topic (re-match-end 0))))))
	       (group-end mark)))
	  (current-region)))))

(define-command narrow-to-topic
  "Narrow to show the current outline level only."
  "d"
  (lambda (mark)
    (region-clip! (topic-region mark))))

(define-command forward-topic
  "Move forward to the next outline topic.  With arg, repeat, and go
backward if negative.  Outline topics match the regexp OUTLINE-PATTERN."
  "p"
  (lambda (argument)
    (move-thing forward-topic argument 'ERROR)))

(define-command backward-topic
  "Move backward to the next outline topic.  With arg, repeat, and go
forward if negative.  Outline topics match the regexp OUTLINE-PATTERN."
  "p"
  (lambda (argument)
    (move-thing backward-topic argument 'ERROR)))

(define-command forward-up-topic
  "Move forward the next-outermost outline topic.  With arg, repeat,
and go forward if negative.  Outline topics match the regexp
OUTLINE-PATTERN."
  "p"
  (lambda (argument)
    (move-thing forward-up-topic argument 'ERROR)))

(define-command backward-up-topic
  "Move backward the next-outermost outline topic.  With arg, repeat,
and go forward if negative.  Outline topics match the regexp
OUTLINE-PATTERN."
  "p"
  (lambda (argument)
    (move-thing backward-up-topic argument 'ERROR)))

(define-command outline-mode
  "Toggle outline mode.
With argument, turn outline mode on iff argument is positive."
  "P"
  (lambda (argument)
    (let ((argument (command-argument-value argument))
	  (mode (ref-mode-object outline)))
      (cond ((and (or (not argument) (positive? argument))
		  (not (current-minor-mode? mode)))
	     (enable-current-minor-mode! mode))
	    ((and (or (not argument) (not (positive? argument)))
		  (current-minor-mode? mode))
	     (disable-current-minor-mode! mode))))))

(define-minor-mode outline "Outline" "Minor mode for moving over outlines.")
(define-key 'outline '(#\C-z #\f) 'forward-topic)
(define-key 'outline '(#\C-z #\b) 'backward-topic)
(define-key 'outline '(#\C-z #\n) 'narrow-to-topic)
(define-key 'outline '(#\M-C-z #\f) 'forward-up-topic)
(define-key 'outline '(#\M-C-z #\b) 'backward-up-topic)