;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/search.scm,v 1.145 1989/03/14 08:02:45 cph Exp $
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

;;;; Search/Match Primitives

;;; The operations in this file are for internal editor use only.  For
;;; the user level search and match primitives, see the regular
;;; expression search and match procedures.

(declare (usual-integrations))

;;;; Character Search
#|
(define (find-next-char start end char)
  (if (not (mark<= start end))
      (error "Marks incorrectly related: FIND-NEXT-CHAR" start end))
  (let ((index (%find-next-char (mark-group start)
				(mark-index start)
				(mark-index end)
				char)))
    (and index (make-mark (mark-group start) index))))

(define (find-previous-char start end char)
  (if (not (mark>= start end))
      (error "Marks incorrectly related: FIND-PREVIOUS-CHAR" start end))
  (let ((index (%find-previous-char (mark-group start)
				    (mark-index start)
				    (mark-index end)
				    char)))
    (and index (make-mark (mark-group start) index))))
|#
(define (%find-next-newline group start end)
  (and (not (= start end))
       (let ((start (group-index->position group start true))
	     (end (group-index->position group end false))
	     (gap-start (group-gap-start group))
	     (gap-end (group-gap-end group))
	     (text (group-text group))
	     (char #\newline))
	 (let ((pos
		(if (and (<= start gap-start) (<= gap-end end))
		    (or (substring-find-next-char text start gap-start char)
			(substring-find-next-char text gap-end end char))
		    (substring-find-next-char text start end char))))
	   (and pos
		(group-position->index group pos))))))

(define (%find-previous-newline group start end)
  (and (not (= start end))
       (let ((start (group-index->position group start false))
	     (end (group-index->position group end true))
	     (gap-start (group-gap-start group))
	     (gap-end (group-gap-end group))
	     (text (group-text group))
	     (char #\newline))
	 (let ((pos
		(if (and (<= end gap-start) (<= gap-end start))
		    (or (substring-find-previous-char text gap-end start char)
			(substring-find-previous-char text end gap-start char))
		    (substring-find-previous-char text end start char))))
	   (and pos
		(1+ (group-position->index group pos)))))))

;;;; Character-set Search
#|
(define ((char-set-forward-search char-set) start end #!optional limit?)
  (or (find-next-char-in-set start end char-set)
      (limit-mark-motion (and (not (default-object? limit?)) limit?) end)))

(define ((char-set-backward-search char-set) start end #!optional limit?)
  (or (find-previous-char-in-set start end char-set)
      (limit-mark-motion (and (not (default-object? limit?)) limit?) end)))

(define (find-next-char-in-set start end char-set)
  (if (not (mark<= start end))
      (error "Marks incorrectly related: FIND-NEXT-CHAR-IN-SET" start end))
  (let ((index
	 (%find-next-char-in-set (mark-group start)
				 (mark-index start)
				 (mark-index end)
				 char-set)))
    (and index
	 (make-mark (mark-group start) index))))

(define (find-previous-char-in-set start end char-set)
  (if (not (mark>= start end))
      (error "Marks incorrectly related: FIND-PREVIOUS-CHAR-IN-SET" start end))
  (let ((index
	 (%find-previous-char-in-set (mark-group start)
				     (mark-index start)
				     (mark-index end)
				     char-set)))
    (and index
	 (make-mark (mark-group start) index))))
|#
(define (%find-next-char-in-set group start end char-set)
  (and (not (= start end))
       (let ((start (group-index->position group start true))
	     (end (group-index->position group end false))
	     (gap-start (group-gap-start group))
	     (gap-end (group-gap-end group))
	     (text (group-text group)))
	 (let ((pos
		(if (and (<= start gap-start)
			 (<= gap-end end))
		    (or (substring-find-next-char-in-set text start gap-start
							 char-set)
			(substring-find-next-char-in-set text gap-end end
							 char-set))
		    (substring-find-next-char-in-set text start end
						     char-set))))
	   (and pos (group-position->index group pos))))))

(define (%find-previous-char-in-set group start end char-set)
  (and (not (= start end))
       (let ((start (group-index->position group start false))
	     (end (group-index->position group end true))
	     (gap-start (group-gap-start group))
	     (gap-end (group-gap-end group))
	     (text (group-text group)))
	 (let ((pos
		(if (and (<= end gap-start)
			 (<= gap-end start))
		    (or (substring-find-previous-char-in-set text gap-end start
							     char-set)
			(substring-find-previous-char-in-set text end gap-start
							     char-set))
		    (substring-find-previous-char-in-set text end start
							 char-set))))
	   (and pos (1+ (group-position->index group pos)))))))

;;;; String Search
#|
(define (find-next-string start-mark end-mark string)
  (find-next-substring start-mark end-mark string 0 (string-length string)))

(define (find-next-substring start-mark end-mark string start end)
  (if (not (mark<= start-mark end-mark))
      (error "Marks incorrectly related: FIND-NEXT-SUBSTRING"
	     start-mark end-mark))
  (if (= start end)
      start-mark
      (let ((index
	     (%find-next-substring (mark-group start-mark)
				   (mark-index start-mark)
				   (mark-index end-mark)
				   string start end)))
	(and index (make-mark (mark-group start-mark) index)))))

(define (%find-next-string group start-index end-index string)
  (%find-next-substring group start-index end-index
			string 0 (string-length string)))

(define (find-previous-string start-mark end-mark string)
  (find-previous-substring start-mark end-mark
			   string 0 (string-length string)))

(define (find-previous-substring start-mark end-mark string start end)
  (if (not (mark>= start-mark end-mark))
      (error "Marks incorrectly related: FIND-PREVIOUS-SUBSTRING"
	     start-mark end-mark))
  (if (= start end)
      end-mark
      (let ((index
	     (%find-previous-substring (mark-group start-mark)
				       (mark-index start-mark)
				       (mark-index end-mark)
				       string start end)))
	(and index (make-mark (mark-group start-mark) index)))))

(define (%find-previous-string group start-index end-index string)
  (%find-previous-substring group start-index end-index
			    string 0 (string-length string)))

(define (%find-next-substring group start-index end-index string start end)
  (let ((char (string-ref string start))
	(bound (- end-index (-1+ (- end start)))))
    (define (loop first)
      (and first
	   (if (%match-next-substring group first end-index string start end)
	       first
	       (and (< first bound)
		    (loop (%find-next-char group (1+ first) bound char))))))
    (and (< start-index bound)
	 (loop (%find-next-char group start-index bound char)))))

(define (%find-previous-substring group start-index end-index string start end)
  (let ((char (string-ref string (-1+ end)))
	(bound (+ end-index (-1+ (- end start)))))
    (define (loop first)
      (and first
	   (if (%match-previous-substring group first end-index
					  string start end)
	       first
	       (and (> first bound)
		    (loop (%find-previous-char group (-1+ first) bound
					       char))))))
    (and (> start-index bound)
	 (loop (%find-previous-char group start-index bound char)))))

;;;; String Match

(define (match-next-strings start end strings)
  (let loop ((strings strings))
    (and (not (null? strings))
	 (or (match-next-string start end (car strings))
	     (loop (cdr strings))))))

(define (match-next-string start end string)
  (match-next-substring start end string 0 (string-length string)))

(define (match-next-substring start-mark end-mark string start end)
  (if (not (mark<= start-mark end-mark))
      (error "marks incorrectly related" start-mark end-mark))
  (let ((index
	 (%match-next-substring (mark-group start-mark)
				(mark-index start-mark)
				(mark-index end-mark)
				string start end)))
    (and index
	 (make-mark (mark-group start-mark) index))))

(define (match-previous-strings start end strings)
  (let loop ((strings strings))
    (and (not (null? strings))
	 (or (match-previous-string start end (car strings))
	     (loop (cdr strings))))))

(define (match-previous-string start end string)
  (match-previous-substring start end string 0 (string-length string)))

(define (match-previous-substring start-mark end-mark string start end)
  (if (not (mark>= start-mark end-mark))
      (error "marks incorrectly related" start-mark end-mark))
  (let ((index
	 (%match-previous-substring (mark-group start-mark)
				    (mark-index start-mark)
				    (mark-index end-mark)
				    string start end)))
    (and index
	 (make-mark (mark-group start-mark) index))))

(define (%match-next-string group start-index end-index string)
  (%match-next-substring group start-index end-index
			 string 0 (string-length string)))

(define (%match-previous-string group start-index end-index string)
  (%match-previous-substring group start-index end-index
			     string 0 (string-length string)))

(define (%match-next-substring group start-index end-index string start end)
  (let ((end-index* (+ start-index (- end start))))
    (and (<= end-index* end-index)
	 (%%match-substring group start-index end-index* string start end)
	 end-index*)))

(define (%match-previous-substring group start-index end-index
				   string start end)
  (let ((end-index* (- start-index (- end start))))
    (and (>= end-index* end-index)
	 (%%match-substring group end-index* start-index string start end)
	 end-index*)))

(define (%%match-substring group start-index end-index string start end)
  (and (not (= start-index end-index))
       (let ((start* (group-index->position group start-index true))
	     (end* (group-index->position group end-index false))
	     (gap-start (group-gap-start group))
	     (gap-end (group-gap-end group))
	     (text (group-text group)))
	 (if (and (<= start* gap-start) (<= gap-end end*))
	     (let ((split (+ start (- gap-start start*))))
	       (and (substring-ci=? text start* gap-start string start split)
		    (substring-ci=? text gap-end end* string split end)))
	     (substring-ci=? text start* end* string start end)))))

;;;; Character Match

(define (match-next-char start end char)
  (%match-next-char (mark-group start)
		    (mark-index start)
		    (mark-index end)
		    char))

(define (%match-next-char group start end char)
  (and (< start end)
       (char=? char (group-right-char group start))
       (1+ start)))

(define (match-previous-char start end char)
  (%match-previous-char (mark-group start)
			(mark-index start)
			(mark-index end)
			char))

(define (%match-previous-char group start end char)
  (and (> start end)
       (char=? char (group-left-char group start))
       (-1+ start)))

(define (match-next-char-in-set start end char-set)
  (%match-next-char-in-set (mark-group start)
			   (mark-index start)
			   (mark-index end)
			   char-set))

(define (%match-next-char-in-set group start end char-set)
  (and (< start end)
       (char-set-member? char-set (group-right-char group start))
       (1+ start)))

(define (match-previous-char-in-set start end char-set)
  (%match-previous-char-in-set (mark-group start)
			       (mark-index start)
			       (mark-index end)
			       char-set))

(define (%match-previous-char-in-set group start end char-set)
  (and (> start end)
       (char-set-member? char-set (group-left-char group start))
       (-1+ start)))
|#