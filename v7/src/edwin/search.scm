;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/search.scm,v 1.148 1991/04/21 00:51:57 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
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

;;;; Search/Match Primitives

(declare (usual-integrations))

;;;; Character Search and Match

(let-syntax
    ((define-forward-search
       (macro (name find-next)
	 `(DEFINE (,name GROUP START END CHAR)
	    ;; Assume (FIX:<= START END)
	    (AND (NOT (FIX:= START END))
		 (COND ((FIX:<= END (GROUP-GAP-START GROUP))
			(,find-next (GROUP-TEXT GROUP) START END CHAR))
		       ((FIX:<= (GROUP-GAP-START GROUP) START)
			(LET ((POSITION
			       (,find-next
				(GROUP-TEXT GROUP)
				(FIX:+ START (GROUP-GAP-LENGTH GROUP))
				(FIX:+ END (GROUP-GAP-LENGTH GROUP))
				CHAR)))
			  (AND POSITION
			       (FIX:- POSITION (GROUP-GAP-LENGTH GROUP)))))
		       ((,find-next (GROUP-TEXT GROUP)
				    START
				    (GROUP-GAP-START GROUP)
				    CHAR))
		       (ELSE
			(LET ((POSITION
			       (,find-next (GROUP-TEXT GROUP)
					   (GROUP-GAP-END GROUP)
					   (FIX:+ END (GROUP-GAP-LENGTH GROUP))
					   CHAR)))
			  (AND POSITION
			       (FIX:- POSITION
				      (GROUP-GAP-LENGTH GROUP)))))))))))
(define-forward-search group-find-next-char substring-find-next-char)
(define-forward-search group-find-next-char-ci substring-find-next-char-ci)
(define-forward-search group-find-next-char-in-set
  substring-find-next-char-in-set))

(let-syntax
    ((define-backward-search
       (macro (name find-previous)
	 `(DEFINE (,name GROUP START END CHAR)
	    ;; Assume (FIX:<= START END)
	    (AND (NOT (FIX:= START END))
		 (COND ((FIX:<= END (GROUP-GAP-START GROUP))
			(,find-previous (GROUP-TEXT GROUP) START END CHAR))
		       ((FIX:<= (GROUP-GAP-START GROUP) START)
			(LET ((POSITION
			       (,find-previous
				(GROUP-TEXT GROUP)
				(FIX:+ START (GROUP-GAP-LENGTH GROUP))
				(FIX:+ END (GROUP-GAP-LENGTH GROUP))
				CHAR)))
			  (AND POSITION
			       (FIX:- POSITION (GROUP-GAP-LENGTH GROUP)))))
		       ((,find-previous (GROUP-TEXT GROUP)
					(GROUP-GAP-END GROUP)
					(FIX:+ END (GROUP-GAP-LENGTH GROUP))
					CHAR)
			=> (LAMBDA (POSITION)
			     (FIX:- POSITION (GROUP-GAP-LENGTH GROUP))))
		       (else
			(,find-previous (GROUP-TEXT GROUP)
					START
					(GROUP-GAP-START GROUP)
					CHAR))))))))
(define-backward-search group-find-previous-char substring-find-previous-char)
(define-backward-search group-find-previous-char-ci
  substring-find-previous-char-ci)
(define-backward-search group-find-previous-char-in-set
  substring-find-previous-char-in-set))

(define-integrable (%find-next-newline group start end)
  (group-find-next-char group start end #\newline))

(define-integrable (%find-previous-newline group start end)
  ;; Note reversal of index arguments here.
  (let ((index (group-find-previous-char group end start #\newline)))
    (and index
	 (fix:+ index 1))))

(define (char-search-forward char start end #!optional case-fold-search)
  (let ((group (mark-group start))
	(start-index (mark-index start))
	(end-index (mark-index end)))
    (if (not (and (eq? group (mark-group end))
		  (fix:<= start-index end-index)))
	(error "Marks incorrectly related:" start end))
    (let ((index
	   (if (if (default-object? case-fold-search)
		   (group-case-fold-search group)
		   case-fold-search)
	       (group-find-next-char-ci group start-index end-index char)
	       (group-find-next-char group start-index end-index char))))
      (and index
	   (make-mark group (fix:+ index 1))))))

(define (char-search-backward char start end #!optional case-fold-search)
  (let ((group (mark-group start))
	(start-index (mark-index start))
	(end-index (mark-index end)))
    (if (not (and (eq? group (mark-group end))
		  (fix:>= start-index end-index)))
	(error "Marks incorrectly related:" start end))
    (let ((index
	   (if (if (default-object? case-fold-search)
		   (group-case-fold-search group)
		   case-fold-search)
	       (group-find-next-char-ci group end-index start-index char)
	       (group-find-next-char group end-index start-index char))))
      (and index
	   (make-mark group index)))))

(define (char-match-forward char mark #!optional case-fold-search)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (and (not (group-end-index? group index))
	 (if (if (default-object? case-fold-search)
		 (group-case-fold-search group)
		 case-fold-search)
	     (char-ci=? char (group-right-char group index))
	     (char=? char (group-right-char group index))))))

(define (char-match-backward char mark #!optional case-fold-search)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (and (not (group-start-index? group index))
	 (if (if (default-object? case-fold-search)
		 (group-case-fold-search group)
		 case-fold-search)
	     (char-ci=? char (group-left-char group index))
	     (char=? char (group-left-char group index))))))

(define (skip-chars-forward pattern #!optional start end limit?)
  (let ((start (if (default-object? start) (current-point) start)))
    (let ((end (if (default-object? end) (group-end start) end)))
      (let ((limit? (if (default-object? limit?) 'LIMIT limit?)))
	(if (not (mark<= start end))
	    (error "SKIP-CHARS-FORWARD: Marks incorrectly related" start end))
	(let ((index
	       (group-find-next-char-in-set (mark-group start)
					    (mark-index start)
					    (mark-index end)
					    (re-compile-char-set pattern
								 true))))
	  (if index
	      (make-mark (mark-group start) index)
	      (limit-mark-motion limit? end)))))))

(define (skip-chars-backward pattern #!optional start end limit?)
  (let ((start (if (default-object? start) (current-point) start)))
    (let ((end (if (default-object? end) (group-start start) end)))
      (let ((limit? (if (default-object? limit?) 'LIMIT limit?)))
	(if (not (mark>= start end))
	    (error "SKIP-CHARS-BACKWARD: Marks incorrectly related" start end))
	(let ((index
	       (group-find-previous-char-in-set (mark-group start)
						(mark-index end)
						(mark-index start)
						(re-compile-char-set pattern
								     true))))
	  (if index
	      (make-mark (mark-group start) (fix:+ index 1))
	      (limit-mark-motion limit? end)))))))

;;;; String Search and Match

(define (group-match-substring-forward group start end
				       string string-start string-end)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-length (group-gap-length group)))
    (let ((match
	   (lambda (s1 e1 s2)
	     (let loop ((i1 s1) (i2 s2))
	       (if (or (fix:= i1 e1)
		       (fix:= i2 string-end)
		       (not (char=? (string-ref text i1)
				    (string-ref string i2))))
		   i1
		   (loop (fix:+ i1 1) (fix:+ i2 1)))))))
      (cond ((fix:<= end gap-start)
	     (match start end string-start))
	    ((fix:<= gap-start start)
	     (fix:- (match (fix:+ start gap-length)
			   (fix:+ end gap-length)
			   string-start)
		    gap-length))
	    (else
	     (let ((index (match start gap-start string-start)))
	       (if (fix:= index gap-start)
		   (fix:- (match (fix:+ gap-start gap-length)
				 (fix:+ end gap-length)
				 (fix:+ string-start (fix:- gap-start start)))
			  gap-length)
		   index)))))))

(define (group-match-substring-backward group start end
					string string-start string-end)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-length (group-gap-length group)))
    (let ((match
	   (lambda (s1 e1 e2)
	     (let loop ((i1 (fix:- e1 1)) (i2 (fix:- e2 1)))
	       (cond ((not (char=? (string-ref text i1)
				   (string-ref string i2)))
		      (fix:+ i1 1))
		     ((or (fix:= i1 s1) (fix:= i2 string-start))
		      i1)
		     (else
		      (loop (fix:- i1 1) (fix:- i2 1))))))))
      (cond ((or (fix:= start end) (fix:= string-start string-end))
	     end)
	    ((fix:<= end gap-start)
	     (match start end string-end))
	    ((fix:<= gap-start start)
	     (fix:- (match (fix:+ start gap-length)
			   (fix:+ end gap-length)
			   string-end)
		    gap-length))
	    (else
	     (let ((index
		    (fix:- (match (fix:+ gap-start gap-length)
				  (fix:+ end gap-length)
				  string-end)
			   gap-length)))
	       (if (fix:= index gap-start)
		   (match start
			  gap-start
			  (fix:- string-end (fix:- end gap-start)))
		   index)))))))

(define (group-match-substring-forward-ci group start end
					  string string-start string-end)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-length (group-gap-length group)))
    (let ((match
	   (lambda (s1 e1 s2)
	     (let loop ((i1 s1) (i2 s2))
	       (if (or (fix:= i1 e1)
		       (fix:= i2 string-end)
		       (not (char-ci=? (string-ref text i1)
				       (string-ref string i2))))
		   i1
		   (loop (fix:+ i1 1) (fix:+ i2 1)))))))
      (cond ((fix:<= end gap-start)
	     (match start end string-start))
	    ((fix:<= gap-start start)
	     (fix:- (match (fix:+ start gap-length)
			   (fix:+ end gap-length)
			   string-start)
		    gap-length))
	    (else
	     (let ((index (match start gap-start string-start)))
	       (if (fix:= index gap-start)
		   (fix:- (match (fix:+ gap-start gap-length)
				 (fix:+ end gap-length)
				 (fix:+ string-start (fix:- gap-start start)))
			  gap-length)
		   index)))))))

(define (group-match-substring-backward-ci group start end
					   string string-start string-end)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-length (group-gap-length group)))
    (let ((match
	   (lambda (s1 e1 e2)
	     (let loop ((i1 (fix:- e1 1)) (i2 (fix:- e2 1)))
	       (cond ((not (char-ci=? (string-ref text i1)
				      (string-ref string i2)))
		      (fix:+ i1 1))
		     ((or (fix:= i1 s1) (fix:= i2 string-start))
		      i1)
		     (else
		      (loop (fix:- i1 1) (fix:- i2 1))))))))
      (cond ((or (fix:= start end) (fix:= string-start string-end))
	     end)
	    ((fix:<= end gap-start)
	     (match start end string-end))
	    ((fix:<= gap-start start)
	     (fix:- (match (fix:+ start gap-length)
			   (fix:+ end gap-length)
			   string-end)
		    gap-length))
	    (else
	     (let ((index
		    (fix:- (match (fix:+ gap-start gap-length)
				  (fix:+ end gap-length)
				  string-end)
			   gap-length)))
	       (if (fix:= index gap-start)
		   (match start
			  gap-start
			  (fix:- string-end (fix:- end gap-start)))
		   index)))))))

(define (match-forward string mark #!optional case-fold-search)
  (let ((group (mark-group mark))
	(start (mark-index mark))
	(length (string-length string)))
    (let ((end (fix:+ start length)))
      (and (fix:<= end (group-end-index group))
	   (fix:= (if (if (default-object? case-fold-search)
			  (group-case-fold-search group)
			  case-fold-search)
		      (group-match-substring-forward-ci group start end
							string 0 length)
		      (group-match-substring-forward group start end
						     string 0 length))
		  end)
	   (make-mark group end)))))

(define (match-backward string mark #!optional case-fold-search)
  (let ((group (mark-group mark))
	(end (mark-index mark))
	(length (string-length string)))
    (let ((start (fix:- end length)))
      (and (fix:>= start (group-start-index group))
	   (fix:= (if (if (default-object? case-fold-search)
			  (group-case-fold-search group)
			  case-fold-search)
		      (group-match-substring-backward-ci group start end
							 string 0 length)
		      (group-match-substring-backward group start end
						      string 0 length))
		  start)
	   (make-mark group start)))))