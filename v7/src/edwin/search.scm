;;; -*-Scheme-*-
;;;
;;;$Id: search.scm,v 1.154 2002/02/03 03:38:54 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-1999, 2001, 2002 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Search/Match Primitives

(declare (usual-integrations))

(let-syntax
    ((define-search
       (sc-macro-transformer
	(lambda (form environment)
	  (let ((name (close-syntax (cadr form) environment))
		(find-next (close-syntax (caddr form) environment)))
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
					      (FIX:+ END
						     (GROUP-GAP-LENGTH GROUP))
					      CHAR)))
			     (AND POSITION
				  (FIX:- POSITION
					 (GROUP-GAP-LENGTH GROUP)))))))))))))
  (define-search group-find-next-char substring-find-next-char)
  (define-search group-find-next-char-ci substring-find-next-char-ci)
  (define-search group-find-next-char-in-set substring-find-next-char-in-set))

(let-syntax
    ((define-search
       (sc-macro-transformer
	(lambda (form environment)
	  (let ((name (close-syntax (cadr form) environment))
		(find-previous (close-syntax (caddr form) environment)))
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
					   CHAR))))))))))
  (define-search group-find-previous-char substring-find-previous-char)
  (define-search group-find-previous-char-ci substring-find-previous-char-ci)
  (define-search group-find-previous-char-in-set
    substring-find-previous-char-in-set))

(define-integrable (%find-next-newline group start end)
  (group-find-next-char group start end #\newline))

(define-integrable (%find-previous-newline group start end)
  ;; Note reversal of index arguments here.
  (let ((index (group-find-previous-char group end start #\newline)))
    (and index
	 (fix:+ index 1))))

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
	       (group-find-previous-char-ci group end-index start-index char)
	       (group-find-previous-char group end-index start-index char))))
      (and index
	   (make-mark group index)))))

(define-syntax default-end-mark
  (sc-macro-transformer
   (lambda (form environment)
     (let ((start (close-syntax (cadr form) environment))
	   (end (close-syntax (caddr form) environment)))
       `(IF (DEFAULT-OBJECT? ,end)
	    (GROUP-END ,start)
	    (BEGIN
	      (IF (NOT (MARK<= ,start ,end))
		  (ERROR "Marks incorrectly related:" ,start ,end))
	      ,end))))))

(define-syntax default-start-mark
  (sc-macro-transformer
   (lambda (form environment)
     (let ((start (close-syntax (cadr form) environment))
	   (end (close-syntax (caddr form) environment)))
       `(IF (DEFAULT-OBJECT? ,start)
	    (GROUP-START ,end)
	    (BEGIN
	      (IF (NOT (MARK<= ,start ,end))
		  (ERROR "Marks incorrectly related:" ,start ,end))
	      ,start))))))

(define (char-match-forward char start #!optional end case-fold-search)
  (and (mark< start (default-end-mark start end))
       (let ((group (mark-group start)))
	 (if (if (default-object? case-fold-search)
		 (group-case-fold-search group)
		 case-fold-search)
	     (char-ci=? char (group-right-char group (mark-index start)))
	     (char=? char (group-right-char group (mark-index start)))))))

(define (char-match-backward char end #!optional start case-fold-search)
  (and (mark< (default-start-mark start end) end)
       (let ((group (mark-group end)))
	 (if (if (default-object? case-fold-search)
		 (group-case-fold-search group)
		 case-fold-search)
	     (char-ci=? char (group-left-char group (mark-index end)))
	     (char=? char (group-left-char group (mark-index end)))))))

(define (skip-chars-forward pattern #!optional start end limit?)
  (let ((start (if (default-object? start) (current-point) start))
	(limit? (if (default-object? limit?) 'LIMIT limit?)))
    (let ((end (default-end-mark start end)))
      (let ((index
	     (group-find-next-char-in-set (mark-group start)
					  (mark-index start)
					  (mark-index end)
					  (re-compile-char-set pattern true))))
	(if index
	    (make-mark (mark-group start) index)
	    (limit-mark-motion limit? end))))))

(define (skip-chars-backward pattern #!optional end start limit?)
  (let ((end (if (default-object? end) (current-point) end))
	(limit? (if (default-object? limit?) 'LIMIT limit?)))
    (let ((start (default-start-mark start end)))
      (let ((index
	     (group-find-previous-char-in-set (mark-group start)
					      (mark-index start)
					      (mark-index end)
					      (re-compile-char-set pattern
								   true))))
	(if index
	    (make-mark (mark-group start) (fix:+ index 1))
	    (limit-mark-motion limit? start))))))

(define (match-forward string start #!optional end case-fold-search)
  (let ((end (default-end-mark start end))
	(group (mark-group start))
	(start-index (mark-index start))
	(length (string-length string)))
    (let ((i (fix:+ start-index length)))
      (and (fix:<= i (mark-index end))
	   (fix:= (if (if (default-object? case-fold-search)
			  (group-case-fold-search group)
			  case-fold-search)
		      (group-match-substring-forward-ci group start-index i
							string 0 length)
		      (group-match-substring-forward group start-index i
						     string 0 length))
		  i)
	   (make-mark group i)))))

(define (match-backward string end #!optional start case-fold-search)
  (let ((start (default-start-mark start end))
	(group (mark-group end))
	(end-index (mark-index end))
	(length (string-length string)))
    (let ((i (fix:- end-index length)))
      (and (fix:>= i (mark-index start))
	   (fix:= (if (if (default-object? case-fold-search)
			  (group-case-fold-search group)
			  case-fold-search)
		      (group-match-substring-backward-ci group i end-index
							 string 0 length)
		      (group-match-substring-backward group i end-index
						      string 0 length))
		  i)
	   (make-mark group i)))))