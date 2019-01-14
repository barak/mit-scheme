#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Search/Match Primitives

(declare (usual-integrations))

(define-syntax define-next-char-search
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form))
	   (find-next (close-syntax (caddr form) environment)))
       `(define (,name group start end char)
	  ;; Assume (FIX:<= START END)
	  (and (not (fix:= start end))
	       (cond ((fix:<= end (group-gap-start group))
		      (,find-next (group-text group) start end char))
		     ((fix:<= (group-gap-start group) start)
		      (let ((position
			     (,find-next
			      (group-text group)
			      (fix:+ start (group-gap-length group))
			      (fix:+ end (group-gap-length group))
			      char)))
			(and position
			     (fix:- position (group-gap-length group)))))
		     ((,find-next (group-text group)
				  start
				  (group-gap-start group)
				  char))
		     (else
		      (let ((position
			     (,find-next (group-text group)
					 (group-gap-end group)
					 (fix:+ end
						(group-gap-length group))
					 char)))
			(and position
			     (fix:- position
				    (group-gap-length group))))))))))))

(define-next-char-search group-find-next-char
  substring-find-next-char)
(define-next-char-search group-find-next-char-ci
  substring-find-next-char-ci)
(define-next-char-search group-find-next-char-in-set
  substring-find-next-char-in-set)

(define-syntax define-prev-char-search
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form))
	   (find-previous (close-syntax (caddr form) environment)))
       `(define (,name group start end char)
	  ;; Assume (FIX:<= START END)
	  (and (not (fix:= start end))
	       (cond ((fix:<= end (group-gap-start group))
		      (,find-previous (group-text group) start end char))
		     ((fix:<= (group-gap-start group) start)
		      (let ((position
			     (,find-previous
			      (group-text group)
			      (fix:+ start (group-gap-length group))
			      (fix:+ end (group-gap-length group))
			      char)))
			(and position
			     (fix:- position (group-gap-length group)))))
		     ((,find-previous (group-text group)
				      (group-gap-end group)
				      (fix:+ end (group-gap-length group))
				      char)
		      => (lambda (position)
			   (fix:- position (group-gap-length group))))
		     (else
		      (,find-previous (group-text group)
				      start
				      (group-gap-start group)
				      char)))))))))

(define-prev-char-search group-find-previous-char
  substring-find-previous-char)
(define-prev-char-search group-find-previous-char-ci
  substring-find-previous-char-ci)
(define-prev-char-search group-find-previous-char-in-set
  substring-find-previous-char-in-set)

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
	   (if (default-case-fold-search case-fold-search start)
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
	   (if (default-case-fold-search case-fold-search start)
	       (group-find-previous-char-ci group end-index start-index char)
	       (group-find-previous-char group end-index start-index char))))
      (and index
	   (make-mark group index)))))

(define (char-match-forward char start #!optional end case-fold-search)
  (and (mark< start (default-end-mark start end))
       (let ((group (mark-group start)))
	 (if (default-case-fold-search case-fold-search start)
	     (char-ci=? char (group-right-char group (mark-index start)))
	     (char=? char (group-right-char group (mark-index start)))))))

(define (char-match-backward char end #!optional start case-fold-search)
  (and (mark< (default-start-mark start end) end)
       (let ((group (mark-group end)))
	 (if (default-case-fold-search case-fold-search end)
	     (char-ci=? char (group-left-char group (mark-index end)))
	     (char=? char (group-left-char group (mark-index end)))))))

(define (default-start-mark start end)
  (if (default-object? start)
      (group-start end)
      (begin
	(if (not (mark<= start end))
	    (error "Marks incorrectly related:" start end))
	start)))

(define (default-end-mark start end)
  (if (default-object? end)
      (group-end start)
      (begin
	(if (not (mark<= start end))
	    (error "Marks incorrectly related:" start end))
	end)))

(define (default-case-fold-search case-fold-search mark)
  (if (default-object? case-fold-search)
      (group-case-fold-search (mark-group mark))
      case-fold-search))

(define (skip-chars-forward pattern #!optional start end limit?)
  (let ((start (if (default-object? start) (current-point) start))
	(limit? (if (default-object? limit?) 'limit limit?)))
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
	(limit? (if (default-object? limit?) 'limit limit?)))
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
	   (fix:= (if (default-case-fold-search case-fold-search start)
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
	   (fix:= (if (default-case-fold-search case-fold-search start)
		      (group-match-substring-backward-ci group i end-index
							 string 0 length)
		      (group-match-substring-backward group i end-index
						      string 0 length))
		  i)
	   (make-mark group i)))))