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

;;;; Recursive regular-expression implementation
;;; package: (runtime regexp recursive)

;;; The compiler takes a regular sexpression and returns an
;;; instruction.  An instruction is a procedure that accepts a success
;;; continuation, and returns a "linked instruction".  But success
;;; continuations and linked instructions have the same signature,
;;; which encourages the use of a combinator language.

(declare (usual-integrations))

;;;; Instructions

(define (insn:always-succeed)
  (lambda (succeed)
    succeed))

(define (insn:always-fail)
  (lambda (succeed)
    succeed
    (lambda (position groups fail)
      position groups
      (fail))))

(define (insn:string-start)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (not (prev-char position))
	  (succeed position groups fail)
	  (fail)))))

(define (insn:string-end)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (not (next-char position))
	  (succeed position groups fail)
	  (fail)))))

(define (insn:line-start)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (let ((char (prev-char position)))
	    (or (not char)
		(char=? char #\newline)))
	  (succeed position groups fail)
	  (fail)))))

(define (insn:line-end)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (let ((char (next-char position)))
	    (or (not char)
		(char=? char #\newline)))
	  (succeed position groups fail)
	  (fail)))))

(define (insn:char-matching predicate)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (let ((char (next-char position)))
	    (and char
		 (predicate char)))
	  (succeed (next-position position) groups fail)
	  (fail)))))

(define (insn:char char fold-case?)
  (insn:char-matching
   ((if fold-case? char-ci=-predicate char=-predicate) char)))

(define (insn:char-set char-set)
  (insn:char-matching (char-set-predicate char-set)))

(define (insn:inverse-char-set char-set)
  (insn:char-set (char-set-invert char-set)))

(define (insn:string string fold-case?)
  (let ((end (string-length string)))
    (cond ((fix:= end 0)
	   (insn:always-succeed))
	  ((fix:= end 1)
	   (insn:char (string-ref string 0) fold-case?))
	  (else
	   (let ((c= (if fold-case? char-ci=? char=?)))
	     (lambda (succeed)
	       (lambda (position groups fail)
		 (let loop ((i 0) (position position))
		   (if (fix:< i end)
		       (if (let ((char (next-char position)))
			     (and char
				  (c= char (string-ref string i))))
			   (loop (fix:+ i 1) (next-position position))
			   (fail))
		       (succeed position groups fail))))))))))

(define (insn:group key insn)
  (let ((start
	 (lambda (succeed)
	   (lambda (position groups fail)
	     (succeed position
		      (start-group key position groups)
		      fail))))
	(end
	 (lambda (succeed)
	   (lambda (position groups fail)
	     (succeed position
		      (end-group key position groups)
		      fail)))))
    (lambda (succeed)
      (start (insn (end succeed))))))

(define (insn:group-ref key)
  (lambda (succeed)
    (lambda (position groups fail)
      ((let ((group (find-group key groups)))
	 (if group
	     ((insn:string (group-value group) #f) succeed)
	     ;; This can happen with (* (GROUP ...)), but in other cases it
	     ;; would be an error.
	     succeed))
       position groups fail))))

(define (insn:seq insns)
  (lambda (succeed)
    (fold-right (lambda (insn next)
		  (insn next))
		succeed
		insns)))

(define (insn:alt insns)
  (reduce-right (lambda (insn1 insn2)
		  (lambda (succeed)
		    (%failure-chain (insn1 succeed)
				    (insn2 succeed))))
		(insn:always-fail)
		insns))

(define (insn:? insn)
  (lambda (succeed)
    (%failure-chain (insn succeed) succeed)))

(define (insn:?? insn)
  (lambda (succeed)
    (%failure-chain succeed (insn succeed))))

;;; The next two operations must fail when the instruction makes no
;;; progress in a given iteration.  Otherwise patterns like (* (SEQ))
;;; will loop forever.

(define (insn:* insn)
  (lambda (succeed)
    (define (loop position groups fail)
      ((%failure-chain (insn
			(lambda (position* groups* fail*)
			  (if (same-positions? position* position)
			      (fail*)
			      (loop position* groups* fail*))))
		       succeed)
       position groups fail))
    loop))

(define (insn:*? insn)
  (lambda (succeed)
    (define (loop position groups fail)
      ((%failure-chain succeed
		       (insn
			(lambda (position* groups* fail*)
			  (if (same-positions? position* position)
			      (fail*)
			      (loop position* groups* fail*)))))
       position groups fail))
    loop))

(define (%failure-chain s1 s2)
  (lambda (position groups fail)
    (s1 position
	groups
	(lambda () (s2 position groups fail)))))

(define (insn:** n m insn)
  (%repeat n m insn
	   (lambda (limit insn)
	     (%hybrid-chain limit
			    (lambda (succeed)
			      (lambda (continue)
				(%failure-chain (insn continue) succeed)))))
	   insn:*))

(define (insn:**? n m insn)
  (%repeat n m insn
	   (lambda (limit insn)
	     (%hybrid-chain limit
			    (lambda (succeed)
			      (lambda (continue)
				(%failure-chain succeed (insn continue))))))
	   insn:*?))

(define (%repeat n m insn repeat-limited repeat-unlimited)
  (let ((insn1 (%repeat-exactly n insn))
	(insn2
	 (if m
	     (repeat-limited (- m n) insn)
	     (repeat-unlimited insn))))
    (lambda (succeed)
      (insn1 (insn2 succeed)))))

(define (%repeat-exactly n insn)
  (%hybrid-chain n
		 (lambda (succeed)
		   (declare (ignore succeed))
		   insn)))

(define (%hybrid-chain limit pre-linker)
  (if (<= limit 8)
      (%immediate-chain limit pre-linker)
      (%delayed-chain limit pre-linker)))

(define (%immediate-chain limit pre-linker)
  (lambda (succeed)
    (let ((linker (pre-linker succeed)))
      (let loop ((i 0))
	(if (< i limit)
	    (linker (loop (+ i 1)))
	    succeed)))))

(define (%delayed-chain limit pre-linker)
  (lambda (succeed)
    (let ((linker (pre-linker succeed)))
      (let loop ((i 0))
	(if (< i limit)
	    (lambda (position groups fail)
	      ((linker (loop (+ i 1))) position groups fail))
	    succeed)))))

;;;; Positions

(define-record-type <position>
    (make-position marker index next-char prev-char next-pos prev-pos)
    position?
  (marker pos-marker)
  (index pos-index)
  (next-char next-char)
  (prev-char prev-char)
  (next-pos %next-pos)
  (prev-pos %prev-pos))

(define (next-position position)
  ((%next-pos position)))

(define (prev-position position)
  ((%prev-pos position)))

(define (same-positions? p1 p2)
  (and (eq? (pos-marker p1) (pos-marker p2))
       (fix:= (pos-index p1) (pos-index p2))))

(define (extract-string start-position end-position)
  (let ((builder (string-builder)))
    (do ((position start-position (next-position position)))
	((same-positions? position end-position))
      (builder (next-char position)))
    (builder)))

(define (make-source-position source)
  (let ((marker (list 'source-position)))
    (let loop
	((index 0)
	 (next-char (source))
	 (prev-char #f)
	 (prev-pos #f))
      (define this
	(make-position marker index next-char prev-char
	  (lambda ()
	    (loop (fix:+ index 1) (source) next-char this))
	  (lambda ()
	    prev-pos)))
      this)))

(define (make-string-position string start end)
  (let ((marker (list 'string-position)))
    (let loop ((index start))
      (define this
	(make-position marker index
		       (and (fix:< index end)
			    (string-ref string index))
		       (and (fix:> index start)
			    (string-ref string (fix:- index 1)))
		       (lambda ()
			 (if (fix:< index end)
			     (loop (fix:+ index 1))
			     this))
		       (lambda ()
			 (if (fix:>= index start)
			     (loop (fix:- index 1))
			     this))))
      this)))

;;;; Groups

(define (make-groups)

  (define (state started-groups ended-groups)

    (define (start key position)
      (if (assv key started-groups)
	  (error "Incorrectly nested group:" key))
      (state (cons (cons key position) started-groups)
	     ended-groups))

    (define (end key position)
      (if (not (and (pair? started-groups)
		    (eqv? (caar started-groups) key)))
	  (error "Incorrectly nested group:" key))
      (state (cdr started-groups)
	     (cons (make-group key
			       (cdar started-groups)
			       position)
		   ended-groups)))

    (define (*find key)
      (if (assv key started-groups)
	  (error "Can't refer to unfinished group:" key))
      (find (lambda (g)
	      (eqv? key (group-key g)))
	    ended-groups))

    (define (get-all)
      (reverse ended-groups))

    (%make-groups start end *find get-all))

  (state '() '()))

(define-record-type <groups>
    (%make-groups start end find all)
    groups?
  (start groups:start)
  (end groups:end)
  (find groups:find)
  (all groups:all))

(define (start-group key position groups)
  ((groups:start groups) key position))

(define (end-group key position groups)
  ((groups:end groups) key position))

(define (find-group key groups)
  ((groups:find groups) key))

(define (all-groups groups)
  ((groups:all groups)))

(define (make-group key start-position end-position)
  (%make-group key
	       (pos-index start-position)
	       (pos-index end-position)
	       (extract-string start-position end-position)))

(define-record-type <group>
    (%make-group key start end value)
    group?
  (key group-key)
  (start group-start)
  (end group-end)
  (value group-value))