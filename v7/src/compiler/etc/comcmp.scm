#| -*-Scheme-*-

$Id: comcmp.scm,v 1.8 2001/12/20 20:51:15 cph Exp $

Copyright (c) 1989-1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Compiled code binary comparison program

(declare (usual-integrations))

(if (unassigned? compiled-code-block/bytes-per-object)
    (set! compiled-code-block/bytes-per-object 4))

(define-syntax ucode-type
  (lambda (name)
    (microcode-type name)))

(define comcmp:ignore-debugging-info? #t)
(define comcmp:show-differing-blocks? #f)

(define (compare-code-blocks b1 b2)
  (let ((memoizations '()))
    (define (equal? x y)
      (or (eq? x y)
	  (if (object-type? (object-type x) y)
	      (cond ((object-type? (ucode-type cell) y)
		     (equal? (cell-contents x) (cell-contents y)))
		    ((object-type? (ucode-type list) y)
		     (and (equal? (car x) (car y))
			  (equal? (cdr x) (cdr y))))
		    ((object-type? (ucode-type character-string) y)
		     (string=? x y))
		    ((object-type? (ucode-type vector-1b) y)
		     (bit-string=? x y))
		    ((number? y)
		     (and (= x y)
			  (boolean=? (exact? x) (exact? y))))
		    ((pathname? x)
		     (and (pathname? y)
			  (pathname=? x y)))
		    ((object-type? (ucode-type vector) y)
		     (let ((size (vector-length x)))
		       (and (= size (vector-length y))
			    (let loop ((index 0))
			      (or (= index size)
				  (and (equal? (vector-ref x index)
					       (vector-ref y index))
				       (loop (1+ index))))))))
		    ((compiled-code-block? x)
		     (not (compare-blocks x y #f)))
		    ((compiled-code-address? x)
		     (and (= (compiled-entry/offset x)
			     (compiled-entry/offset y))
			  (not (compare-blocks
				(compiled-entry/block x)
				(compiled-entry/block y)
				#f))))
		    (else
		     #f))
	      (and (number? x)
		   (number? y)
		   (= x y)
		   (boolean=? (exact? x) (exact? y))))))

    (define (compare-blocks b1 b2 top-level?)
      (memoize! b1 b2
		(let ((core
		       (lambda ()
			 (let ((l1 (system-vector-length b1))
			       (l2 (system-vector-length b2)))
			   (if (not (= l1 l2))
			       `(length ,l1 ,l2)
			       (or (compare-code-sections b1 b2)
				   (compare-constant-sections b1 b2)))))))
		  (if (or top-level?
			  (not comcmp:show-differing-blocks?))
		      core
		      (lambda ()
			(let ((result (core)))
			  (if result
			      (begin
				(newline)
				(write `(subblocks ,b1 ,b2 ,result))))
			  result))))))

    (define (memoize! b1 b2 do-it)
      (let ((entry (assq b1 memoizations))
	    (if-not-found
	     (lambda ()
	       (let ((result (do-it)))
		 (let ((entry (assq b1 memoizations)))
		   (if entry
		       (let ((entry* (assq b2 (cdr entry))))
			 (if entry*
			     (set-cdr! entry* result)
			     (set-cdr! entry
				       (cons (cons b2 result) (cdr entry)))))
		       (set! memoizations
			     (cons (list b1 (cons b2 result))
				   memoizations))))
		 result))))
	(if entry
	    (let ((entry (assq b2 (cdr entry))))
	      (if entry
		  (cdr entry)
		  (if-not-found)))
	    (if-not-found))))

    (define (compare-code-sections b1 b2)
      (let ((s1 (compiled-code-block/code-start b1))
	    (s2 (compiled-code-block/code-start b2))
	    (e1 (compiled-code-block/code-end b1))
	    (e2 (compiled-code-block/code-end b2)))
	(cond ((not (= s1 s2))
	       `(code-start ,s1 ,s2))
	      ((not (= e1 e2))
	       `(code-end ,e1 ,e2))
	      ((not (bit-string=? (read-code b1 s1 e1)
				  (read-code b2 s2 e2)))
	       `(code))
	      (else
	       #f))))

    (define (read-code b s e)
      (let ((bs (bit-string-allocate (* addressing-granularity (- e s)))))
	(read-bits! b (* addressing-granularity s) bs)
	bs))

    (define addressing-granularity 8)

    (define (compare-constant-sections b1 b2)
      ;; Kludge!
      (if comcmp:ignore-debugging-info?
	  (begin
	    (set-compiled-code-block/debugging-info! b1 #f)
	    (set-compiled-code-block/debugging-info! b2 #f)))

      (let ((s1 (compiled-code-block/constants-start b1))
	    (s2 (compiled-code-block/constants-start b2))
	    (e1 (compiled-code-block/constants-end b1))
	    (e2 (compiled-code-block/constants-end b2)))
	(cond ((not (= s1 s2))
	       `(constant-start ,s1 ,s2))
	      ((not (= e1 e2))
	       `(constant-end ,e1 ,e2))
	      (else
	       (let loop ((s s1) (e e1) (diffs '()))
		 (cond ((<= s e)
			(let ((diff
			       (compare-constants
				s
				(system-vector-ref b1 s)
				(system-vector-ref b2 s))))
			  (cond ((not diff)
				 (loop (1+ s) e diffs))
				((eq? (car diff) 'CONSTANTS)
				 (loop (1+ s)
				       e
				       (if (member (cadr diff) diffs)
					   diffs
					   (cons (cadr diff) diffs))))
				(else
				 diff))))
		       ((null? diffs)
			#f)
		       (else
			(cons 'CONSTANTS (reverse! diffs)))))))))

    (define (compare-constants s c1 c2)
      (and (not (equal? c1 c2))
	   (let ((differ
		  (lambda ()
		    `(CONSTANTS (,s ,c1 ,c2)))))
	     (cond ((quotation? c1)
		    (if (quotation? c2)
			(compare-constants s
					   (quotation-expression c1)
					   (quotation-expression c2))
			(differ)))
		   ((LAMBDA? C1)
		    (if (lambda? c2)
			(lambda-components c1
			  (lambda (name required optional rest auxiliary
					declarations body)
			    (lambda-components c1
			      (lambda (name* required* optional* rest*
					     auxiliary* declarations* body*)
				(if (and (eqv? name name*)
					 (equal? required required*)
					 (equal? optional optional*)
					 (eqv? rest rest*)
					 (equal? auxiliary auxiliary*)
					 (equal? declarations declarations*))
				    (compare-constants s body body*)
				    (differ))))))
			(differ)))
		   (else
		    (differ))))))
    (compare-blocks b1 b2 #t)))

(define (compare-com-files f1 f2 #!optional verbose?)
  (let ((quiet? (or (default-object? verbose?) (not verbose?))))

    (let ((s1 (fasload f1 quiet?))
	  (s2 (fasload f2 quiet?))
	  (dbg-info-vector?
	   (access dbg-info-vector?
		   (->environment '(RUNTIME COMPILER-INFO))))
	  (dbg-info-vector/blocks-vector
	   (access dbg-info-vector/blocks-vector
		   (->environment '(RUNTIME COMPILER-INFO)))))
      (if (and (comment? s1) (dbg-info-vector? (comment-text s1)))
	  (if (and (comment? s2) (dbg-info-vector? (comment-text s2)))
	      (let ((v1 (dbg-info-vector/blocks-vector (comment-text s1)))
		    (v2 (dbg-info-vector/blocks-vector (comment-text s2))))
		(let ((e1 (vector-length v1))
		      (e2 (vector-length v2)))
		  (if (= e1 e2)
		      (compare-code-blocks (vector-ref v1 0) (vector-ref v2 0))
		      `(number-of-blocks ,e1 ,e2))))
	      '(block-structure))
	  (if (and (comment? s2) (dbg-info-vector? (comment-text s2)))
	      '(block-structure)
	      (compare-code-blocks (compiled-code-address->block s1)
				   (compiled-code-address->block s2)))))))

(define (show-differences f1 f2)
  (define (->name f)
    (enough-namestring (merge-pathnames f)))

  (let ((result (compare-com-files f1 f2)))
    (if (pair? result)
	(begin
	  (newline)
	  (for-each display
		    (list "*** Files " (->name f1)
			  " and " (->name f2)
			  " differ: "))
	  (if (eq? 'CONSTANTS (car result))
	      (begin
		(display "***")
		(newline)
		(display "(constants")
		(for-each (lambda (c)
			    (newline)
			    (display "  ")
			    (write c))
			  (cdr result))
		(display ")"))
	      (begin
		(write result)
		(display " ***")))))))