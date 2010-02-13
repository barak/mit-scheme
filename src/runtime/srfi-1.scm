#| -*- Scheme -*-

Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
this code as long as you do not remove this copyright notice or
hold me liable for its use. Please send bug reports to shivers at
ai.mit.edu.
    -Olin

This implementation heavily modified by John Kraemer and Chris
Hanson for inclusion in MIT/GNU Scheme.

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; SRFI-1 list-processing library

(declare (usual-integrations))

;;; [Olin's original notes]

;;; This is a library of list- and pair-processing functions. I wrote it after
;;; carefully considering the functions provided by the libraries found in
;;; R4RS/R5RS Scheme, MIT Scheme, Gambit, RScheme, MzScheme, slib, Common
;;; Lisp, Bigloo, guile, T, APL and the SML standard basis. It is a pretty
;;; rich toolkit, providing a superset of the functionality found in any of
;;; the various Schemes I considered.

;;; This implementation is intended as a portable reference implementation
;;; for SRFI-1. See the porting notes below for more information.

;;; Exported:
;;; xcons tree-copy make-list list-tabulate cons* list-copy
;;; proper-list? circular-list? dotted-list? not-pair? null-list? list=
;;; circular-list length+
;;; iota
;;; first second third fourth fifth sixth seventh eighth ninth tenth
;;; car+cdr
;;; take       drop
;;; take-right drop-right
;;; take!      drop-right!
;;; split-at   split-at!
;;; last last-pair
;;; zip unzip1 unzip2 unzip3 unzip4 unzip5
;;; count
;;; append! append-reverse append-reverse! concatenate concatenate!
;;; unfold       fold       pair-fold       reduce
;;; unfold-right fold-right pair-fold-right reduce-right
;;; append-map append-map! map! pair-for-each filter-map map-in-order
;;; filter  partition  remove
;;; filter! partition! remove!
;;; find find-tail any every list-index
;;; take-while drop-while take-while!
;;; span break span! break!
;;; delete delete!
;;; alist-cons alist-copy
;;; delete-duplicates delete-duplicates!
;;; alist-delete alist-delete!
;;; reverse!
;;; lset<= lset= lset-adjoin
;;; lset-union  lset-intersection  lset-difference  lset-xor
;;; lset-union! lset-intersection! lset-difference! lset-xor!
;;; lset-diff+intersection
;;; lset-diff+intersection!

;;; In principle, the following R4RS list- and pair-processing procedures
;;; are also part of this package's exports, although they are not defined
;;; in this file:
;;;   Primitives: cons pair? null? car cdr set-car! set-cdr!
;;;   Non-primitives: list length append reverse cadr ... cddddr list-ref
;;;                   memq memv assq assv
;;;   (The non-primitives are defined in this file, but commented out.)

;;; These R4RS procedures have extended definitions in SRFI-1 and are defined
;;; in this file:
;;;   map for-each member assoc

;;; The remaining two R4RS list-processing procedures are not included:
;;;   list-tail (use drop)
;;;   list? (use proper-list?)

;;; A note on recursion and iteration/reversal:
;;; Many iterative list-processing algorithms naturally compute the elements
;;; of the answer list in the wrong order (left-to-right or head-to-tail) from
;;; the order needed to cons them into the proper answer (right-to-left, or
;;; tail-then-head). One style or idiom of programming these algorithms, then,
;;; loops, consing up the elements in reverse order, then destructively
;;; reverses the list at the end of the loop. I do not do this. The natural
;;; and efficient way to code these algorithms is recursively. This trades off
;;; intermediate temporary list structure for intermediate temporary stack
;;; structure. In a stack-based system, this improves cache locality and
;;; lightens the load on the GC system. Don't stand on your head to iterate!
;;; Recurse, where natural. Multiple-value returns make this even more
;;; convenient, when the recursion/iteration has multiple state values.

;;; Porting:
;;; This is carefully tuned code; do not modify casually.
;;;   - It is careful to share storage when possible;
;;;   - Side-effecting code tries not to perform redundant writes.

;;; That said, a port of this library to a specific Scheme system might wish
;;; to tune this code to exploit particulars of the implementation.
;;; The single most important compiler-specific optimisation you could make
;;; to this library would be to add rewrite rules or transforms to:
;;; - transform applications of n-ary procedures (e.g. LIST=, CONS*, APPEND,
;;;   LSET-UNION) into multiple applications of a primitive two-argument
;;;   variant.
;;; - transform applications of the mapping functions (MAP, FOR-EACH, FOLD,
;;;   ANY, EVERY) into open-coded loops. The killer here is that these
;;;   functions are n-ary. Handling the general case is quite inefficient,
;;;   requiring many intermediate data structures to be allocated and
;;;   discarded.
;;; - transform applications of procedures that take optional arguments
;;;   into calls to variants that do not take optional arguments. This
;;;   eliminates unnecessary consing and parsing of the rest parameter.

;;; These transforms would provide BIG speedups. In particular, the n-ary
;;; mapping functions are particularly slow and cons-intensive, and are good
;;; candidates for tuning. I have coded fast paths for the single-list cases,
;;; but what you really want to do is exploit the fact that the compiler
;;; usually knows how many arguments are being passed to a particular
;;; application of these functions -- they are usually explicitly called, not
;;; passed around as higher-order values. If you can arrange to have your
;;; compiler produce custom code or custom linkages based on the number of
;;; arguments in the call, you can speed these functions up a *lot*. But this
;;; kind of compiler technology no longer exists in the Scheme world as far as
;;; I can see.

;;; Note that this code is, of course, dependent upon standard bindings for
;;; the R5RS procedures -- i.e., it assumes that the variable CAR is bound
;;; to the procedure that takes the car of a list. If your Scheme
;;; implementation allows user code to alter the bindings of these procedures
;;; in a manner that would be visible to these definitions, then there might
;;; be trouble. You could consider horrible kludgery along the lines of
;;;    (define fact
;;;      (let ((= =) (- -) (* *))
;;;        (letrec ((real-fact (lambda (n)
;;;                              (if (= n 0) 1 (* n (real-fact (- n 1)))))))
;;;          real-fact)))
;;; Or you could consider shifting to a reasonable Scheme system that, say,
;;; has a module system protecting code from this kind of lossage.

;;; This code does a fair amount of run-time argument checking. If your
;;; Scheme system has a sophisticated compiler that can eliminate redundant
;;; error checks, this is no problem. However, if not, these checks incur
;;; some performance overhead -- and, in a safe Scheme implementation, they
;;; are in some sense redundant: if we don't check to see that the PROC
;;; parameter is a procedure, we'll find out anyway three lines later when
;;; we try to call the value. It's pretty easy to rip all this argument
;;; checking code out if it's inappropriate for your implementation -- just
;;; nuke every call to CHECK-ARG.

;;; On the other hand, if you *do* have a sophisticated compiler that will
;;; actually perform soft-typing and eliminate redundant checks (Rice's systems
;;; being the only possible candidate of which I'm aware), leaving these checks
;;; in can *help*, since their presence can be elided in redundant cases,
;;; and in cases where they are needed, performing the checks early, at
;;; procedure entry, can "lift" a check out of a loop.

;;; Finally, I have only checked the properties that can portably be checked
;;; with R5RS Scheme -- and this is not complete. You may wish to alter
;;; the CHECK-ARG parameter checks to perform extra, implementation-specific
;;; checks, such as procedure arity for higher-order values.

;;; The code has only these non-R4RS dependencies:
;;;   A few calls to an ERROR procedure;
;;;   Uses of the R5RS multiple-value procedure VALUES and the m-v binding
;;;     RECEIVE macro (which isn't R5RS, but is a trivial macro).
;;;   Many calls to a parameter-checking procedure check-arg:
;;;    (define (check-arg pred val caller)
;;;      (let lp ((val val))
;;;        (if (pred val) val (lp (error "Bad argument" val pred caller)))))
;;;   A few uses of the LET-OPTIONAL and :OPTIONAL macros for parsing
;;;     optional arguments.

;;; Most of these procedures use the NULL-LIST? test to trigger the
;;; base case in the inner loop or recursion. The NULL-LIST? function
;;; is defined to be a careful one -- it raises an error if passed a
;;; non-nil, non-pair value. The spec allows an implementation to use
;;; a less-careful implementation that simply defines NULL-LIST? to
;;; be NOT-PAIR?. This would speed up the inner loops of these procedures
;;; at the expense of having them silently accept dotted lists.

;;; A note on dotted lists:
;;; I, personally, take the view that the only consistent view of lists
;;; in Scheme is the view that *everything* is a list -- values such as
;;; 3 or "foo" or 'bar are simply empty dotted lists. This is due to the
;;; fact that Scheme actually has no true list type. It has a pair type,
;;; and there is an *interpretation* of the trees built using this type
;;; as lists.
;;;
;;; I lobbied to have these list-processing procedures hew to this
;;; view, and accept any value as a list argument. I was overwhelmingly
;;; overruled during the SRFI discussion phase. So I am inserting this
;;; text in the reference lib and the SRFI spec as a sort of "minority
;;; opinion" dissent.
;;;
;;; Many of the procedures in this library can be trivially redefined
;;; to handle dotted lists, just by changing the NULL-LIST? base-case
;;; check to NOT-PAIR?, meaning that any non-pair value is taken to be
;;; an empty list. For most of these procedures, that's all that is
;;; required.
;;;
;;; However, we have to do a little more work for some procedures that
;;; *produce* lists from other lists.  Were we to extend these procedures to
;;; accept dotted lists, we would have to define how they terminate the lists
;;; produced as results when passed a dotted list. I designed a coherent set
;;; of termination rules for these cases; this was posted to the SRFI-1
;;; discussion list. I additionally wrote an earlier version of this library
;;; that implemented that spec. It has been discarded during later phases of
;;; the definition and implementation of this library.
;;;
;;; The argument *against* defining these procedures to work on dotted
;;; lists is that dotted lists are the rare, odd case, and that by
;;; arranging for the procedures to handle them, we lose error checking
;;; in the cases where a dotted list is passed by accident -- e.g., when
;;; the programmer swaps a two arguments to a list-processing function,
;;; one being a scalar and one being a list. For example,
;;;     (member '(1 3 5 7 9) 7)
;;; This would quietly return #f if we extended MEMBER to accept dotted
;;; lists.
;;;
;;; The SRFI discussion record contains more discussion on this topic.

;;;; Selectors

(define (take lis k)
  (guarantee-index-fixnum k 'TAKE)
  (let recur ((lis lis) (k k))
    (if (fix:> k 0)
	(cons (car lis)
	      (recur (cdr lis) (fix:- k 1)))
	'())))

(define (drop lis k)
  (guarantee-index-fixnum k 'DROP)
  (%drop lis k))

(define (%drop lis k)
  (let iter ((lis lis) (k k))
    (if (fix:> k 0)
	(iter (cdr lis) (fix:- k 1))
	lis)))

(define (take! lis k)
  (guarantee-index-fixnum k 'TAKE!)
  (if (fix:> k 0)
      (begin
	(set-cdr! (drop lis (fix:- k 1)) '())
	lis)
      '()))

;;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list,
;;; off by K, then chasing down the list until the lead pointer falls off
;;; the end.

(define (take-right lis k)
  (guarantee-index-fixnum k 'TAKE-RIGHT)
  (let lp ((lag lis) (lead (%drop lis k)))
    (if (pair? lead)
	(lp (cdr lag) (cdr lead))
	lag)))

(define (drop-right lis k)
  (guarantee-index-fixnum k 'DROP-RIGHT)
  (let recur ((lag lis) (lead (%drop lis k)))
    (if (pair? lead)
	(cons (car lag) (recur (cdr lag) (cdr lead)))
	'())))

;;; In this function, LEAD is actually K+1 ahead of LAG. This lets
;;; us stop LAG one step early, in time to smash its cdr to ().

(define (drop-right! lis k)
  (guarantee-index-fixnum k 'DROP-RIGHT!)
  (let ((lead (%drop lis k)))
    (if (pair? lead)
	;; Standard case
	(let lp ((lag lis) (lead (cdr lead)))
	  (if (pair? lead)
	      (lp (cdr lag) (cdr lead))
	      (begin
		(set-cdr! lag '())
		lis)))
	;; Special case dropping everything -- no cons to side-effect.
	'())))

(define (split-at x k)
  (guarantee-index-fixnum k 'SPLIT-AT)
  (let recur ((lis x) (k k))
    (if (fix:> k 0)
	(receive (prefix suffix) (recur (cdr lis) (fix:- k 1))
	  (values (cons (car lis) prefix) suffix))
	(values '() lis))))

(define (split-at! x k)
  (guarantee-index-fixnum k 'SPLIT-AT!)
  (if (fix:> k 0)
      (let* ((prev (%drop x (fix:- k 1)))
	     (suffix (cdr prev)))
	(set-cdr! prev '())
	(values x suffix))
      (values '() x)))

;;;; Miscellaneous

(define (length+ x)
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
	(let ((x (cdr x))
	      (len (fix:+ len 1)))
	  (if (pair? x)
	      (let ((x (cdr x))
		    (lag (cdr lag))
		    (len (fix:+ len 1)))
		(if (eq? x lag)
		    #f
		    (lp x lag len)))
	      len))
	len)))

(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head 'APPEND-REVERSE)
	tail
	(lp (cdr rev-head) (cons (car rev-head) tail)))))

(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head 'APPEND-REVERSE!)
	tail
	(let ((next-rev (cdr rev-head)))
	  (set-cdr! rev-head tail)
	  (lp next-rev rev-head)))))

(define (concatenate lists)
  (reduce-right append '() lists))

(define (concatenate! lists)
  (reduce-right append! '() lists))

(define (count pred list1 . lists)
  (if (pair? lists)
      (let lp ((list1 list1) (lists lists) (i 0))
	(if (null-list? list1 'COUNT)
	    i
	    (receive (as ds) (%cars+cdrs lists)
	      (if (null? as)
		  i
		  (lp (cdr list1)
		      ds
		      (if (apply pred (car list1) as)
			  (fix:+ i 1)
			  i))))))
      (count-matching-items list1 pred)))

(define (zip list1 . more-lists)
  (apply map list list1 more-lists))

(define (unzip1 lis)
  (map car lis))

(define (unzip2 lis)
  (let recur ((lis lis))
    (if (null-list? lis 'UNZIP2)
	(values lis lis)
	(let ((elt (car lis)))
	  (receive (a b) (recur (cdr lis))
	    (values (cons (car elt) a)
		    (cons (cadr elt) b)))))))

(define (unzip3 lis)
  (let recur ((lis lis))
    (if (null-list? lis 'UNZIP3)
	(values lis lis lis)
	(let ((elt (car lis)))
	  (receive (a b c) (recur (cdr lis))
	    (values (cons (car elt) a)
		    (cons (cadr elt) b)
		    (cons (caddr elt) c)))))))

(define (unzip4 lis)
  (let recur ((lis lis))
    (if (null-list? lis 'UNZIP4)
	(values lis lis lis lis)
	(let ((elt (car lis)))
	  (receive (a b c d) (recur (cdr lis))
	    (values (cons (car elt) a)
		    (cons (cadr elt) b)
		    (cons (caddr elt) c)
		    (cons (cadddr elt) d)))))))

(define (unzip5 lis)
  (let recur ((lis lis))
    (if (null-list? lis 'UNZIP5)
	(values lis lis lis lis lis)
	(let ((elt (car lis)))
	  (receive (a b c d e) (recur (cdr lis))
	    (values (cons (car elt) a)
		    (cons (cadr elt) b)
		    (cons (caddr elt) c)
		    (cons (cadddr elt) d)
		    (cons (car (cddddr elt)) e)))))))

(define (unfold p f g seed #!optional tail-gen)
  (let recur ((seed seed))
    (if (p seed)
	(if (default-object? tail-gen) '() (tail-gen seed))
	(cons (f seed) (recur (g seed))))))

(define (unfold-right p f g seed #!optional tail)
  (let lp
      ((seed seed)
       (ans (if (default-object? tail) '() tail)))
    (if (p seed)
	ans
	(lp (g seed)
	    (cons (f seed) ans)))))

(define (pair-fold f zero lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans zero))
	(let ((tails (%cdrs lists)))
	  (if (null? tails)
	      ans
	      (lp tails (apply f (append! lists (list ans)))))))
      (let lp ((lis lis1) (ans zero))
	(if (null-list? lis 'PAIR-FOLD)
	    ans
	    ;; Grab the cdr now, in case F SET-CDR!s LIS.
	    (let ((tail (cdr lis)))
	      (lp tail
		  (f lis ans)))))))

(define (pair-fold-right f zero lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs)
	      zero
	      (apply f (append! lists (list (recur cdrs)))))))
      (let recur ((lis lis1))
	(if (null-list? lis 'PAIR-FOLD-RIGHT)
	    zero
	    (f lis (recur (cdr lis)))))))

(define (pair-for-each proc lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)))
	(let ((tails (%cdrs lists)))
	  (if (pair? tails)
	      (begin
		(apply proc lists)
		(lp tails)))))
      (let lp ((lis lis1))
	(if (not (null-list? lis 'PAIR-FOR-EACH))
	    ;; Grab the cdr now, in case PROC SET-CDR!s LIS.
	    (let ((tail (cdr lis)))
	      (proc lis)
	      (lp tail))))))

;;; We stop when LIS1 runs out, not when any list runs out.

(define (map! f lis1 . lists)
  (if (pair? lists)
      (let lp ((lis1 lis1) (lists lists))
	(if (not (null-list? lis1 'MAP!))
	    (receive (heads tails) (%cars+cdrs/no-test lists)
	      (set-car! lis1 (apply f (car lis1) heads))
	      (lp (cdr lis1) tails))))
      (pair-for-each (lambda (pair) (set-car! pair (f (car pair))))
		     lis1))
  lis1)

;;; Map F across L, and save up all the non-false results.

(define (filter-map f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(receive (cars cdrs) (%cars+cdrs lists)
	  (if (pair? cars)
	      (cond ((apply f cars) => (lambda (x) (cons x (recur cdrs))))
		    (else (recur cdrs))) ; Tail call in this arm.
	      '())))
      (let recur ((lis lis1))
	(if (null-list? lis 'FILTER-MAP)
	    lis
	    (let ((tail (recur (cdr lis))))
	      (cond ((f (car lis)) => (lambda (x) (cons x tail)))
		    (else tail)))))))

;;; Map F across lists, guaranteeing to go left-to-right.
;;; NOTE: Some implementations of R5RS MAP are compliant with this spec;
;;; in which case this procedure may simply be defined as a synonym for MAP.

(define (map-in-order f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(receive (cars cdrs) (%cars+cdrs lists)
	  (if (pair? cars)
	      ;; Do head first, then tail.
	      (let ((x (apply f cars)))
		(cons x (recur cdrs)))
	      '())))
      (let recur ((lis lis1))
	(if (null-list? lis 'MAP-IN-ORDER)
	    lis
	    ;; Do head first, then tail.
	    (let ((x (f (car lis))))
	      (cons x
		    (recur (cdr lis))))))))

;;;; filter, remove, partition

;;; FILTER, REMOVE, PARTITION and their destructive counterparts do not
;;; disorder the elements of their argument.

;; This FILTER shares the longest tail of L that has no deleted elements.
;; If Scheme had multi-continuation calls, they could be made more efficient.

;; Sleazing with EQ? makes this one faster.

(define (filter pred lis)
  (let recur ((lis lis))
    (if (null-list? lis 'FILTER)
	lis
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail)))	; Replicate the RECUR call so
		(if (eq? tail new-tail) lis
		    (cons head new-tail)))
	      (recur tail))))))			; this one can be a tail call.

;;; This implementation of FILTER!
;;; - doesn't cons, and uses no stack;
;;; - is careful not to do redundant SET-CDR! writes, as writes to memory are
;;;   usually expensive on modern machines, and can be extremely expensive on
;;;   modern Schemes (e.g., ones that have generational GC's).
;;; It just zips down contiguous runs of in and out elts in LIS doing the
;;; minimal number of SET-CDR!s to splice the tail of one run of ins to the
;;; beginning of the next.

(define (filter! pred lis)
  (let lp ((ans lis))
    (cond ((null-list? ans 'FILTER!) ans) ; Scan looking for
	  ((not (pred (car ans))) (lp (cdr ans)))	; first cons of result.

	  ;; ANS is the eventual answer.
	  ;; SCAN-IN: (CDR PREV) = LIS and (CAR PREV) satisfies PRED.
	  ;;          Scan over a contiguous segment of the list that
	  ;;          satisfies PRED.
	  ;; SCAN-OUT: (CAR PREV) satisfies PRED. Scan over a contiguous
	  ;;           segment of the list that *doesn't* satisfy PRED.
	  ;;           When the segment ends, patch in a link from PREV
	  ;;           to the start of the next good segment, and jump to
	  ;;           SCAN-IN.
	  (else (letrec ((scan-in (lambda (prev lis)
				    (if (pair? lis)
					(if (pred (car lis))
					    (scan-in lis (cdr lis))
					    (scan-out prev (cdr lis))))))
			 (scan-out (lambda (prev lis)
				     (let lp ((lis lis))
				       (if (pair? lis)
					   (if (pred (car lis))
					       (begin (set-cdr! prev lis)
						      (scan-in lis (cdr lis)))
					       (lp (cdr lis)))
					   (set-cdr! prev lis))))))
		  (scan-in ans (cdr ans))
		  ans)))))

;;; Answers share common tail with LIS where possible;
;;; the technique is slightly subtle.

(define (partition pred lis)
  (let recur ((lis lis))
    (if (null-list? lis 'PARTITION)
	(values lis lis)
	(let ((elt (car lis))
	      (tail (cdr lis)))
	  (receive (in out) (recur tail)
	    (if (pred elt)
		(values (if (pair? out) (cons elt in) lis) out)
		(values in (if (pair? in) (cons elt out) lis))))))))

;;; This implementation of PARTITION!
;;; - doesn't cons, and uses no stack;
;;; - is careful not to do redundant SET-CDR! writes, as writes to memory are
;;;   usually expensive on modern machines, and can be extremely expensive on
;;;   modern Schemes (e.g., ones that have generational GC's).
;;; It just zips down contiguous runs of in and out elts in LIS doing the
;;; minimal number of SET-CDR!s to splice these runs together into the result
;;; lists.

(define (partition! pred lis)
  (if (null-list? lis 'PARTITION!)
      (values lis lis)

      ;; This pair of loops zips down contiguous in & out runs of the
      ;; list, splicing the runs together. The invariants are
      ;;   SCAN-IN:  (cdr in-prev)  = LIS.
      ;;   SCAN-OUT: (cdr out-prev) = LIS.
      (letrec ((scan-in (lambda (in-prev out-prev lis)
			  (let lp ((in-prev in-prev) (lis lis))
			    (if (pair? lis)
				(if (pred (car lis))
				    (lp lis (cdr lis))
				    (begin (set-cdr! out-prev lis)
					   (scan-out in-prev lis (cdr lis))))
				(set-cdr! out-prev lis))))) ; Done.

	       (scan-out (lambda (in-prev out-prev lis)
			   (let lp ((out-prev out-prev) (lis lis))
			     (if (pair? lis)
				 (if (pred (car lis))
				     (begin (set-cdr! in-prev lis)
					    (scan-in lis out-prev (cdr lis)))
				     (lp lis (cdr lis)))
				 (set-cdr! in-prev lis)))))) ; Done.

	;; Crank up the scan&splice loops.
	(if (pred (car lis))
	    ;; LIS begins in-list. Search for out-list's first pair.
	    (let lp ((prev-l lis) (l (cdr lis)))
	      (cond ((not (pair? l)) (values lis l))
		    ((pred (car l)) (lp l (cdr l)))
		    (else (scan-out prev-l l (cdr l))
			  (values lis l))))	; Done.

	    ;; LIS begins out-list. Search for in-list's first pair.
	    (let lp ((prev-l lis) (l (cdr lis)))
	      (cond ((not (pair? l)) (values l lis))
		    ((pred (car l))
		     (scan-in l prev-l (cdr l))
		     (values l lis))		; Done.
		    (else (lp l (cdr l)))))))))

(define-integrable (remove  pred l) (filter  (lambda (x) (not (pred x))) l))
(define-integrable (remove! pred l) (filter! (lambda (x) (not (pred x))) l))

;;;; Right-duplicate deletion

;;; delete-duplicates delete-duplicates!
;;;
;;; Beware -- these are N^2 algorithms. To efficiently remove duplicates
;;; in long lists, sort the list to bring duplicates together, then use a
;;; linear-time algorithm to kill the dups. Or use an algorithm based on
;;; element-marking. The former gives you O(n lg n), the latter is linear.

(define (delete-duplicates lis #!optional elt=)
  (let ((elt= (if (default-object? elt=) equal? elt=)))
    (let recur ((lis lis))
      (if (null-list? lis 'DELETE-DUPLICATES)
	  lis
	  (let* ((x (car lis))
		 (tail (cdr lis))
		 (new-tail (recur (delete x tail elt=))))
	    (if (eq? tail new-tail) lis (cons x new-tail)))))))

(define (delete-duplicates! lis #!optional elt=)
  (let ((elt= (if (default-object? elt=) equal? elt=)))
    (let recur ((lis lis))
      (if (null-list? lis 'DELETE-DUPLICATES!)
	  lis
	  (let* ((x (car lis))
		 (tail (cdr lis))
		 (new-tail (recur (delete! x tail elt=))))
	    (if (eq? tail new-tail) lis (cons x new-tail)))))))

(define (find pred list)
  (cond ((find-tail pred list) => car)
	(else #f)))

(define (find-tail pred list)
  (let lp ((list list))
    (and (not (null-list? list 'FIND-TAIL))
	 (if (pred (car list)) list
	     (lp (cdr list))))))

(define (take-while pred lis)
  (let recur ((lis lis))
    (if (null-list? lis 'TAKE-WHILE)
	'()
	(let ((x (car lis)))
	  (if (pred x)
	      (cons x (recur (cdr lis)))
	      '())))))

(define (drop-while pred lis)
  (let lp ((lis lis))
    (if (null-list? lis 'DROP-WHILE)
	'()
	(if (pred (car lis))
	    (lp (cdr lis))
	    lis))))

(define (take-while! pred lis)
  (if (or (null-list? lis 'TAKE-WHILE!)
	  (not (pred (car lis))))
      '()
      (begin
	(let lp ((prev lis) (rest (cdr lis)))
	  (if (pair? rest)
	      (let ((x (car rest)))
		(if (pred x) (lp rest (cdr rest))
		    (set-cdr! prev '())))))
	lis)))

(define (span pred lis)
  (let recur ((lis lis))
    (if (null-list? lis 'SPAN)
	(values '() '())
	(let ((x (car lis)))
	  (if (pred x)
	      (receive (prefix suffix) (recur (cdr lis))
		(values (cons x prefix) suffix))
	      (values '() lis))))))

(define (span! pred lis)
  (if (or (null-list? lis 'SPAN!)
	  (not (pred (car lis))))
      (values '() lis)
      (let ((suffix (let lp ((prev lis) (rest (cdr lis)))
		      (if (null-list? rest 'SPAN!)
			  rest
			  (let ((x (car rest)))
			    (if (pred x) (lp rest (cdr rest))
				(begin (set-cdr! prev '())
				       rest)))))))
	(values lis suffix))))

(define (break pred #!optional lis)
  ;; This is a kludge to retain MIT/GNU Scheme's BREAK.
  (if (default-object? lis)
      (break-both pred)
      (span (lambda (x) (not (pred x))) lis)))

(define (break! pred lis)
  (span! (lambda (x) (not (pred x))) lis))

(define (any pred lis1 . lists)
  (if (pair? lists)
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
	(and (pair? heads)
	     (let lp ((heads heads) (tails tails))
	       (receive (next-heads next-tails) (%cars+cdrs tails)
		 (if (pair? next-heads)
		     (or (apply pred heads)
			 (lp next-heads next-tails))
		     (apply pred heads))))))
      (and (not (null-list? lis1 'ANY))
	   (let lp ((head (car lis1)) (tail (cdr lis1)))
	     (if (null-list? tail 'ANY)
		 (pred head)
		 (or (pred head)
		     (lp (car tail) (cdr tail))))))))

(define (every pred lis1 . lists)
  (if (pair? lists)
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
	(or (not (pair? heads))
	    (let lp ((heads heads) (tails tails))
	      (receive (next-heads next-tails) (%cars+cdrs tails)
		(if (pair? next-heads)
		    (and (apply pred heads)
			 (lp next-heads next-tails))
		    (apply pred heads))))))
      (or (null-list? lis1 'EVERY)
	  (let lp ((head (car lis1)) (tail (cdr lis1)))
	    (if (null-list? tail 'EVERY)
		(pred head)
		(and (pred head)
		     (lp (car tail) (cdr tail))))))))

(define (list-index pred lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (n 0))
	(receive (heads tails) (%cars+cdrs lists)
	  (and (pair? heads)
	       (if (apply pred heads) n
		   (lp tails (fix:+ n 1))))))
      (let lp ((lis lis1) (n 0))
	(and (not (null-list? lis 'LIST-INDEX))
	     (if (pred (car lis))
		 n
		 (lp (cdr lis) (fix:+ n 1)))))))

;;;; Lists-as-sets

;;; This is carefully tuned code; do not modify casually.
;;; - It is careful to share storage when possible;
;;; - Side-effecting code tries not to perform redundant writes.
;;; - It tries to avoid linear-time scans in special cases where constant-time
;;;   computations can be performed.
;;; - It relies on similar properties from the other list-lib procs it calls.
;;;   For example, it uses the fact that the implementations of MEMBER and
;;;   FILTER in this source code share longest common tails between args
;;;   and results to get structure sharing in the lset procedures.

(define (%lset2<= = lis1 lis2)
  (every (lambda (x) (member x lis2 =)) lis1))

(define (lset<= = . lists)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2 (car rest))  (rest (cdr rest)))
	      (and (or (eq? s2 s1)	; Fast path
		       (%lset2<= = s1 s2)) ; Real test
		   (lp s2 rest)))))))

(define (lset= = . lists)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2   (car rest))
		  (rest (cdr rest)))
	      (and (or (eq? s1 s2)	; Fast path
		       (and (%lset2<= = s1 s2) (%lset2<= = s2 s1))) ; Real test
		   (lp s2 rest)))))))

(define (lset-adjoin = lis . elts)
  (fold (lambda (elt ans) (if (member elt ans =) ans (cons elt ans)))
	lis elts))

(define (lset-union = . lists)
  (reduce (lambda (lis ans)		; Compute ANS + LIS.
	    (cond ((null? lis) ans)	; Don't copy any lists
		  ((null? ans) lis) 	; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (fold (lambda (elt ans) (if (any (lambda (x) (= x elt)) ans)
					       ans
					       (cons elt ans)))
			 ans lis))))
	  '() lists))

(define (lset-union! = . lists)
  (reduce (lambda (lis ans)
	    ;; Splice new elts of LIS onto the front of ANS.
	    (cond ((null? lis) ans)	; Don't copy any lists
		  ((null? ans) lis) 	; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (pair-fold (lambda (pair ans)
				(let ((elt (car pair)))
				  (if (any (lambda (x) (= x elt)) ans)
				      ans
				      (begin (set-cdr! pair ans) pair))))
			      ans lis))))
	  '()
	  lists))

(define (lset-intersection = lis1 . lists)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((any (lambda (list)
		  (null-list? list 'LSET-INTERSECTION))
		lists)
	   '())		; Short cut
	  ((null? lists)          lis1)		; Short cut
	  (else (filter (lambda (x)
			  (every (lambda (lis) (member x lis =)) lists))
			lis1)))))

(define (lset-intersection! = lis1 . lists)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((any (lambda (list)
		  (null-list? list 'LSET-INTERSECTION!))
		lists)
	   '())		; Short cut
	  ((null? lists)          lis1)		; Short cut
	  (else (filter! (lambda (x)
			   (every (lambda (lis) (member x lis =)) lists))
			 lis1)))))

(define (lset-difference = lis1 . lists)
  (let ((lists (filter pair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (filter (lambda (x)
			  (every (lambda (lis) (not (member x lis =)))
				 lists))
			lis1)))))

(define (lset-difference! = lis1 . lists)
  (let ((lists (filter pair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (filter! (lambda (x)
			   (every (lambda (lis) (not (member x lis =)))
				  lists))
			 lis1)))))

(define (lset-xor = . lists)
  (reduce (lambda (b a)			; Compute A xor B:
	    ;; Note that this code relies on the constant-time
	    ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	    ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	    ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	    ;; a careful case analysis to see it, but it's carefully
	    ;; built in.

	    ;; Compute a-b and a^b, then compute b-(a^b) and
	    ;; cons it onto the front of a-b.
	    (receive (a-b a-int-b)   (lset-diff+intersection = a b)
	      (cond ((null? a-b)     (lset-difference = b a))
		    ((null? a-int-b) (append b a))
		    (else (fold (lambda (xb ans)
				  (if (member xb a-int-b =) ans (cons xb ans)))
				a-b
				b)))))
	  '() lists))

(define (lset-xor! = . lists)
  (reduce (lambda (b a)			; Compute A xor B:
	    ;; Note that this code relies on the constant-time
	    ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	    ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	    ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	    ;; a careful case analysis to see it, but it's carefully
	    ;; built in.

	    ;; Compute a-b and a^b, then compute b-(a^b) and
	    ;; cons it onto the front of a-b.
	    (receive (a-b a-int-b)   (lset-diff+intersection! = a b)
	      (cond ((null? a-b)     (lset-difference! = b a))
		    ((null? a-int-b) (append! b a))
		    (else (pair-fold (lambda (b-pair ans)
				       (if (member (car b-pair) a-int-b =)
					   ans
					   (begin
					     (set-cdr! b-pair ans)
					     b-pair)))
				     a-b
				     b)))))
	  '() lists))

(define (lset-diff+intersection = lis1 . lists)
  (cond ((every (lambda (list)
		  (null-list? list 'LSET-DIFF+INTERSECTION))
		lists)
	 (values lis1 '()))	; Short cut
	((memq lis1 lists)        (values '() lis1))	; Short cut
	(else (partition (lambda (elt)
			   (not (any (lambda (lis) (member elt lis =))
				     lists)))
			 lis1))))
(define (lset-diff+intersection! = lis1 . lists)
  (cond ((every (lambda (list)
		  (null-list? list 'LSET-DIFF+INTERSECTION!))
		lists)
	 (values lis1 '()))	; Short cut
	((memq lis1 lists)        (values '() lis1))	; Short cut
	(else (partition! (lambda (elt)
			    (not (any (lambda (lis) (member elt lis =))
				      lists)))
			  lis1))))

;;;; Utilities

;;; These little internal utilities are used by the general
;;; fold & mapper funs for the n-ary cases.  It'd be nice if they got inlined.
;;; One the other hand, the n-ary cases are painfully inefficient as it is.
;;; An aggressive implementation should simply re-write these functions
;;; for raw efficiency; I have written them for as much clarity, portability,
;;; and simplicity as can be achieved.
;;;
;;; I use the dreaded call/cc to do local aborts. A good compiler could
;;; handle this with extreme efficiency. An implementation that provides
;;; a one-shot, non-persistent continuation grabber could help the compiler
;;; out by using that in place of the call/cc's in these routines.
;;;
;;; These functions have funky definitions that are precisely tuned to
;;; the needs of the fold/map procs -- for example, to minimize the number
;;; of times the argument lists need to be examined.

(define (%cdrs lists)
  ;; Return (map cdr lists).
  ;; However, if any element of LISTS is empty, just abort and return '().
  (let loop ((lists lists) (cdrs '()))
    (if (pair? lists)
	(if (null-list? (car lists) #f)
	    '()
	    (loop (cdr lists)
		  (cons (cdar lists) cdrs)))
	(reverse! cdrs))))

(define (%cars+ lists last-elt)
  (let recur ((lists lists))
    (if (pair? lists)
	(cons (caar lists) (recur (cdr lists)))
	(list last-elt))))

;;; LISTS is a (not very long) non-empty list of lists.
;;; Return two lists: the cars & the cdrs of the lists.
;;; However, if any of the lists is empty, just abort and return [() ()].

(define (%cars+cdrs lists)
  (let loop ((lists lists) (cars '()) (cdrs '()))
    (if (pair? lists)
	(if (null-list? (car lists) #f)
	    (values '() '())
	    (loop (cdr lists)
		  (cons (caar lists) cars)
		  (cons (cdar lists) cdrs)))
	(values (reverse! cars) (reverse! cdrs)))))

;;; Like %CARS+CDRS, but we pass in a final elt tacked onto the end of the
;;; cars list.  What a hack.

(define (%cars+cdrs+ lists cars-final)
  (let loop ((lists lists) (cars (list cars-final)) (cdrs '()))
    (if (pair? lists)
	(if (null-list? (car lists) #f)
	    (values '() '())
	    (loop (cdr lists)
		  (cons (caar lists) cars)
		  (cons (cdar lists) cdrs)))
	(values (reverse! cars) (reverse! cdrs)))))

;;; Like %CARS+CDRS, but blow up if any list is empty.
(define (%cars+cdrs/no-test lists)
  (let loop ((lists lists) (cars '()) (cdrs '()))
    (if (pair? lists)
	(loop (cdr lists)
	      (cons (caar lists) cars)
	      (cons (cdar lists) cdrs))
	(values (reverse! cars) (reverse! cdrs)))))