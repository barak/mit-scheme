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

;;;; Compiled Code Information: Structures
;;; package: (runtime compiler-info)

(declare (usual-integrations))

;;; Keep in sync with "compiler/base/toplev.scm" and "compiler/base/crsend.scm".

(define (dbg-info-vector? object)
  (and (vector? object)
       ;; Length 6 can be removed after 9.3 release.
       (or (fix:= 6 (vector-length object))
	   (fix:= 7 (vector-length object)))
       (eq? '|#[(runtime compiler-info)dbg-info-vector]|
	    (vector-ref object 0))))

(define-integrable (dbg-info-vector/compilation-type v)
  (vector-ref v 1))

(define-integrable (dbg-info-vector/root-block v)
  (vector-ref v 2))

(define-integrable (dbg-info-vector/other-blocks v)
  (vector-ref v 3))

(define-integrable (dbg-info-vector/tl-bound v)
  (vector-ref v 4))

(define-integrable (dbg-info-vector/tl-free v)
  (vector-ref v 5))

(define-integrable (dbg-info-vector/tl-metadata v)
  (vector-ref v 6))

(define (dbg-info-vector/blocks-vector info)
  (guarantee dbg-info-vector? info 'dbg-info-vector/blocks-vector)
  (vector-append (vector (dbg-info-vector/root-block info))
		 (dbg-info-vector/other-blocks info)))

(define (dbg-info-vector/purification-root info)
  (guarantee dbg-info-vector? info 'dbg-info-vector/purification-root)
  (dbg-info-vector/other-blocks info))

(define-structure (dbg-info
		   (type vector)
		   (named '|#[(runtime compiler-info)dbg-info]|)
		   (conc-name dbg-info/))
  (expression #f read-only #t)		;dbg-expression
  (procedures #f read-only #t)		;vector of dbg-procedure
  (continuations #f read-only #t)	;vector of dbg-continuation
  (labels #f read-only #f)		;vector of dbg-label, sorted by offset
  )

(define-structure (dbg-expression
		   (type vector)
		   (named '|#[(runtime compiler-info)dbg-expression]|)
		   (conc-name dbg-expression/))
  (block #f read-only #t)		;dbg-block
  (label #f)				;dbg-label
  )

(define-integrable (dbg-expression/label-offset expression)
  (dbg-label/offset (dbg-expression/label expression)))

(define-structure (dbg-procedure
		   (type vector)
		   (named '|#[(runtime compiler-info)dbg-procedure]|)
		   (constructor
		    make-dbg-procedure
		    (block label type name required optional rest auxiliary
			   source-code))
		   (conc-name dbg-procedure/))
  (block #f read-only #t)		;dbg-block
  (label #f)				;dbg-label
  (type #f read-only #t)
  (name #f read-only #t)		;procedure's name
  (required #f read-only #t)		;names of required arguments
  (optional #f read-only #t)		;names of optional arguments
  (rest #f read-only #t)		;name of rest argument, or #F
  (auxiliary #f read-only #t)		;names of internal definitions
  (external-label #f)			;for closure, external entry
  (source-code #f read-only #t)		;SCode
  )

(define (dbg-procedure/label-offset procedure)
  (dbg-label/offset
   (or (dbg-procedure/external-label procedure)
       (dbg-procedure/label procedure))))

(define-integrable (dbg-procedure<? x y)
  (< (dbg-procedure/label-offset x) (dbg-procedure/label-offset y)))

(define-structure (dbg-continuation
		   (type vector)
		   (named '|#[(runtime compiler-info)dbg-continuation]|)
		   (conc-name dbg-continuation/))
  (block #f read-only #t)		;dbg-block
  (label #f)				;dbg-label
  (type #f read-only #t)
  (offset #f read-only #t)		;difference between sp and block
  (source-code #f read-only #t)
  )

(define-integrable (dbg-continuation/label-offset continuation)
  (dbg-label/offset (dbg-continuation/label continuation)))

(define-integrable (dbg-continuation<? x y)
  (< (dbg-continuation/label-offset x) (dbg-continuation/label-offset y)))

(define-structure (dbg-block
		   (type vector)
		   (named '|#[(runtime compiler-info)dbg-block]|)
		   (constructor
		    make-dbg-block
		    (type parent original-parent layout stack-link))
		   (conc-name dbg-block/))
  (type #f read-only #t)		;continuation, stack, closure, ic
  (parent #f read-only #t)		;parent block, or #F
  (original-parent #f read-only #t)	;for closures, closing block
  (layout #f read-only #t)		;vector of names, except #F for ic
  (stack-link #f read-only #t)		;next block on stack, or #F
  (procedure #f)			;procedure which this is block of
  )

(define-structure (dbg-variable
		   (type vector)
		   (named '|#[(runtime compiler-info)dbg-variable]|)
		   (conc-name dbg-variable/))
  (name #f read-only #t)		;symbol
  (type #f read-only #t)		;normal, cell, integrated
  value					;for integrated, the value
  )

;;; Various names used in `layout' to identify things that wouldn't otherwise
;;; have names.

(define-integrable dbg-block-name/dynamic-link
  '|#[(runtime compiler-info)dynamic-link]|)

(define-integrable dbg-block-name/ic-parent
  '|#[(runtime compiler-info)ic-parent]|)

(define-integrable dbg-block-name/normal-closure
  '|#[(runtime compiler-info)normal-closure]|)

(define-integrable dbg-block-name/return-address
  '|#[(runtime compiler-info)return-address]|)

(define-integrable dbg-block-name/static-link
  '|#[(runtime compiler-info)static-link]|)

(define-integrable make-dbg-label-2 cons)
(define-integrable dbg-label/name car)
(define-integrable (dbg-label/offset label) (abs (cdr label)))
(define-integrable (dbg-label/external? label) (negative? (cdr label)))
(define-integrable (dbg-label/names label) (list (car label)))

(define (set-dbg-label/external?! label external?)
  (let ((offset (cdr label)))
    (if (if external?
	    (not (negative? offset))
	    (negative? offset))
	(set-cdr! label (- offset))))
  unspecific)

;;;; Debugging-info wrappers

(define (compiled-code-block/debugging-wrapper block)
  (let ((wrapper (compiled-code-block/debugging-info block)))
    (cond ((debugging-wrapper-v2? wrapper)
	   (let ((v (vector-grow wrapper 7)))
	     (vector-set! v 1 3)
	     (vector-set! v 6 #f)
	     (set-compiled-code-block/debugging-info! block v)
	     v))
	  ((debugging-wrapper-v3? wrapper) wrapper)
	  (else #f))))

(define (debugging-wrapper-v2? wrapper)
  (and (vector? wrapper)
       (fix:= (vector-length wrapper) 6)
       (eqv? (vector-ref wrapper 1) 2)
       (debugging-wrapper-common? wrapper)))

(define (debugging-wrapper-v3? wrapper)
  (and (vector? wrapper)
       (fix:= (vector-length wrapper) 7)
       (eqv? (vector-ref wrapper 1) 3)
       (debugging-wrapper-common? wrapper)
       (debugging-library-name? (vector-ref wrapper 6))))

(define (debugging-library-name? object)
  (or (not object)
      (eq? object 'program)
      (library-name? object)))

(define (debugging-library-name=? n1 n2)
  (or (eq? n1 n2)
      (and (library-name? n1)
	   (library-name? n2)
	   (library-name=? n1 n2))))

(define (debugging-wrapper-common? wrapper)
  (and (eq? (vector-ref wrapper 0) 'debugging-info-wrapper)
       (or (and (not (vector-ref wrapper 2))
		(not (vector-ref wrapper 3))
		(not (vector-ref wrapper 4))
		(dbg-info? (vector-ref wrapper 5)))
	   (and (dbg-info-key? (vector-ref wrapper 2))
		(debug-info-pathname? (vector-ref wrapper 3))
		(index-fixnum? (vector-ref wrapper 4))
		(or (not (vector-ref wrapper 5))
		    (dbg-info? (vector-ref wrapper 5)))))))

(define (debugging-wrapper/version wrapper)
  (vector-ref wrapper 1))

(define (debugging-wrapper/key wrapper)
  (vector-ref wrapper 2))

(define (debugging-wrapper/pathname wrapper)
  (convert-old-style-pathname (vector-ref wrapper 3)))

(define (set-debugging-wrapper/pathname! wrapper pathname)
  (vector-set! wrapper 3 pathname))

(define (debugging-wrapper/index wrapper)
  (vector-ref wrapper 4))

(define (debugging-wrapper/info wrapper)
  (vector-ref wrapper 5))

(define (set-debugging-wrapper/info! wrapper info)
  (vector-set! wrapper 5 info))

(define (debugging-wrapper/library-name wrapper)
  (vector-ref wrapper 6))

(define (canonicalize-file-wrapper wrapper)
  (cond ((debugging-file-wrapper-v2? wrapper)
	 (let ((v (vector-grow wrapper 5)))
	   (vector-set! v 1 3)
	   (vector-set! v 4 #f)
	   v))
	((or (debugging-file-wrapper-v3? wrapper)
	     (debugging-library-wrapper? wrapper))
	 wrapper)
	(else #f)))

(define (debugging-file-wrapper-v2? wrapper)
  (and (vector? wrapper)
       (fix:= (vector-length wrapper) 4)
       (eqv? (vector-ref wrapper 1) 2)
       (debugging-file-wrapper-common? wrapper)))

(define (debugging-file-wrapper-v3? wrapper)
  (and (vector? wrapper)
       (fix:= (vector-length wrapper) 5)
       (eqv? (vector-ref wrapper 1) 3)
       (debugging-file-wrapper-common? wrapper)
       (debugging-library-name? (vector-ref wrapper 4))))

(define (debugging-file-wrapper-common? wrapper)
  (and (eq? (vector-ref wrapper 0) 'debugging-file-wrapper)
       (dbg-info-key? (vector-ref wrapper 2))
       (let ((info (vector-ref wrapper 3)))
	 (and (vector? info)
	      (fix:>= (vector-length info) 1)
	      (vector-every dbg-info? info)))))

(define (debugging-file-wrapper/version wrapper)
  (vector-ref wrapper 1))

(define (debugging-file-wrapper/key wrapper)
  (vector-ref wrapper 2))

(define (debugging-file-wrapper/info wrapper)
  (vector-ref wrapper 3))

(define (debugging-file-wrapper/library-name wrapper)
  (vector-ref wrapper 4))

(define (dbg-info-key? object)
  (or (and (bytevector? object)
	   (fix:= (bytevector-length object) 32))
      ;; The following can be removed after 9.3 release:
      (and ((ucode-primitive string? 1) object)
	   (fix:= ((ucode-primitive string-length 1) object) 32))))

(define (dbg-info-key=? k1 k2)
  (or (and k1 k2 (equal? k1 k2))
      (and (not k1) (not k2))))

(define (debugging-library-wrapper? wrapper)
  (and (vector? wrapper)
       (fix:= (vector-length wrapper) 4)
       (eq? (vector-ref wrapper 0) 'debugging-library-wrapper)
       (eqv? (vector-ref wrapper 1) 3)
       (dbg-info-key? (vector-ref wrapper 2))
       (let ((info (vector-ref wrapper 3)))
	 (and (vector? info)
	      (fix:>= (vector-length info) 1)
	      (vector-every debugging-file-wrapper-v3? info)))))

(define (debugging-library-wrapper/version wrapper)
  (vector-ref wrapper 1))

(define (debugging-library-wrapper/key wrapper)
  (vector-ref wrapper 2))

(define (debugging-library-wrapper/file-wrappers wrapper)
  (vector-ref wrapper 3))

(define (get-wrapped-dbg-info from-file from-block)
  (let ((lookup-by-index
	 (lambda (from-file)
	   (let ((v (debugging-file-wrapper/info from-file))
		 (index (debugging-wrapper/index from-block)))
	     (and (fix:< index (vector-length v))
		  (vector-ref v index))))))
    (cond ((debugging-file-wrapper-v3? from-file)
	   (and (dbg-info-key=? (debugging-wrapper/key from-block)
				(debugging-file-wrapper/key from-file))
		(lookup-by-index from-file)))
	  ((debugging-library-wrapper? from-file)
	   (and (dbg-info-key=? (debugging-wrapper/key from-block)
				(debugging-library-wrapper/key from-file))
		(let ((name (debugging-wrapper/library-name from-block))
		      (v (debugging-library-wrapper/file-wrappers from-file)))
		  (let ((n (vector-length v)))
		    (let loop ((i 0))
		      (and (fix:< i n)
			   (if (debugging-library-name=?
				name
				(debugging-file-wrapper/library-name
				 (vector-ref v i)))
			       (lookup-by-index (vector-ref v i))
			       (loop (fix:+ i 1)))))))))
	  (else #f))))

(define (debug-info-pathname? object)
  (or (string? object)
      (old-style-pathname? object)))

;; This can be removed after the 9.3 release.
(define (old-style-pathname? object)
  (and (vector? object)
       (fix:= 7 (vector-length object))
       (eq? '|#[(runtime pathname)pathname]| (vector-ref object 0))))

;; This can be removed after the 9.3 release.
(define (convert-old-style-pathname object)
  (if (old-style-pathname? object)
      (%make-pathname (let ((host (vector-ref object 1)))
			(if (and (vector? host)
				 (fix:= 3 (vector-length host))
				 (eq? '|#[(runtime pathname)host]|
				      (vector-ref host 0)))
			    (%make-host (vector-ref host 1)
					(vector-ref host 2))
			    host))
		      (vector-ref object 2)
		      (vector-ref object 3)
		      (vector-ref object 4)
		      (vector-ref object 5)
		      (vector-ref object 6))
      object))