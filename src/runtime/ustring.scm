#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Unicode strings
;;; package: (runtime ustring)

;;; For simplicity, the implementation uses a 24-bit encoding for non-8-bit
;;; strings.  This is not a good long-term approach and should be revisited once
;;; the runtime system has been converted to this string abstraction.

(declare (usual-integrations))

;;;; Code-point vectors

(define-integrable (cp->byte-index index)
  (fix:* index 3))

(define-integrable (byte->cp-index index)
  (fix:quotient index 3))

(define-integrable (make-cp b0 b1 b2)
  (fix:+ b0
	 (fix:+ (fix:lsh b1 8)
		(fix:lsh b2 16))))

(define-integrable (cp-byte-0 cp) (fix:and cp #xFF))
(define-integrable (cp-byte-1 cp) (fix:and (fix:lsh cp -8) #xFF))
(define-integrable (cp-byte-2 cp) (fix:and (fix:lsh cp -16) #x1F))

(define (make-cp-vector length)
  (make-bytevector (cp->byte-index length)))

(define (cp-vector-length bytes)
  (byte->cp-index (bytevector-length bytes)))

(define (cp-vector-ref bytes index)
  (let ((i (cp->byte-index index)))
    (make-cp (bytevector-u8-ref bytes i)
	     (bytevector-u8-ref bytes (fix:+ i 1))
	     (bytevector-u8-ref bytes (fix:+ i 2)))))

(define (cp-vector-set! bytes index cp)
  (let ((i (cp->byte-index index)))
    (bytevector-u8-set! bytes i (cp-byte-0 cp))
    (bytevector-u8-set! bytes (fix:+ i 1) (cp-byte-1 cp))
    (bytevector-u8-set! bytes (fix:+ i 2) (cp-byte-2 cp))))

(define-integrable (cp-vector-copy! to at from start end)
  (bytevector-copy! to (cp->byte-index at)
		    from (cp->byte-index start) (cp->byte-index end)))

;;;; Component types

(define-primitives
  (legacy-string? string? 1)
  (legacy-string-allocate string-allocate 1)
  (legacy-string-length string-length 1)
  (legacy-string-ref string-ref 2)
  (legacy-string-set! string-set! 3))

(define (full-string? object)
  (and (%record? object)
       (fix:= 2 (%record-length object))
       (eq? %full-string-tag (%record-ref object 0))))

(define-integrable (full-string-allocate k)
  (%record %full-string-tag (make-cp-vector k)))

(define-integrable %full-string-tag
  '|#[(runtime ustring)full-string]|)

(define-integrable (%full-string-cp-vector string)
  (%record-ref string 1))

(define (make-full-string k #!optional char)
  (let ((string (full-string-allocate k)))
    (if (not (default-object? char))
	(string-fill! string char))
    string))

(define-integrable (full-string-length string)
  (cp-vector-length (%full-string-cp-vector string)))

(define-integrable (%full-string-ref string index)
  (integer->char (cp-vector-ref (%full-string-cp-vector string) index)))

(define-integrable (%full-string-set! string index char)
  (cp-vector-set! (%full-string-cp-vector string) index (char->integer char)))

(define (slice? object)
  (and (%record? object)
       (fix:= 4 (%record-length object))
       (eq? %slice-tag (%record-ref object 0))))

(define-integrable (make-slice string start length)
  (%record %slice-tag string start length))

(define-integrable %slice-tag
  '|#[(runtime ustring)slice]|)

(define-integrable (slice-string slice) (%record-ref slice 1))
(define-integrable (slice-start slice) (%record-ref slice 2))
(define-integrable (slice-length slice) (%record-ref slice 3))

(define (slice-end slice)
  (fix:+ (slice-start slice) (slice-length slice)))

(define (translate-slice string start end)
  (if (slice? string)
      (values (slice-string string)
	      (fix:+ (slice-start string) start)
	      (fix:+ (slice-start string) end))
      (values string start end)))

(define (register-ustring-predicates!)
  (register-predicate! string? 'string)
  (register-predicate! legacy-string? 'legacy-string '<= string?)
  (register-predicate! full-string? 'full-string '<= string?)
  (register-predicate! slice? 'string-slice '<= string?)
  (register-predicate! 8-bit-string? '8-bit-string '<= string?)
  (register-predicate! ->string-component? '->string-component))

;;;; Basic operations

(define (string? object)
  (or (legacy-string? object)
      (full-string? object)
      (slice? object)))

(define (make-string k #!optional char)
  (guarantee index-fixnum? k 'make-string)
  (if (fix:> k 0)
      (make-full-string k char)
      (legacy-string-allocate 0)))

(define (string-length string)
  (cond ((legacy-string? string) (legacy-string-length string))
	((full-string? string) (full-string-length string))
	((slice? string) (slice-length string))
	(else (error:not-a string? string 'string-length))))

(define (string-ref string index)
  (guarantee index-fixnum? index 'string-ref)
  (cond ((legacy-string? string)
	 (legacy-string-ref string index))
	((full-string? string)
	 (if (not (fix:< index (full-string-length string)))
	     (error:bad-range-argument index 'string-ref))
	 (%full-string-ref string index))
	((slice? string)
	 (let ((string* (slice-string string))
	       (index* (fix:+ (slice-start string) index)))
	   (if (legacy-string? string*)
	       (legacy-string-ref string* index*)
	       (%full-string-ref string* index*))))
	(else
	 (error:not-a string? string 'string-ref))))

(define (string-set! string index char)
  (guarantee index-fixnum? index 'string-set!)
  (guarantee bitless-char? char 'string-set!)
  (cond ((legacy-string? string)
	 (legacy-string-set! string index char))
	((full-string? string)
	 (if (not (fix:< index (full-string-length string)))
	     (error:bad-range-argument index 'string-set!))
	 (%full-string-set! string index char))
	((slice? string)
	 (let ((string* (slice-string string))
	       (index* (fix:+ (slice-start string) index)))
	   (if (legacy-string? string*)
	       (legacy-string-set! string* index* char)
	       (%full-string-set! string* index* char))))
	(else
	 (error:not-a string? string 'string-set!))))

(define (string-slice string #!optional start end)
  (let* ((len (string-length string))
	 (end (fix:end-index end len 'string-slice))
	 (start (fix:start-index start end 'string-slice)))
    (cond ((and (fix:= start 0) (fix:= end len))
	   string)
	  ((slice? string)
	   (make-slice (slice-string string)
		       (fix:+ (slice-start string) start)
		       (fix:- end start)))
	  (else
	   (make-slice string
		       start
		       (fix:- end start))))))

;;;; Streaming build

(define (string-builder)
  (let ((builder
	 (make-sequence-builder (lambda () (full-string-allocate 16))
				string-length
				string-set!
				string-builder:finish-build)))
    (lambda (#!optional object)
      (cond ((default-object? object) ((builder 'build)))
	    ((bitless-char? object) ((builder 'append-element!) object))
	    ((string? object) ((builder 'append-sequence!) object))
	    ((memq object '(empty? count reset!)) ((builder object)))
	    (else (error "Not a char or string:" object))))))

(define (string-builder:finish-build parts)
  (let ((result
	 (do ((parts parts (cdr parts))
	      (n 0 (fix:+ n (cdar parts)))
	      (8-bit? #t
		      (and 8-bit?
			   (string-8-bit?
			    (string-slice (caar parts) 0 (cdar parts))))))
	     ((not (pair? parts))
	      (if 8-bit?
		  (legacy-string-allocate n)
		  (full-string-allocate n))))))
    (do ((parts parts (cdr parts))
	 (i 0 (fix:+ i (cdar parts))))
	((not (pair? parts)))
      (string-copy! result i (caar parts) 0 (cdar parts)))
    result))

;;;; Copy

(define (string-copy! to at from #!optional start end)
  (let* ((end (fix:end-index end (string-length from) 'string-copy!))
	 (start (fix:start-index start end 'string-copy!)))
    (guarantee index-fixnum? at 'string-copy!)
    (let ((final-at (fix:+ at (fix:- end start))))
      (if (not (fix:<= final-at (string-length to)))
	  (error:bad-range-argument to 'string-copy!))
      (receive (to at)
	  (if (slice? to)
	      (values (slice-string to)
		      (fix:+ (slice-start to) at))
	      (values to at))
	(receive (from start end) (translate-slice from start end)
	  (if (legacy-string? to)
	      (if (legacy-string? from)
		  (copy-loop legacy-string-set! to at
			     legacy-string-ref from start end)
		  (copy-loop legacy-string-set! to at
			     %full-string-ref from start end))
	      (if (legacy-string? from)
		  (copy-loop %full-string-set! to at
			     legacy-string-ref from start end)
		  (%full-string-copy! to at from start end)))))
      final-at)))

(define-integrable (%full-string-copy! to at from start end)
  (cp-vector-copy! (%full-string-cp-vector to) at
		   (%full-string-cp-vector from) start end))

(define (string-copy string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-copy))
	 (start (fix:start-index start end 'string-copy)))
    (receive (string start end) (translate-slice string start end)
      (cond ((legacy-string? string)
	     (let ((to (legacy-string-allocate (fix:- end start))))
	       (copy-loop legacy-string-set! to 0
			  legacy-string-ref string start end)
	       to))
	    ((%full-string-8-bit? string start end)
	     (let ((to (legacy-string-allocate (fix:- end start))))
	       (copy-loop legacy-string-set! to 0
			  %full-string-ref string start end)
	       to))
	    (else
	     (let ((to (full-string-allocate (fix:- end start))))
	       (%full-string-copy! to 0 string start end)
	       to))))))

(define (string-head string end)
  (string-copy string 0 end))

(define (string-tail string start)
  (string-copy string start))

;;;; Compare

;; Non-Unicode implementation, acceptable to R7RS.
(define-integrable (string-compare string1 string2 if= if< if>)
  (let ((end1 (string-length string1))
	(end2 (string-length string2)))
    (let ((end (fix:min end1 end2)))
      (let loop ((i 0))
	(if (fix:< i end)
	    (let ((c1 (string-ref string1 i))
		  (c2 (string-ref string2 i)))
	      (cond ((char<? c1 c2) (if<))
		    ((char<? c2 c1) (if>))
		    (else (loop (fix:+ i 1)))))
	    (cond ((fix:< end1 end2) (if<))
		  ((fix:< end2 end1) (if>))
		  (else (if=))))))))

(define-integrable (string-compare-ci string1 string2 if= if< if>)
  (string-compare (string-foldcase string1)
		  (string-foldcase string2)
		  if=
		  if<
		  if>))

(define-integrable (true) #t)
(define-integrable (false) #f)

(define-integrable (%string-comparison-maker if= if< if>)
  (lambda (string1 string2)
    (string-compare string1 string2 if= if< if>)))

(define %string=?  (%string-comparison-maker  true false false))
(define %string<?  (%string-comparison-maker false  true false))
(define %string<=? (%string-comparison-maker  true  true false))
(define %string>?  (%string-comparison-maker false false  true))
(define %string>=? (%string-comparison-maker  true false  true))

(define-integrable (%string-ci-comparison-maker if= if< if>)
  (lambda (string1 string2)
    (string-compare-ci string1 string2 if= if< if>)))

(define %string-ci=?  (%string-ci-comparison-maker  true false false))
(define %string-ci<?  (%string-ci-comparison-maker false  true false))
(define %string-ci<=? (%string-ci-comparison-maker  true  true false))
(define %string-ci>?  (%string-ci-comparison-maker false false  true))
(define %string-ci>=? (%string-ci-comparison-maker  true false  true))

(define-integrable (string-comparison-maker %compare)
  (lambda (string1 string2 . strings)
    (let loop ((string1 string1) (string2 string2) (strings strings))
      (if (pair? strings)
	  (and (%compare string1 string2)
	       (loop string2 (car strings) (cdr strings)))
	  (%compare string1 string2)))))

(define string=? (string-comparison-maker %string=?))
(define string<? (string-comparison-maker %string<?))
(define string<=? (string-comparison-maker %string<=?))
(define string>? (string-comparison-maker %string>?))
(define string>=? (string-comparison-maker %string>=?))

(define string-ci=? (string-comparison-maker %string-ci=?))
(define string-ci<? (string-comparison-maker %string-ci<?))
(define string-ci<=? (string-comparison-maker %string-ci<=?))
(define string-ci>? (string-comparison-maker %string-ci>?))
(define string-ci>=? (string-comparison-maker %string-ci>=?))

;;;; Match

(define (string-match-forward string1 string2)
  (let ((end1 (string-length string1))
	(end2 (string-length string2)))
    (let ((end (fix:min end1 end2)))
      (let loop ((i 0))
	(if (and (fix:< i end)
		 (char=? (string-ref string1 i)
			 (string-ref string2 i)))
	    (loop (fix:+ i 1))
	    i)))))

(define (string-match-forward-ci string1 string2)
  (string-match-forward (string-foldcase string1)
			(string-foldcase string2)))

(define (string-match-backward string1 string2)
  (let ((s1 (fix:- (string-length string1) 1)))
    (let loop ((i s1) (j (fix:- (string-length string2) 1)))
      (if (and (fix:>= i 0)
	       (fix:>= j 0)
	       (char=? (string-ref string1 i)
		       (string-ref string2 j)))
	  (loop (fix:- i 1)
		(fix:- j 1))
	  (fix:- s1 i)))))

(define (string-match-backward-ci string1 string2)
  (string-match-backward (string-foldcase string1)
			 (string-foldcase string2)))

(define (string-prefix? prefix string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-prefix?))
	 (start (fix:start-index start end 'string-prefix?))
	 (n (string-length prefix)))
    (and (fix:<= n (fix:- end start))
	 (let loop ((i 0) (j start))
	   (if (fix:< i n)
	       (and (eq? (string-ref prefix i) (string-ref string j))
		    (loop (fix:+ i 1) (fix:+ j 1)))
	       #t)))))

(define (string-suffix? suffix string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-suffix?))
	 (start (fix:start-index start end 'string-suffix?))
	 (n (string-length suffix)))
    (and (fix:<= n (fix:- end start))
	 (let loop ((i 0) (j (fix:- end n)))
	   (if (fix:< i n)
	       (and (eq? (string-ref suffix i) (string-ref string j))
		    (loop (fix:+ i 1) (fix:+ j 1)))
	       #t)))))

(define (string-prefix-ci? prefix string #!optional start end)
  (string-prefix? (string-foldcase prefix)
		  (string-foldcase (string-slice string start end))))

(define (string-suffix-ci? suffix string #!optional start end)
  (string-suffix? (string-foldcase suffix)
		  (string-foldcase (string-slice string start end))))

;;;; Case

(define (string-downcase string)
  (case-transform char-downcase-full string))

(define (string-foldcase string)
  (case-transform char-foldcase-full string))

(define (string-upcase string)
  (case-transform char-upcase-full string))

(define (case-transform transform string)
  (let ((builder (string-builder))
	(end (string-length string)))
    (do ((index 0 (fix:+ index 1)))
	((not (fix:< index end)))
      (for-each builder (transform (string-ref string index))))
    (builder)))

(define (string-titlecase string)
  (let ((builder (string-builder)))
    (find-word-breaks string 0
		      (lambda (end start)
			(maybe-titlecase string start end builder)
			end))
    (builder)))

(define (maybe-titlecase string start end builder)
  (let loop ((index start))
    (if (fix:< index end)
	(let ((char (string-ref string index)))
	  (if (char-cased? char)
	      (begin
		(for-each builder (char-titlecase-full char))
		(do ((index (fix:+ index 1) (fix:+ index 1)))
		    ((not (fix:< index end)))
		  (for-each builder
			    (char-downcase-full (string-ref string index)))))
	      (begin
		(builder char)
		(loop (fix:+ index 1))))))))

(define (string-lower-case? string)
  (nfd-string-lower-case? (string->nfd string)))

(define (string-upper-case? string)
  (nfd-string-upper-case? (string->nfd string)))

(define (nfd-string-lower-case? nfd)
  (let ((end (string-length nfd)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (not (char-changes-when-lower-cased? (string-ref nfd i)))
	       (loop (fix:+ i 1)))
	  #t))))

(define (nfd-string-upper-case? nfd)
  (let ((end (string-length nfd)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (not (char-changes-when-upper-cased? (string-ref nfd i)))
	       (loop (fix:+ i 1)))
	  #t))))

(define (nfd-string-case-folded? nfd)
  (let ((end (string-length nfd)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (not (char-changes-when-case-folded? (string-ref nfd i)))
	       (loop (fix:+ i 1)))
	  #t))))

(define (string-canonical-foldcase string)
  (let ((nfd (string->nfd string)))
    (if (nfd-string-case-folded? nfd)
	nfd
	(string->nfd (string-foldcase string)))))

;;;; Normalization

(define (string->nfd string)
  (if (or (string-ascii? string)	;ASCII unaffected by normalization
	  (string-in-nfd? string))
      string
      (canonical-ordering! (canonical-decomposition string))))

(define (string-ascii? string)
  (let ((n (string-length string)))
    (let loop ((i 0))
      (if (fix:< i n)
	  (and (char-ascii? (string-ref string i))
	       (loop (fix:+ i 1)))
	  #t))))

(define (string-in-nfd? string)
  (let ((n (string-length string)))
    (let loop ((i 0) (last-ccc 0))
      (if (fix:< i n)
	  (let* ((char (string-ref string i))
		 (ccc (ucd-ccc-value char)))
	    (and (or (fix:= ccc 0)
		     (fix:>= ccc last-ccc))
		 (char-nfd-quick-check? char)
		 (loop (fix:+ i 1) ccc)))
	  #t))))

(define (canonical-decomposition string)
  (let ((end (string-length string))
	(builder (string-builder)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i end)))
      (for-each builder (ucd-dm-value (string-ref string i))))
    (builder)))

(define (canonical-ordering! string)
  (let ((end (string-length string)))

    (define (scan-for-non-starter i)
      (if (fix:< i end)
	  (let* ((char (string-ref string i))
		 (ccc (ucd-ccc-value char)))
	    (if (fix:= 0 ccc)
		(scan-for-non-starter (fix:+ i 1))
		(maybe-twiddle char ccc i)))))

    (define (maybe-twiddle char1 ccc1 i1)
      (let ((i2 (fix:+ i1 1)))
	(if (fix:< i2 end)
	    (let* ((char2 (string-ref string i2))
		   (ccc2 (ucd-ccc-value char2)))
	      (cond ((fix:= 0 ccc2)
		     (scan-for-non-starter (fix:+ i2 1)))
		    ((fix:<= ccc1 ccc2)
		     (maybe-twiddle char2 ccc2 i2))
		    (else
		     (string-set! string i1 char2)
		     (string-set! string i2 char1)
		     (maybe-twiddle char1 ccc1 i2)))))))

    (scan-for-non-starter 0))
  string)

(define (quick-check string qc-value)
  (let ((n (string-length string)))
    (let loop ((i 0) (last-ccc 0) (result #t))
      (if (fix:< i n)
	  (let* ((char (string-ref string i))
		 (ccc (ucd-ccc-value char)))
	    (if (and (fix:> ccc 0)
		     (fix:< ccc last-ccc))
		#f
		(let ((check (qc-value char)))
		  (and check
		       (if (eq? check 'maybe)
			   (loop (fix:+ i 1) ccc check)
			   (loop (fix:+ i 1) ccc result))))))
	  result))))

;;;; Grapheme clusters

(define (grapheme-cluster-length string)
  (let ((breaks
	 (find-grapheme-cluster-breaks string
				       0
				       (lambda (i count)
					 (declare (ignore i))
					 (fix:+ count 1)))))
    (if (fix:> breaks 0)
	(fix:- breaks 1)
	breaks)))

(define (grapheme-cluster-slice string start end)
  ;; START and END refer to the cluster breaks, they must be <= the number of
  ;; clusters in STRING.
  (guarantee index-fixnum? start 'grapheme-cluster-slice)
  (guarantee index-fixnum? end 'grapheme-cluster-slice)
  (if (not (fix:<= start end))
      (error:bad-range-argument start 'grapheme-cluster-slice))
  (let ((start-index #f)
	(end-index #f))
    (find-grapheme-cluster-breaks string
				  0
				  (lambda (index count)
				    (if (fix:= count start)
					(set! start-index index))
				    (if (fix:= count end)
					(set! end-index index))
				    (fix:+ count 1)))
    (if (not start-index)
	(error:bad-range-argument start 'grapheme-cluster-slice))
    (if (not end-index)
	(error:bad-range-argument end 'grapheme-cluster-slice))
    (string-slice string start-index end-index)))

(define (find-grapheme-cluster-breaks string initial-ctx break)
  (let ((n (string-length string)))

    (define (get-gcb i)
      (and (fix:< i n)
	   (ucd-gcb-value (string-ref string i))))

    (define (transition gcb i ctx)
      (if gcb
	  (let ((i* (fix:+ i 1)))
	    ((vector-ref gcb-states gcb)
	     (get-gcb i*)
	     (lambda (gcb* break?)
	       (transition gcb* i* (if break? (break i* ctx) ctx)))))
	  ctx))

    (if (fix:> n 0)
	(transition (get-gcb 0) 0 (break 0 initial-ctx))
	initial-ctx)))

(define gcb-names
  '#(control
     carriage-return
     emoji-base
     emoji-base-gaz
     emoji-modifier
     extend
     glue-after-zwj
     hst=l
     linefeed
     hst=lv
     hst=lvt
     prepend
     regional-indicator
     spacing-mark
     hst=t
     hst=v
     other
     zwj))

(define (name->code namev name)
  (let ((end (vector-length namev)))
    (let loop ((code 0))
      (if (not (fix:< code end))
	  (error "Unknown name:" name))
      (if (eq? (vector-ref namev code) name)
	  code
	  (loop (fix:+ code 1))))))

(define (make-!selector namev names)
  (let loop
      ((names names)
       (mask (fix:- (fix:lsh 1 (vector-length namev)) 1)))
    (if (pair? names)
	(loop (cdr names)
	      (fix:andc mask (fix:lsh 1 (name->code namev (car names)))))
	(lambda (gcb)
	  (not (fix:= 0 (fix:and mask (fix:lsh 1 gcb))))))))

(define (make-selector namev names)
  (let loop
      ((names names)
       (mask 0))
    (if (pair? names)
	(loop (cdr names)
	      (fix:or mask (fix:lsh 1 (name->code namev (car names)))))
	(lambda (gcb)
	  (not (fix:= 0 (fix:and mask (fix:lsh 1 gcb))))))))

(define gcb-states
  (let ((simple-state
	 (lambda (break?)
	   (lambda (gcb k)
	     (k gcb (break? gcb)))))
	(gcb-code
	 (lambda (name)
	   (name->code gcb-names name)))
	(make-no-breaks
	 (lambda (names)
	   (make-!selector gcb-names names)))
	(make-breaks
	 (lambda (names)
	   (make-selector gcb-names names))))
    (let ((state:control (simple-state (make-no-breaks '())))
	  (state:emoji-base
	   (let ((gcb:extend (gcb-code 'extend))
		 (gcb:emoji-base (gcb-code 'emoji-base))
		 (break?
		  (make-no-breaks '(emoji-modifier extend spacing-mark zwj))))
	     (lambda (gcb k)
	       (if (fix:= gcb gcb:extend)
		   (k gcb:emoji-base #f)
		   (k gcb (break? gcb))))))
	  (state:extend
	   (simple-state (make-no-breaks '(extend spacing-mark zwj))))
	  (state:hst=v
	   (simple-state
	    (make-no-breaks '(hst=t hst=v extend spacing-mark zwj))))
	  (state:hst=t
	   (simple-state (make-no-breaks '(hst=t extend spacing-mark zwj)))))
      (vector state:control
	      (simple-state (make-no-breaks '(linefeed)))
	      state:emoji-base
	      state:emoji-base
	      state:extend
	      state:extend
	      state:extend
	      (simple-state
	       (make-no-breaks
		'(hst=l hst=lv hst=lvt hst=v extend spacing-mark zwj)))
	      state:control
	      state:hst=v
	      state:hst=t
	      (simple-state (make-breaks '(control carriage-return linefeed)))
	      (let ((gcb:regional-indicator (gcb-code 'regional-indicator))
		    (gcb:extend (gcb-code 'extend))
		    (break? (make-no-breaks '(extend spacing-mark zwj))))
		(lambda (gcb k)
		  (let ((gcb
			 (if (fix:= gcb gcb:regional-indicator)
			     gcb:extend
			     gcb)))
		    (k gcb (break? gcb)))))
	      state:extend
	      state:hst=t
	      state:hst=v
	      state:extend
	      (simple-state
	       (make-no-breaks
		'(emoji-base-gaz glue-after-zwj extend spacing-mark zwj)))))))

;;;; Word breaks

(define (find-word-breaks string initial-ctx break)
  (let ((n (string-length string)))

    (define (get-wb i)
      (and (fix:< i n)
	   (ucd-wb-value (string-ref string i))))

    (define (transition wb0 wb1 i0 ctx)
      (if wb0
	  (let* ((i1 (fix:+ i0 1))
		 (i2 (fix:+ i1 1))
		 (wb2 (get-wb i2)))
	    ((vector-ref wb-states wb0)
	     wb1
	     wb2
	     (lambda (break?)
	       (transition wb0
			   wb2
			   i1
			   (if break? (break i1 ctx) ctx)))
	     (lambda (wb1* break?)
	       (transition wb1*
			   wb2
			   i1
			   (if break? (break i1 ctx) ctx)))
	     (lambda (wb2* break?)
	       (transition wb2*
			   (get-wb (fix:+ i2 1))
			   i2
			   (if break? (break i2 ctx) ctx)))))
	  ctx))

    (if (fix:> n 0)
	(transition (get-wb 0)
		    (get-wb 1)
		    0
		    (break 0 initial-ctx))
	initial-ctx)))

(define wb-names
  '#(carriage-return
     double-quote
     emoji-base
     emoji-base-gaz
     emoji-modifier
     extend-num-let
     extend
     format
     glue-after-zwj
     hebrew-letter
     katakana
     letter
     linefeed
     mid-num-let
     mid-letter
     mid-number
     newline
     numeric
     regional-indicator
     single-quote
     other
     zwj))

(define wb-states
  (let ((select:extender (make-selector wb-names '(extend format zwj)))
	(select:mb/ml/sq
	 (make-selector wb-names '(mid-letter mid-num-let single-quote)))
	(select:hl/le (make-selector wb-names '(hebrew-letter letter))))

    (let ((standard-state
	   (lambda (break?)
	     (lambda (wb1 wb2 k0 k1 k2)
	       (declare (ignore wb2 k2))
	       (if (select:extender wb1)
		   (k0 #f)
		   (k1 wb1 (break? wb1)))))))

      (let ((state:always-break
	     (lambda (wb1 wb2 k0 k1 k2)
	       (declare (ignore wb2 k0 k2))
	       (k1 wb1 #t)))
	    (state:default
	     (lambda (wb1 wb2 k0 k1 k2)
	       (declare (ignore wb2 k2))
	       (if (select:extender wb1)
		   (k0 #f)
		   (k1 wb1 #t))))
	    (state:emoji-base
	     (standard-state (make-!selector wb-names '(emoji-modifier)))))

	(vector (let ((break?		;carriage-return
		       (make-!selector wb-names '(linefeed))))
		  (lambda (wb1 wb2 k0 k1 k2)
		    (declare (ignore wb2 k0 k2))
		    (k1 wb1 (break? wb1))))
		state:default		;double-quote
		state:emoji-base	;emoji-base
		state:emoji-base	;emoji-base-gaz
		state:default		;emoji-modifier
		(standard-state		;extend-num-let
		 (make-!selector wb-names
				 '(extend-num-let hebrew-letter katakana letter
						  numeric)))
		state:default		;extend
		state:default		;format
		state:default		;glue-after-zwj
		(let ((break?		;hebrew-letter
		       (make-!selector wb-names
				       '(extend-num-let hebrew-letter letter
							numeric single-quote)))
		      (select:dq (make-selector wb-names '(double-quote)))
		      (select:hl (make-selector wb-names '(hebrew-letter))))
		  (lambda (wb1 wb2 k0 k1 k2)
		    (cond ((select:extender wb1)
			   (k0 #f))
			  ((and wb2
				(select:mb/ml/sq wb1)
				(select:hl/le wb2))
			   (k2 wb2 #f))
			  ((and wb2
				(select:dq wb1)
				(select:hl wb2))
			   (k2 wb2 #f))
			  (else
			   (k1 wb1 (break? wb1))))))
		(standard-state		;katakana
		 (make-!selector wb-names '(extend-num-let katakana)))
		(let ((break?		;letter
		       (make-!selector wb-names
				       '(extend-num-let hebrew-letter letter
							numeric))))
		  (lambda (wb1 wb2 k0 k1 k2)
		    (cond ((select:extender wb1)
			   (k0 #f))
			  ((and wb2
				(select:mb/ml/sq wb1)
				(select:hl/le wb2))
			   (k2 wb2 #f))
			  (else
			   (k1 wb1 (break? wb1))))))
		state:always-break	;linefeed
		state:default		;mid-num-let
		state:default		;mid-letter
		state:default		;mid-number
		state:always-break	;newline
		(let ((break?		;numeric
		       (make-!selector wb-names
				       '(extend-num-let hebrew-letter letter
							numeric)))
		      (select:mb/mn/sq
		       (make-selector wb-names
				      '(mid-num-let mid-number single-quote)))
		      (select:numeric
		       (make-selector wb-names '(numeric))))
		  (lambda (wb1 wb2 k0 k1 k2)
		    (cond ((select:extender wb1)
			   (k0 #f))
			  ((and wb2
				(select:mb/mn/sq wb1)
				(select:numeric wb2))
			   (k2 wb2 #f))
			  (else
			   (k1 wb1 (break? wb1))))))
		;; regional-indicator
		(let ((select:regional-indicator
		       (make-selector wb-names '(regional-indicator)))
		      (wb:extend (name->code wb-names 'extend)))
		  (lambda (wb1 wb2 k0 k1 k2)
		    (declare (ignore wb2 k2))
		    (cond ((select:extender wb1)
			   (k0 #f))
			  ((select:regional-indicator wb1)
			   (k1 wb:extend #f))
			  (else
			   (k1 wb1 #t)))))
		state:default		;single-quote
		state:default		;other
		(standard-state		;zwj
		 (make-!selector wb-names '(emoji-base-gaz glue-after-zwj)))
		)))))

;;;; Search

(define-integrable (string-matcher caller matcher)
  (lambda (pattern text #!optional start end)
    (let ((pend (string-length pattern)))
      (if (fix:= 0 pend)
	  (error:bad-range-argument pend caller))
      (let* ((tend (fix:end-index end (string-length text) caller))
	     (tstart (fix:start-index start end caller)))
	(matcher pattern pend text tstart (fix:- tend pend))))))

(define string-search-forward
  (string-matcher 'string-search-forward
		  %dumb-string-search-forward))

(define string-search-backward
  (string-matcher 'string-search-backward
		  %dumb-string-search-backward))

(define string-search-all
  (string-matcher 'string-search-all
		  %dumb-string-search-all))

(define (%dumb-string-search-forward pattern pend text tstart tlast)
  (let find-match ((tindex tstart))
    (and (fix:<= tindex tlast)
	 (let match ((pi 0) (ti tindex))
	   (if (fix:< pi pend)
	       (if (char=? (string-ref pattern pi)
			   (string-ref text ti))
		   (match (fix:+ pi 1) (fix:+ ti 1))
		   (find-match (fix:+ tindex 1)))
	       tindex)))))

(define (%dumb-string-search-backward pattern pend text tstart tlast)
  (let find-match ((tindex tlast))
    (and (fix:>= tindex tstart)
	 (let match ((pi 0) (ti tindex))
	   (if (fix:< pi pend)
	       (if (char=? (string-ref pattern pi)
			   (string-ref text ti))
		   (match (fix:+ pi 1) (fix:+ ti 1))
		   (find-match (fix:- tindex 1)))
	       ti)))))

(define (%dumb-string-search-all pattern pend text tstart tlast)
  (let find-match ((tindex tlast) (matches '()))
    (if (fix:>= tindex tstart)
	(find-match (fix:- tindex 1)
		    (let match ((pi 0) (ti tindex))
		      (if (fix:< pi pend)
			  (if (char=? (string-ref pattern pi)
				      (string-ref text ti))
			      (match (fix:+ pi 1) (fix:+ ti 1))
			      matches)
			  (cons tindex matches))))
	matches)))

;;;; Sequence converters

(define (list->string chars)
  (if (every char-8-bit? chars)
      (let ((string (legacy-string-allocate (length chars))))
	(do ((chars chars (cdr chars))
	     (i 0 (fix:+ i 1)))
	    ((not (pair? chars)))
	  (legacy-string-set! string i (car chars)))
	string)
      (let ((string (full-string-allocate (length chars))))
	(do ((chars chars (cdr chars))
	     (i 0 (fix:+ i 1)))
	    ((not (pair? chars)))
	  (%full-string-set! string i (car chars)))
	string)))

(define (string->list string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string->list))
	 (start (fix:start-index start end 'string->list)))
    (receive (string start end) (translate-slice string start end)
      (if (legacy-string? string)
	  (do ((i (fix:- end 1) (fix:- i 1))
	       (chars '() (cons (legacy-string-ref string i) chars)))
	      ((not (fix:>= i start)) chars))
	  (do ((i (fix:- end 1) (fix:- i 1))
	       (chars '() (cons (%full-string-ref string i) chars)))
	      ((not (fix:>= i start)) chars))))))

(define (vector->string vector #!optional start end)
  (let* ((end (fix:end-index end (vector-length string) 'vector->string))
	 (start (fix:start-index start end 'vector->string))
	 (to
	  (if (do ((i start (fix:+ i 1))
		   (8-bit? #t (and 8-bit? (char-8-bit? (vector-ref vector i)))))
		  ((not (fix:< start end)) 8-bit?))
	      (legacy-string-allocate (fix:- end start))
	      (full-string-allocate (fix:- end start)))))
    (copy-loop string-set! to 0
	       vector-ref vector start end)
    to))

(define (string->vector string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string->vector))
	 (start (fix:start-index start end 'string->vector)))
    (receive (string start end) (translate-slice string start end)
      (if (legacy-string? string)
	  (let ((to (make-vector (fix:- end start))))
	    (copy-loop vector-set! to 0
		       legacy-string-ref string start end)
	    to)
	  (let ((to (make-vector (fix:- end start))))
	    (copy-loop vector-set! to 0
		       %full-string-ref string start end)
	    to)))))

;;;; Append and general constructor

(define (string-append . strings)
  (%string-append* strings))

(define (string-append* strings)
  (guarantee list? strings 'string-append*)
  (%string-append* strings))

(define (%string-append* strings)
  (let ((string
	 (do ((strings strings (cdr strings))
	      (n 0 (fix:+ n (string-length (car strings))))
	      (8-bit? #t (and 8-bit? (string-8-bit? (car strings)))))
	     ((not (pair? strings))
	      (if 8-bit?
		  (legacy-string-allocate n)
		  (full-string-allocate n))))))
    (let loop ((strings strings) (i 0))
      (if (pair? strings)
	  (let ((n (string-length (car strings))))
	    (string-copy! string i (car strings) 0 n)
	    (loop (cdr strings) (fix:+ i n)))))
    string))

(define (string . objects)
  (%string* objects 'string))

(define (string* objects)
  (guarantee list? objects 'string*)
  (%string* objects 'string*))

(define (%string* objects caller)
  (%string-append*
   (map (lambda (object)
	  (->string object caller))
	objects)))

(define (->string object caller)
  (cond ((not object) "")
	((bitless-char? object) (char->string object))
	((string? object) object)
	((symbol? object) (symbol->string object))
	((pathname? object) (->namestring object))
	((number? object) (number->string object))
	((uri? object) (uri->string object))
	(else (error:not-a ->string-component? object caller))))

(define (->string-component? object)
  (or (not object)
      (bitless-char? object)
      (string? object)
      (symbol? object)
      (pathname? object)
      (number? object)
      (uri? object)))

;;;; Mapping

(define (mapper-values proc string strings)
  (cond ((null? strings)
	 (values (string-length string)
		 (lambda (i)
		   (proc (string-ref string i)))))
	((null? (cdr strings))
	 (let* ((string2 (car strings))
		(n (fix:min (string-length string)
			    (string-length string2))))
	   (values n
		   (lambda (i)
		     (proc (string-ref string i)
			   (string-ref string2 i))))))
	(else
	 (let ((n (min-length string-length string strings)))
	   (values n
		   (lambda (i)
		     (apply proc
			    (string-ref string i)
			    (map (lambda (string)
				   (string-ref string i))
				 strings))))))))

(define (min-length string-length string strings)
  (do ((strings strings (cdr strings))
       (n (string-length string)
	  (fix:min n (string-length (car strings)))))
      ((null? strings) n)))

(define (string-for-each proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)))
      (proc i))))

(define (string-map proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let ((builder (string-builder)))
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i n)))
	(builder (proc i)))
      (builder))))

(define (string-count proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let loop ((i 0) (count 0))
      (if (fix:< i n)
	  (loop (fix:+ i 1)
		(if (proc i)
		    (fix:+ count 1)
		    count))
	  count))))

(define (string-any proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let loop ((i 0))
      (and (fix:< i n)
	   (if (proc i)
	       #t
	       (loop (fix:+ i 1)))))))

(define (string-every proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let loop ((i 0))
      (if (fix:< i n)
	  (and (proc i)
	       (loop (fix:+ i 1)))
	  #t))))

(define (string-find-first-index proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let loop ((i 0))
      (and (fix:< i n)
	   (if (proc i)
	       i
	       (loop (fix:+ i 1)))))))

(define (string-find-last-index proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let loop ((i (fix:- n 1)))
      (and (fix:>= i 0)
	   (if (proc i)
	       i
	       (loop (fix:- i 1)))))))

;;;; Joiner

(define (string-joiner . options)
  (let ((joiner (%string-joiner options 'string-joiner)))
    (lambda strings
      (joiner strings))))

(define (string-joiner* . options)
  (%string-joiner options 'string-joiner*))

(define (%string-joiner options caller)
  (receive (infix prefix suffix) (string-joiner-options options caller)
    (let ((infix (string-append suffix infix prefix)))
      (lambda (strings)
	(string-append*
	 (if (pair? strings)
	     (cons* prefix
		    (car strings)
		    (let loop ((strings (cdr strings)))
		      (if (pair? strings)
			  (cons* infix
				 (car strings)
				 (loop (cdr strings)))
			  (list suffix))))
	     '()))))))

(define-deferred string-joiner-options
  (keyword-option-parser
   (list (list 'infix string? "")
	 (list 'prefix string? "")
	 (list 'suffix string? ""))))

;;;; Splitter

(define (string-splitter . options)
  (receive (delimiter allow-runs? copy?)
      (string-splitter-options options 'string-splitter)
    (let ((predicate (splitter-delimiter->predicate delimiter))
	  (get-part (if copy? string-copy string-slice)))

      (lambda (string #!optional start end)
	(let* ((end (fix:end-index end (string-length string) 'string-splitter))
	       (start (fix:start-index start end 'string-splitter)))

	  (define (find-start start)
	    (if allow-runs?
		(let loop ((index start))
		  (if (fix:< index end)
		      (if (predicate (string-ref string index))
			  (loop (fix:+ index 1))
			  (find-end index (fix:+ index 1)))
		      '()))
		(find-end start start)))

	  (define (find-end start index)
	    (let loop ((index index))
	      (if (fix:< index end)
		  (if (predicate (string-ref string index))
		      (cons (get-part string start index)
			    (find-start (fix:+ index 1)))
		      (loop (fix:+ index 1)))
		  (list (get-part string start end)))))

	  (find-start start))))))

(define-deferred string-splitter-options
  (keyword-option-parser
   (list (list 'delimiter splitter-delimiter? char-whitespace?)
	 (list 'allow-runs? boolean? #t)
	 (list 'copy? boolean? #f))))

(define (splitter-delimiter->predicate delimiter)
  (cond ((char? delimiter) (char=-predicate delimiter))
	((char-set? delimiter) (char-set-predicate delimiter))
	((unary-procedure? delimiter) delimiter)
	(else (error:not-a splitter-delimiter? delimiter 'string-splitter))))

(define (splitter-delimiter? object)
  (or (char? object)
      (char-set? object)
      (unary-procedure? object)))

;;;; Trimmer/Padder

(define (string-trimmer . options)
  (receive (where copy? trim-char?)
      (string-trimmer-options options 'string-trimmer)
    (let ((get-trimmed (if copy? string-copy string-slice)))
      (lambda (string)
	(let ((end (string-length string)))
	  (get-trimmed
	   string
	   (if (eq? where 'trailing)
	       0
	       (let loop ((index 0))
		 (if (and (fix:< index end)
			  (trim-char? (string-ref string index)))
		     (loop (fix:+ index 1))
		     index)))
	   (if (eq? where 'leading)
	       end
	       (let loop ((index end))
		 (if (and (fix:> index 0)
			  (trim-char? (string-ref string (fix:- index 1))))
		     (loop (fix:- index 1))
		     index)))))))))

(define-deferred string-trimmer-options
  (keyword-option-parser
   (list (list 'where '(leading trailing both) 'both)
	 (list 'copy? boolean? #t)
	 (list 'trim-char? unary-procedure? char-whitespace?))))

(define (string-padder . options)
  (receive (where fill-with clip?)
      (string-padder-options options 'string-padder)
    (lambda (string n)
      (guarantee index-fixnum? n 'string-padder)
      (let ((cluster-length (grapheme-cluster-length string)))
	(cond ((fix:= n cluster-length)
	       string)
	      ((fix:< n cluster-length)
	       (if clip?
		   (if (eq? where 'leading)
		       (grapheme-cluster-slice string
					       (fix:- cluster-length n)
					       cluster-length)
		       (grapheme-cluster-slice string 0 n))
		   string))
	      (else
	       (let ((builder (string-builder)))
		 (if (eq? where 'trailing)
		     (builder string))
		 (do ((i 0 (fix:+ i 1)))
		     ((not (fix:< i n)))
		   (builder fill-with))
		 (if (eq? where 'leading)
		     (builder string))
		 (builder))))))))

(define (grapheme-cluster-string? object)
  (and (string? object)
       (fix:= 1 (grapheme-cluster-length object))))

(define-deferred string-padder-options
  (keyword-option-parser
   (list (list 'where '(leading trailing) 'leading)
	 (list 'fill-with grapheme-cluster-string? " ")
	 (list 'clip? boolean? #t))))

;;;; Miscellaneous

(define (string-fill! string char #!optional start end)
  (guarantee bitless-char? char 'string-fill!)
  (let* ((end (fix:end-index end (string-length string) 'string-fill!))
	 (start (fix:start-index start end 'string-fill!)))
    (receive (string start end) (translate-slice string start end)
      (if (legacy-string? string)
	  (do ((index start (fix:+ index 1)))
	      ((not (fix:< index end)) unspecific)
	    (legacy-string-set! string index char))
	  (let ((bytes (%full-string-cp-vector string))
		(cp (char->integer char)))
	    (do ((i start (fix:+ i 1)))
		((not (fix:< i end)))
	      (cp-vector-set! bytes i cp)))))))

(define (string-replace string char1 char2)
  (guarantee bitless-char? char1 'string-replace)
  (guarantee bitless-char? char2 'string-replace)
  (string-map (lambda (char)
		(if (char=? char char1) char2 char))
	      string))

(define (string-hash string #!optional modulus)
  (let ((string* (string-for-primitive string)))
    (if (default-object? modulus)
	((ucode-primitive string-hash) string*)
	((ucode-primitive string-hash-mod) string* modulus))))

(define (string-ci-hash string #!optional modulus)
  (string-hash (string-foldcase string) modulus))

(define (8-bit-string? object)
  (and (string? object)
       (string-8-bit? object)))

(define (string-8-bit? string)
  (receive (string start end) (translate-slice string 0 (string-length string))
    (if (legacy-string? string)
	#t
	(%full-string-8-bit? string start end))))

(define-integrable (%full-string-8-bit? string start end)
  (every-loop char-8-bit? %full-string-ref string start end))

(define (string-for-primitive string)
  (cond ((legacy-string? string)
	 (let ((end (legacy-string-length string)))
	   (if (every-loop char-ascii? legacy-string-ref string 0 end)
	       string
	       (string->utf8 string))))
	((full-string? string)
	 (let ((end (full-string-length string)))
	   (if (every-loop char-ascii? %full-string-ref string 0 end)
	       (let ((to (legacy-string-allocate end)))
		 (copy-loop legacy-string-set! to 0
			    %full-string-ref string 0 end)
		 to)
	       (string->utf8 string))))
	(else
	 (error:not-a string? string 'string-for-primitive))))

(define-integrable (copy-loop to-set! to at from-ref from start end)
  (do ((i start (fix:+ i 1))
       (j at (fix:+ j 1)))
      ((not (fix:< i end)))
    (to-set! to j (from-ref from i))))

(define-integrable (every-loop proc ref string start end)
  (let loop ((i start))
    (if (fix:< i end)
	(and (proc (ref string i))
	     (loop (fix:+ i 1)))
	#t)))

;;;;Backwards compatibility

(define (string-find-next-char string char)
  (string-find-first-index (char=-predicate char) string))

(define (string-find-next-char-ci string char)
  (string-find-first-index (char-ci=-predicate char) string))

(define (string-find-next-char-in-set string char-set)
  (string-find-first-index (char-set-predicate char-set) string))

(define (string-find-previous-char string char)
  (string-find-last-index (char=-predicate char) string))

(define (string-find-previous-char-ci string char)
  (string-find-last-index (char-ci=-predicate char) string))

(define (string-find-previous-char-in-set string char-set)
  (string-find-last-index (char-set-predicate char-set) string))

(define-integrable (substring-find-maker string-find)
  (lambda (string start end key)
    (let* ((slice (string-slice string start end))
	   (index (string-find slice key)))
      (and index
	   (fix:+ start index)))))

(define substring-find-next-char
  (substring-find-maker string-find-next-char))

(define substring-find-next-char-ci
  (substring-find-maker string-find-next-char-ci))

(define substring-find-next-char-in-set
  (substring-find-maker string-find-next-char-in-set))

(define substring-find-previous-char
  (substring-find-maker string-find-previous-char))

(define substring-find-previous-char-ci
  (substring-find-maker string-find-previous-char-ci))

(define substring-find-previous-char-in-set
  (substring-find-maker string-find-previous-char-in-set))

(define (substring? pattern text)
  (and (or (fix:= 0 (string-length pattern))
	   (string-search-forward pattern text))
       #t))

(define (string-move! string1 string2 start2)
  (string-copy! string2 start2 string1))

(define (substring-move! string1 start1 end1 string2 start2)
  (string-copy! string2 start2 string1 start1 end1))

(define (substring-ci<? string1 start1 end1 string2 start2 end2)
  (string-ci<? (string-slice string1 start1 end1)
	       (string-slice string2 start2 end2)))

(define (substring-ci=? string1 start1 end1 string2 start2 end2)
  (string-ci=? (string-slice string1 start1 end1)
	       (string-slice string2 start2 end2)))

(define (substring<? string1 start1 end1 string2 start2 end2)
  (string<? (string-slice string1 start1 end1)
	    (string-slice string2 start2 end2)))

(define (substring=? string1 start1 end1 string2 start2 end2)
  (string=? (string-slice string1 start1 end1)
	    (string-slice string2 start2 end2)))

(define (substring-prefix? string1 start1 end1 string2 start2 end2)
  (string-prefix? (string-slice string1 start1 end1)
		  (string-slice string2 start2 end2)))

(define (substring-prefix-ci? string1 start1 end1 string2 start2 end2)
  (string-prefix-ci? (string-slice string1 start1 end1)
		     (string-slice string2 start2 end2)))

(define (substring-suffix? string1 start1 end1 string2 start2 end2)
  (string-suffix? (string-slice string1 start1 end1)
		  (string-slice string2 start2 end2)))

(define (substring-suffix-ci? string1 start1 end1 string2 start2 end2)
  (string-suffix-ci? (string-slice string1 start1 end1)
		     (string-slice string2 start2 end2)))

(define (substring-fill! string start end char)
  (string-fill! string char start end))

(define (substring-lower-case? string start end)
  (string-lower-case? (string-slice string start end)))

(define (substring-upper-case? string start end)
  (string-upper-case? (string-slice string start end)))

(define (string-null? string)
  (fix:= 0 (string-length string)))

(define (char->string char)
  (guarantee bitless-char? char 'char->string)
  (let ((s
	 (if (char-8-bit? char)
	     (legacy-string-allocate 1)
	     (full-string-allocate 1))))
    (string-set! s 0 char)
    s))

(define (legacy-string-trimmer where)
  (lambda (string #!optional char-set)
    ((string-trimmer 'where where
		     'trim-char?
		     (char-set-predicate
		      (if (default-object? char-set)
			  char-set:whitespace
			  (char-set-invert char-set))))
     string)))

(define string-trim-left (legacy-string-trimmer 'leading))
(define string-trim-right (legacy-string-trimmer 'trailing))
(define string-trim (legacy-string-trimmer 'both))

(define (legacy-string-padder where)
  (lambda (string n #!optional char)
    ((string-padder 'where where
		    'fill-with (if (default-object? char)
				   char
				   (char->string char)))
     string n)))

(define string-pad-left (legacy-string-padder 'leading))
(define string-pad-right (legacy-string-padder 'trailing))

(define (decorated-string-append prefix infix suffix strings)
  ((string-joiner* 'prefix prefix
		   'infix infix
		   'suffix suffix)
   strings))

(define (burst-string string delimiter allow-runs?)
  ((string-splitter 'delimiter delimiter
		    'allow-runs? allow-runs?
		    'copy? #t)
   string))