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
  (register-predicate! ->string-component? '->string-component))

;;;; Strings

(define (string? object)
  (or (legacy-string? object)
      (full-string? object)
      (slice? object)))

(define (make-ustring k #!optional char)
  (guarantee index-fixnum? k 'make-ustring)
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

(define (string-builder)
  ;; This is optimized to minimize copying, so it wastes some space.
  (let ((buffer-size 16))
    (let ((buffers '())
	  (buffer (full-string-allocate buffer-size))
	  (index 0))

      (define (new-buffer!)
	(set! buffers (cons (string-slice buffer 0 index) buffers))
	(set! buffer (full-string-allocate buffer-size))
	(set! index 0)
	unspecific)

      (define (empty?)
	(and (fix:= 0 index)
	     (null? buffers)))

      (define (append-char! char)
	(if (not (fix:< index buffer-size))
	    (new-buffer!))
	(string-set! buffer index char)
	(set! index (fix:+ index 1))
	unspecific)

      (define (append-string! string)
	(if (fix:> index 0)
	    (new-buffer!))
	(set! buffers (cons string buffers))
	unspecific)

      (define (build)
	(let ((strings (reverse! (cons (string-slice buffer 0 index) buffers))))
	  (set! buffer)
	  (set! buffers)
	  (set! index)
	  (let ((result
		 (do ((strings strings (cdr strings))
		      (n 0 (fix:+ n (string-length (car strings))))
		      (8-bit? #t (and 8-bit? (string-8-bit? (car strings)))))
		     ((not (pair? strings))
		      (if 8-bit?
			  (legacy-string-allocate n)
			  (full-string-allocate n))))))
	    (do ((strings strings (cdr strings))
		 (i 0 (string-copy! result i (car strings))))
		((not (pair? strings))))
	    result)))

      (lambda (#!optional object)
	(cond ((default-object? object) (build))
	      ((bitless-char? object) (append-char! object))
	      ((string? object) (append-string! object))
	      ((eq? 'empty? object) (empty?))
	      (else (error "Not a char or string:" object)))))))

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

;; Non-Unicode implementation, acceptable to R7RS.
(define-integrable (%string-comparison-maker c= c< f<)
  (lambda (string1 string2)
    (let ((end1 (string-length string1))
	  (end2 (string-length string2)))
      (let ((end (fix:min end1 end2)))
	(let loop ((i 0))
	  (if (fix:< i end)
	      (let ((c1 (string-ref string1 i))
		    (c2 (string-ref string2 i)))
		(if (c= c1 c2)
		    (loop (fix:+ i 1))
		    (c< c1 c2)))
	      (f< end1 end2)))))))

(define %string<? (%string-comparison-maker char=? char<? fix:<))
(define %string<=? (%string-comparison-maker char=? char<=? fix:<=))
(define %string=? (%string-comparison-maker char=? char=? fix:=))
(define %string>? (%string-comparison-maker char=? char>? fix:>))
(define %string>=? (%string-comparison-maker char=? char>=? fix:<=))

(define-integrable (%string-ci-comparison-maker string-compare)
  (lambda (string1 string2)
    (string-compare (string-foldcase string1)
		    (string-foldcase string2))))

(define %string-ci<? (%string-ci-comparison-maker %string<?))
(define %string-ci<=? (%string-ci-comparison-maker %string<=?))
(define %string-ci=? (%string-ci-comparison-maker %string=?))
(define %string-ci>? (%string-ci-comparison-maker %string>?))
(define %string-ci>=? (%string-ci-comparison-maker %string>=?))

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

(define-integrable (prefix-maker c= caller)
  (lambda (prefix string #!optional start end)
    (let* ((end (fix:end-index end (string-length string) caller))
	   (start (fix:start-index start end caller))
	   (n (string-length prefix)))
      (and (fix:<= n (fix:- end start))
	   (let loop ((i 0) (j start))
	     (if (fix:< i n)
		 (and (c= (string-ref prefix i) (string-ref string j))
		      (loop (fix:+ i 1) (fix:+ j 1)))
		 #t))))))

(define-integrable (suffix-maker c= caller)
  (lambda (suffix string #!optional start end)
    (let* ((end (fix:end-index end (string-length string) caller))
	   (start (fix:start-index start end caller))
	   (n (string-length suffix)))
      (and (fix:<= n (fix:- end start))
	   (let loop ((i 0) (j (fix:- end n)))
	     (if (fix:< i n)
		 (and (c= (string-ref suffix i) (string-ref string j))
		      (loop (fix:+ i 1) (fix:+ j 1)))
		 #t))))))

(define string-prefix? (prefix-maker eq? 'string-prefix?))
(define string-suffix? (suffix-maker eq? 'string-suffix?))

;;; Incorrect implementation: should do string-foldcase on both args.
(define string-prefix-ci? (prefix-maker char-ci=? 'string-prefix-ci?))
(define string-suffix-ci? (suffix-maker char-ci=? 'string-suffix-ci?))

(define (string-downcase string)
  (case-transform char-downcase-full string))

(define (string-foldcase string)
  (case-transform char-foldcase-full string))

(define (string-upcase string)
  (case-transform char-upcase-full string))

(define (case-transform transform string)
  (let ((chars (append-map transform (string->list string))))
    (let ((n (length chars)))
      (let ((result
	     (if (every char-8-bit? chars)
		 (legacy-string-allocate n)
		 (full-string-allocate n))))
	(do ((chars chars (cdr chars))
	     (i 0 (fix:+ i 1)))
	    ((not (pair? chars)))
	  (string-set! result i (car chars)))
	result))))

(define (string-lower-case? string)
  (let* ((nfd (string->nfd string))
	 (end (string-length nfd)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (not (char-changes-when-lower-cased? (string-ref nfd i)))
	       (loop (fix:+ i 1)))
	  #t))))

(define (string-upper-case? string)
  (let* ((nfd (string->nfd string))
	 (end (string-length nfd)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (not (char-changes-when-upper-cased? (string-ref nfd i)))
	       (loop (fix:+ i 1)))
	  #t))))

(define (string->nfd string)
  (if (string-in-nfd? string)
      string
      (canonical-ordering! (canonical-decomposition string))))

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
  (let ((end (string-length string)))
    (let ((result
	   (make-ustring
	    (do ((i 0 (fix:+ i 1))
		 (j 0 (fix:+ j (length (ucd-dm-value (string-ref string i))))))
		((not (fix:< i end)) j)))))
      (let loop ((i 0) (j 0))
	(if (fix:< i end)
	    (loop (fix:+ i 1)
		  (do ((chars (ucd-dm-value (string-ref string i))
			      (cdr chars))
		       (j j (fix:+ j 1)))
		      ((not (pair? chars)) j)
		    (string-set! result j (car chars))))))
      result)))

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

#|
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
|#

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
    (let ((result (full-string-allocate n)))
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i n)))
	(%full-string-set! result i (proc i)))
      result)))

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

(define (string-hash string #!optional modulus)
  (let ((string* (string-for-primitive string)))
    (if (default-object? modulus)
	((ucode-primitive string-hash) string*)
	((ucode-primitive string-hash-mod) string* modulus))))

(define (string-ci-hash string #!optional modulus)
  (string-hash (string-foldcase string) modulus))

(define (string-joiner infix #!optional prefix suffix)
  (let ((joiner (string-joiner* prefix infix suffix)))
    (lambda strings
      (joiner strings))))

(define (string-joiner* infix #!optional prefix suffix)
  (let ((prefix (if (default-object? prefix) "" prefix))
	(suffix (if (default-object? suffix) "" suffix)))
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

(define (string-splitter delimiter #!optional allow-runs?)
  (let ((predicate (splitter-delimiter->predicate delimiter))
	(allow-runs? (if (default-object? allow-runs?) #t allow-runs?)))

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
		    (cons (string-copy string start index)
			  (find-start (fix:+ index 1)))
		    (loop (fix:+ index 1)))
		(list (string-copy string start end)))))

	(find-start start)))))

(define (splitter-delimiter->predicate delimiter)
  (cond ((char? delimiter) (char=-predicate delimiter))
	((char-set? delimiter) (char-set-predicate delimiter))
	((unary-procedure? delimiter) delimiter)
	(else (error:not-a splitter-delimiter? delimiter 'string-splitter))))

(define (splitter-delimiter? object)
  (or (char? object)
      (char-set? object)
      (unary-procedure? object)))

(define (decorated-string-append prefix infix suffix strings)
  ((string-joiner* infix prefix suffix) strings))

(define (burst-string string delimiter allow-runs?)
  ((string-splitter delimiter allow-runs?) string))

(define (ustring->legacy-string string)
  (if (legacy-string? string)
      string
      (and (string-8-bit? string)
	   (string-copy string))))

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

(define (legacy-string-downcase string)
  (let ((end (legacy-string-length string)))
    (let ((string* (legacy-string-allocate end)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i end))
	(legacy-string-set! string* i
			    (char-downcase (legacy-string-ref string i))))
      string*)))

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