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

;;;; R7RS bytevectors
;;; package: (runtime bytevector)

(declare (usual-integrations))

(add-boot-deps! '(runtime character) '(runtime miscellaneous-global))

(define (u8? object)
  (and (index-fixnum? object)
       (fix:< object #x100)))
(register-predicate! u8? 'u8 '<= index-fixnum?)

(define (make-bytevector k #!optional byte)
  (let ((bytevector (allocate-bytevector k)))
    (if (not (default-object? byte))
	(bytevector-fill! bytevector byte 0 k))
    bytevector))

(define (bytevector . bytes)
  (list->bytevector bytes))

(define (legacy-string->bytevector string)
  (if (bytevector? string)
      string
      (begin
	(guarantee legacy-string? string 'legacy-string->bytevector)
	(object-new-type (ucode-type bytevector) string))))

(define (bytevector-append . bytevectors)
  (let* ((k
	  (do ((bytevectors bytevectors (cdr bytevectors))
	       (k 0 (fix:+ k (bytevector-length (car bytevectors)))))
	      ((not (pair? bytevectors)) k)))
	 (bytevector (allocate-bytevector k)))
    (do ((bytevectors bytevectors (cdr bytevectors))
	 (index 0 (fix:+ index (bytevector-length (car bytevectors)))))
	((not (pair? bytevectors)))
      (bytevector-copy! bytevector index (car bytevectors)))
    bytevector))

(define (bytevector-fill! bytevector fill #!optional start end)
  ((ucode-primitive bytevector-fill! 4)
   bytevector
   fill
   (if (default-object? start) 0 start)
   (if (default-object? end) (bytevector-length bytevector) end)))

(define (bytevector-zero-explicit! bytevector #!optional start end)
  ;; Don't let any compiler optimize this away.
  ((identity-procedure bytevector-fill!) bytevector 0 start end))

(define (bytevector-copy bytevector #!optional start end)
  ((ucode-primitive bytevector-copy 3)
   bytevector
   (if (default-object? start) 0 start)
   (if (default-object? end) (bytevector-length bytevector) end)))

(define (bytevector-copy! to at from #!optional start end)
  ((ucode-primitive bytevector-copy! 5)
   to
   at
   from
   (if (default-object? start) 0 start)
   (if (default-object? end) (bytevector-length from) end)))

(define (bytevector=? b1 b2)
  (let ((length (bytevector-length b1)))
    (and (fix:= length (bytevector-length b2))
	 (let loop ((index 0))
	   (or (not (fix:< index length))
	       (and (fix:= (bytevector-u8-ref b1 index)
			   (bytevector-u8-ref b2 index))
		    (loop (fix:+ index 1))))))))

;; String hash primitives work on bytevectors too.
(define (bytevector-hash bytevector #!optional modulus)
  (if (default-object? modulus)
      ((ucode-primitive string-hash) bytevector)
      ((ucode-primitive string-hash-mod) bytevector modulus)))

(define (bytevector-builder #!optional buffer-length)
  (make-sequence-builder u8? bytevector? allocate-bytevector bytevector-length
			 bytevector-u8-set! bytevector-copy!
    (if (default-object? buffer-length)
	16
	(begin
	  (guarantee positive-fixnum? buffer-length 'bytevector-builder)
	  buffer-length))))

(define (bytevector-any predicate bv)
  (let ((end (bytevector-length bv)))
    (let loop ((i 0))
      (and (fix:< i end)
	   (or (predicate (bytevector-u8-ref bv i))
	       (loop (fix:+ i 1)))))))

(define (bytevector-every predicate bv)
  (let ((end (bytevector-length bv)))
    (let loop ((i 0) (result #t))
      (if (fix:< i end)
	  (let ((v (predicate (bytevector-u8-ref bv i))))
	    (and v
		 (loop (fix:+ i 1) v)))
	  result))))

(define (list->bytevector bytes)
  (let ((bytevector (allocate-bytevector (length bytes))))
    (do ((bytes bytes (cdr bytes))
	 (i 0 (fix:+ i 1)))
	((not (pair? bytes)))
      (bytevector-u8-set! bytevector i (car bytes)))
    bytevector))

(define (bytevector->list bv #!optional start end)
  (let* ((end (fix:end-index end (bytevector-length bv) 'bytevector->list))
	 (start (fix:start-index start end 'bytevector->list)))
    (do ((i (fix:- end 1) (fix:- i 1))
	 (bytes '() (cons (bytevector-u8-ref bv i) bytes)))
	((not (fix:>= i start))
	 bytes))))

(define (vector->bytevector v #!optional start end)
  (let* ((end (fix:end-index end (vector-length v) 'vector->bytevector))
	 (start (fix:start-index start end 'vector->bytevector))
	 (bv (allocate-bytevector (fix:- end start))))
    (do ((i start (fix:+ i 1))
	 (j 0 (fix:+ j 1)))
	((not (fix:< i end)))
      (bytevector-u8-set! bv j (vector-ref v i)))
    bv))

(define (bytevector->vector bv #!optional start end)
  (let* ((end (fix:end-index end (bytevector-length bv) 'bytevector->vector))
	 (start (fix:start-index start end 'bytevector->vector))
	 (v (make-vector (fix:- end start))))
    (do ((i start (fix:+ i 1))
	 (j 0 (fix:+ j 1)))
	((not (fix:< i end)))
      (vector-set! v j (bytevector-u8-ref bv i)))
    v))

;;;; U16 accessors

(define-integrable (bytes->u16be b0 b1) (fix:or (fix:lsh b0 8) b1))
(define-integrable (u16be-byte0 u16) (fix:lsh u16 -8))
(define-integrable (u16be-byte1 u16) (fix:and u16 #xFF))

(define-integrable (bytes->u16le b0 b1) (fix:or b0 (fix:lsh b1 8)))
(define-integrable (u16le-byte0 u16) (fix:and u16 #xFF))
(define-integrable (u16le-byte1 u16) (fix:lsh u16 -8))

(define (u16? object)
  (and (index-fixnum? object)
       (fix:< object #x10000)))
(register-predicate! u16? 'u16 '<= index-fixnum?)

(define (bytevector-u16be-ref bytevector index)
  (if (not (fix:< (fix:+ index 1) (bytevector-length bytevector)))
      (error:bad-range-argument index 'bytevector-u16be-ref))
  (bytes->u16be (bytevector-u8-ref bytevector index)
		(bytevector-u8-ref bytevector (fix:+ index 1))))

(define (bytevector-u16be-set! bytevector index u16)
  (if (not (fix:< (fix:+ index 1) (bytevector-length bytevector)))
      (error:bad-range-argument index 'bytevector-u16be-ref))
  (guarantee u16? u16 'bytevector-u16be-set!)
  (bytevector-u8-set! bytevector index (u16be-byte0 u16))
  (bytevector-u8-set! bytevector (fix:+ index 1) (u16be-byte1 u16)))

(define (bytevector-u16le-ref bytevector index)
  (if (not (fix:< (fix:+ index 1) (bytevector-length bytevector)))
      (error:bad-range-argument index 'bytevector-u16le-ref))
  (bytes->u16le (bytevector-u8-ref bytevector index)
		(bytevector-u8-ref bytevector (fix:+ index 1))))

(define (bytevector-u16le-set! bytevector index u16)
  (if (not (fix:< (fix:+ index 1) (bytevector-length bytevector)))
      (error:bad-range-argument index 'bytevector-u16le-ref))
  (guarantee u16? u16 'bytevector-u16le-set!)
  (bytevector-u8-set! bytevector index (u16le-byte0 u16))
  (bytevector-u8-set! bytevector (fix:+ index 1) (u16le-byte1 u16)))

;;;; U32 accessors

(define-syntax select-u32-code
  (er-macro-transformer
   (lambda (form rename compare)
     rename compare
     (syntax-check '(_ expression expression) form)
     ;; XXX This queries the host system rather than the target system.
     ;; We should expose definitions of fix:fixnum? to macro expanders
     ;; that reflect the target system instead.
     #;
     (if (fix:fixnum? #xFFFFFFFF)
	 (cadr form)
	 (caddr form))
     ;; XXX For now, we can use the number of bytes per word as a proxy
     ;; for whether fixnums have 32 bits.
     `(select-on-bytes-per-word ,(caddr form) ,(cadr form)))))

(select-u32-code
 ;; Can use fixnums:
 (begin
   (define-integrable (bytes->u32be b0 b1 b2 b3)
     (fix:or (fix:or (fix:lsh b0 24)
		     (fix:lsh b1 16))
	     (fix:or (fix:lsh b2 8)
		     b3)))

   (define-integrable (u32be-byte0 u32) (fix:lsh u32 -24))
   (define-integrable (u32be-byte1 u32) (fix:and (fix:lsh u32 -16) #xFF))
   (define-integrable (u32be-byte2 u32) (fix:and (fix:lsh u32 -8) #xFF))
   (define-integrable (u32be-byte3 u32) (fix:and u32 #xFF))

   (define (u32? object)
     (and (index-fixnum? object)
	  (fix:<= object #xFFFFFFFF)))
   (register-predicate! u32? 'u32 '<= index-fixnum?))
 ;; Must use bignums:
 (begin
   (define-integrable (bytes->u32be b0 b1 b2 b3)
     (int:+ (int:+ (int:* b0 #x1000000)
		   (int:* b1 #x10000))
	    (int:+ (int:* b2 #x100)
		   b3)))

   (define-integrable (u32be-byte0 u32)
     (int:quotient u32 #x1000000))

   (define-integrable (u32be-byte1 u32)
     (int:remainder (int:quotient u32 #x10000) #x100))

   (define-integrable (u32be-byte2 u32)
     (int:remainder (int:quotient u32 #x100) #x100))

   (define-integrable (u32be-byte3 u32)
     (int:remainder u32 #x100))

   (define (u32? object)
     (and (exact-nonnegative-integer? object)
	  (int:<= object #xFFFFFFFF)))

   (register-predicate! u32? 'u32 '<= exact-nonnegative-integer?)))

(define-integrable (bytes->u32le b0 b1 b2 b3) (bytes->u32be b3 b2 b1 b0))
(define-integrable u32le-byte0 u32be-byte3)
(define-integrable u32le-byte1 u32be-byte2)
(define-integrable u32le-byte2 u32be-byte1)
(define-integrable u32le-byte3 u32be-byte0)

(define (bytevector-u32be-ref bytevector index)
  (if (not (fix:< (fix:+ index 3) (bytevector-length bytevector)))
      (error:bad-range-argument index 'bytevector-u32be-ref))
  (bytes->u32be (bytevector-u8-ref bytevector index)
		(bytevector-u8-ref bytevector (fix:+ index 1))
		(bytevector-u8-ref bytevector (fix:+ index 2))
		(bytevector-u8-ref bytevector (fix:+ index 3))))

(define (bytevector-u32be-set! bytevector index u32)
  (if (not (fix:< (fix:+ index 3) (bytevector-length bytevector)))
      (error:bad-range-argument index 'bytevector-u32be-ref))
  (guarantee u32? u32 'bytevector-u32be-set!)
  (bytevector-u8-set! bytevector index (u32be-byte0 u32))
  (bytevector-u8-set! bytevector (fix:+ index 1) (u32be-byte1 u32))
  (bytevector-u8-set! bytevector (fix:+ index 2) (u32be-byte2 u32))
  (bytevector-u8-set! bytevector (fix:+ index 3) (u32be-byte3 u32)))

(define (bytevector-u32le-ref bytevector index)
  (if (not (fix:< (fix:+ index 3) (bytevector-length bytevector)))
      (error:bad-range-argument index 'bytevector-u32le-ref))
  (bytes->u32le (bytevector-u8-ref bytevector index)
		(bytevector-u8-ref bytevector (fix:+ index 1))
		(bytevector-u8-ref bytevector (fix:+ index 2))
		(bytevector-u8-ref bytevector (fix:+ index 3))))

(define (bytevector-u32le-set! bytevector index u32)
  (if (not (fix:< (fix:+ index 3) (bytevector-length bytevector)))
      (error:bad-range-argument index 'bytevector-u32le-ref))
  (guarantee u32? u32 'bytevector-u32le-set!)
  (bytevector-u8-set! bytevector index (u32le-byte0 u32))
  (bytevector-u8-set! bytevector (fix:+ index 1) (u32le-byte1 u32))
  (bytevector-u8-set! bytevector (fix:+ index 2) (u32le-byte2 u32))
  (bytevector-u8-set! bytevector (fix:+ index 3) (u32le-byte3 u32)))

(define-integrable (string-encoder char-byte-length allocator encode-char!
				   bom? caller)
  (lambda (string #!optional start end)
    (let* ((end (fix:end-index end (string-length string) caller))
	   (start (fix:start-index start end caller)))
      (let ((bytes
	     (allocator
	      (let loop ((index start)
			 (n-bytes (if bom? (char-byte-length #\bom) 0)))
		(if (fix:< index end)
		    (loop (fix:+ index 1)
			  (fix:+ n-bytes
				 (char-byte-length (string-ref string index))))
		    n-bytes)))))
	(let loop ((from start)
		   (to (if bom? (encode-char! bytes 0 #\bom) 0)))
	  (if (fix:< from end)
	      (loop (fix:+ from 1)
		    (encode-char! bytes to (string-ref string from)))))
	bytes))))

;; Make sure UTF-8 bytevectors have null termination.
(define (utf8-allocator k)
  (legacy-string->bytevector (legacy-string-allocate k)))

(define string->utf8)
(define string->utf16be)
(define string->utf16le)
(define string->utf16be+bom)
(define string->utf16le+bom)
(define string->utf16)
(define string->utf32be)
(define string->utf32le)
(define string->utf32be+bom)
(define string->utf32le+bom)
(define string->utf32)
(add-boot-init!
 (lambda ()
   (set! string->utf8
	 (string-encoder char-utf8-byte-length utf8-allocator
			 encode-utf8-char! #f 'string->utf8))
   (set! string->utf16be
	 (string-encoder char-utf16-byte-length allocate-bytevector
			 encode-utf16be-char! #f 'string->utf16be))
   (set! string->utf16le
	 (string-encoder char-utf16-byte-length allocate-bytevector
			 encode-utf16le-char! #f 'string->utf16le))
   (set! string->utf16be+bom
	 (string-encoder char-utf16-byte-length allocate-bytevector
			 encode-utf16be-char! #t 'string->utf16))
   (set! string->utf16le+bom
	 (string-encoder char-utf16-byte-length allocate-bytevector
			 encode-utf16le-char! #t 'string->utf16))
   (set! string->utf16
	 (if (host-big-endian?) string->utf16be+bom string->utf16le+bom))
   (set! string->utf32be
	 (string-encoder char-utf32-byte-length allocate-bytevector
			 encode-utf32be-char! #f 'string->utf32be))
   (set! string->utf32le
	 (string-encoder char-utf32-byte-length allocate-bytevector
			 encode-utf32le-char! #f 'string->utf32le))
   (set! string->utf32be+bom
	 (string-encoder char-utf32-byte-length allocate-bytevector
			 encode-utf32be-char! #t 'string->utf32))
   (set! string->utf32le+bom
	 (string-encoder char-utf32-byte-length allocate-bytevector
			 encode-utf32le-char! #t 'string->utf32))
   (set! string->utf32
	 (if (host-big-endian?) string->utf32be+bom string->utf32le+bom))
   unspecific))

(define-integrable (bytes-decoder getter initial->length decode-char step noun
				  caller)
  (lambda (bytevector #!optional start end replace?)
    (let* ((end (fix:end-index end (bytevector-length bytevector) caller))
	   (start (fix:start-index start end caller))
	   (builder (string-builder)))
      (let ((truncated
	     (if (or (default-object? replace?) (not replace?))
		 (lambda (index)
		   (error (string "Truncated " noun " sequence:")
			  (bytevector-copy bytevector
					   index
					   (fix:min (fix:+ index 4) end))))
		 (lambda (index) index char:replacement)))
	    (ill-formed
	     (if (or (default-object? replace?) (not replace?))
		 (lambda (index)
		   (error (string "Ill-formed " noun " sequence:")
			  (bytevector-copy bytevector
					   index
					   (fix:min (fix:+ index 4) end))))
		 (lambda (index) index char:replacement))))
	(let loop ((index start))
	  (if (fix:<= (fix:+ index step) end)
	      (let ((n (initial->length (getter bytevector index))))
		(let ((index* (fix:+ index n)))
		  (builder
		   (if (not (fix:<= index* end))
		       (truncated index)
		       (or (decode-char bytevector index)
			   (ill-formed index))))
		  (loop index*)))
	      (if (fix:< index end)
		  (builder (truncated index))))))
      (builder))))

(define utf8->string)
(define utf16be->string)
(define utf16le->string)
(define utf32be->string)
(define utf32le->string)
(add-boot-init!
 (lambda ()
   (set! utf8->string
	 (bytes-decoder bytevector-u8-ref initial-byte->utf8-char-length
			decode-utf8-char 1 "UTF-8" 'utf8->string))
   (set! utf16be->string
	 (bytes-decoder bytevector-u16be-ref initial-u16->utf16-char-length
			decode-utf16be-char 2 "UTF-16BE" 'utf16be->string))
   (set! utf16le->string
	 (bytes-decoder bytevector-u16le-ref initial-u16->utf16-char-length
			decode-utf16le-char 2 "UTF-16LE" 'utf16le->string))
   (set! utf32be->string
	 (bytes-decoder bytevector-u32be-ref initial-u32->utf32-char-length
			decode-utf32be-char 4 "UTF-32BE" 'utf32be->string))
   (set! utf32le->string
	 (bytes-decoder bytevector-u32le-ref initial-u32->utf32-char-length
			decode-utf32le-char 4 "UTF-32LE" 'utf32le->string))
   unspecific))

(define (utf16->string bytevector #!optional start end replace?)
  (let* ((end (fix:end-index end (bytevector-length bytevector) 'utf16->string))
	 (start (fix:start-index start end 'utf16->string)))

    (define (default)
      (if (host-big-endian?)
	  (utf16be->string bytevector start end replace?)
	  (utf16le->string bytevector start end replace?)))

    (if (fix:<= (fix:+ start 2) end)
	(let ((b0 (bytevector-u8-ref bytevector start))
	      (b1 (bytevector-u8-ref bytevector (fix:+ start 1))))
	  (cond ((and (fix:= b0 #xFE) (fix:= b1 #xFF))
		 (utf16be->string bytevector (fix:+ start 2) end replace?))
		((and (fix:= b0 #xFF) (fix:= b1 #xFE))
		 (utf16le->string bytevector (fix:+ start 2) end replace?))
		(else
		 (default))))
	(default))))

(define (utf32->string bytevector #!optional start end replace?)
  (let* ((end (fix:end-index end (bytevector-length bytevector) 'utf32->string))
	 (start (fix:start-index start end 'utf32->string)))

    (define (default)
      (if (host-big-endian?)
	  (utf32be->string bytevector start end replace?)
	  (utf32le->string bytevector start end replace?)))

    (if (fix:<= (fix:+ start 4) end)
	(let ((b0 (bytevector-u8-ref bytevector start))
	      (b1 (bytevector-u8-ref bytevector (fix:+ start 1)))
	      (b2 (bytevector-u8-ref bytevector (fix:+ start 2)))
	      (b3 (bytevector-u8-ref bytevector (fix:+ start 3))))
	  (cond ((and (fix:= b0 0) (fix:= b1 0) (fix:= b2 #xFE) (fix:= b3 #xFF))
		 (utf32be->string bytevector (fix:+ start 4) end replace?))
		((and (fix:= b0 #xFF) (fix:= b1 #xFE) (fix:= b2 0) (fix:= b3 0))
		 (utf32le->string bytevector (fix:+ start 4) end replace?))
		(else
		 (default))))
	(default))))

(define (string->iso8859-1 string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string->iso8859-1))
	 (start (fix:start-index start end 'string->iso8859-1))
	 (result (allocate-bytevector (fix:- end start))))
    (do ((i start (fix:+ i 1))
	 (j 0 (fix:+ j 1)))
	((not (fix:< i end)))
      (bytevector-u8-set! result j (char->integer (string-ref string i))))
    result))

(define (iso8859-1->string bytes #!optional start end)
  (let* ((end (fix:end-index end (bytevector-length bytes) 'iso8859-1->string))
	 (start (fix:start-index start end 'iso8859-1->string))
	 (builder (string-builder)))
    (do ((i start (fix:+ i 1)))
	((not (fix:< i end)))
      (builder (integer->char (bytevector-u8-ref bytes i))))
    (builder)))

(define (bytevector->hexadecimal bytes)
  (define-integrable (hex-char k)
    (string-ref "0123456789ABCDEF" (fix:and k #x0F)))
  (guarantee bytevector? bytes 'bytevector->hexadecimal)
  (let ((n (bytevector-length bytes))
	(builder (string-builder)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)))
      (builder (hex-char (fix:lsh (bytevector-u8-ref bytes i) -4)))
      (builder (hex-char (bytevector-u8-ref bytes i))))
    (builder)))

(define (hexadecimal->bytevector string)
  (guarantee string? string 'hexadecimal->bytevector)
  (let ((end (string-length string))
	(lose
	 (lambda ()
	   (error:bad-range-argument string 'hexadecimal->bytevector))))
    (define-integrable (hex-digit char)
      (let ((i (char->integer char))
	    (d0 (char->integer #\0))
	    (d9 (char->integer #\9))
	    (la (char->integer #\a))
	    (lf (char->integer #\f))
	    (ua (char->integer #\A))
	    (uf (char->integer #\F)))
	(cond ((and (fix:<= d0 i) (fix:<= i d9)) (fix:- i d0))
	      ((and (fix:<= la i) (fix:<= i lf)) (fix:+ #xa (fix:- i la)))
	      ((and (fix:<= ua i) (fix:<= i uf)) (fix:+ #xA (fix:- i ua)))
	      (else (lose)))))
    (if (not (fix:= (fix:and end 1) 0))
	(lose))
    (let ((builder (bytevector-builder)))
      (do ((i 0 (fix:+ i 2)))
	  ((not (fix:< i end)))
	(builder
	 (fix:+ (fix:lsh (hex-digit (string-ref string i)) 4)
		(hex-digit (string-ref string (fix:+ i 1))))))
      (builder))))

(define (bytevector->exact-nonnegative-integer bytes)
  (let ((len (bytevector-length bytes)))
    (do ((i 0 (fix:+ i 1))
	 (n 0 (+ (* n #x100) (bytevector-u8-ref bytes i))))
	((not (fix:< i len)) n))))

(define (exact-nonnegative-integer->bytevector n)
  (guarantee exact-nonnegative-integer? n
	     'exact-nonnegative-integer->bytevector)
  (let* ((n-bytes (quotient (+ (integer-length-in-bits n) 7) 8))
	 (result (make-bytevector n-bytes)))
    (do ((n n (quotient n #x100))
	 (i 0 (fix:+ i 1)))
	((not (> n 0)))
      (bytevector-u8-set! result i (remainder n #x100)))
    result))