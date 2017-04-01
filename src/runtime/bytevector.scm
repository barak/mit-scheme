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

;;;; R7RS bytevectors
;;; package: (runtime bytevector)

(declare (usual-integrations))

(define (register-mit-bytevector-predicates!)
  (register-predicate! u8? 'u8 '<= index-fixnum?)
  (register-predicate! u16? 'u16 '<= index-fixnum?)
  (register-predicate! u32? 'u32 '<= (if (fix:fixnum? #xFFFFFFFF)
					 index-fixnum?
					 exact-nonnegative-integer?)))

(define (u8? object)
  (and (index-fixnum? object)
       (fix:< object #x100)))

(define-primitives
  (allocate-bytevector 1)
  (bytevector-length 1)
  (bytevector-u8-ref 2)
  (bytevector-u8-set! 3)
  (bytevector? 1)
  (legacy-string-allocate string-allocate 1)
  (legacy-string? string? 1))

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
	(%legacy-string->bytevector string))))

(define-integrable (%legacy-string->bytevector string)
  (object-new-type (ucode-type bytevector) string))

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

(define (bytevector-builder)
  (let ((builder
	 (make-sequence-builder allocate-bytevector
				bytevector-length
				bytevector-u8-ref
				bytevector-u8-set!
				(lambda (bv) bv)
				16
				bytevector-builder:finish-build)))
    (lambda (#!optional object)
      (cond ((default-object? object) ((builder 'build)))
	    ((byte? object) ((builder 'append-element!) object))
	    ((bytevector? object) ((builder 'append-sequence!) object))
	    ((memq object '(empty? count reset!)) ((builder object)))
	    (else (error "Not a byte or bytevector:" object))))))

(define (bytevector-builder:finish-build parts)
  (let ((result
	 (do ((parts parts (cdr parts))
	      (n 0 (fix:+ n (cdar parts))))
	     ((not (pair? parts))
	      (allocate-bytevector n)))))
    (do ((parts parts (cdr parts))
	 (i 0 (fix:+ i (cdar parts))))
	((not (pair? parts)))
      (bytevector-copy! result i (caar parts) 0 (cdar parts)))
    result))

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

(define-integrable (list->bytevector-u16-maker u16-byte0 u16-byte1)
  (lambda (u16s)
    (let ((bv (allocate-bytevector (fix:lsh (length u16s) 1))))
      (do ((u16s u16s (cdr u16s))
	   (i 0 (fix:+ i 2)))
	  ((not (pair? u16s)))
	(bytevector-u8-set! bv i (u16-byte0 (car u16s)))
	(bytevector-u8-set! bv (fix:+ i 1) (u16-byte1 (car u16s))))
      bv)))

(define list->bytevector-u16be
  (list->bytevector-u16-maker u16be-byte0 u16be-byte1))

(define list->bytevector-u16le
  (list->bytevector-u16-maker u16le-byte0 u16le-byte1))

(define (bytevector-u16be . u16s)
  (list->bytevector-u16be u16s))

(define (bytevector-u16le . u16s)
  (list->bytevector-u16le u16s))

(define-integrable (bytevector-u16->list-maker bytes->u16 caller)
  (lambda (bv #!optional start end)
    (let* ((end (fix:end-index end (bytevector-length bv) caller))
	   (start (fix:start-index start end caller)))
      (do ((i (fix:- end 2) (fix:- i 2))
	   (bytes '()
		  (cons (bytes->u16 (bytevector-u8-ref bv i)
				    (bytevector-u8-ref bv (fix:+ i 1)))
			bytes)))
	  ((not (fix:>= i start)) bytes)))))

(define bytevector-u16be->list
  (bytevector-u16->list-maker bytes->u16be 'bytevector-u16be->list))

(define bytevector-u16le->list
  (bytevector-u16->list-maker bytes->u16le 'bytevector-u16le->list))

(define-integrable (vector->bytevector-u16-maker u16-byte0 u16-byte1 caller)
  (lambda (v #!optional start end)
    (let* ((end (fix:end-index end (vector-length v) caller))
	   (start (fix:start-index start end caller))
	   (bv (allocate-bytevector (fix:lsh (fix:- end start) 1))))
      (do ((i start (fix:+ i 1))
	   (j 0 (fix:+ j 2)))
	  ((not (fix:< i end)))
	(bytevector-u8-set! bv j (u16-byte0 (vector-ref v i)))
	(bytevector-u8-set! bv (fix:+ j 1) (u16-byte1 (vector-ref v i))))
      bv)))

(define vector->bytevector-u16be
  (vector->bytevector-u16-maker u16be-byte0 u16be-byte1
				'vector->bytevector-u16be))

(define vector->bytevector-u16le
  (vector->bytevector-u16-maker u16le-byte0 u16le-byte1
				'vector->bytevector-u16le))

(define-integrable (bytevector-u16->vector-maker bytes->u16 caller)
  (lambda (bv #!optional start end)
    (let* ((end (fix:end-index end (bytevector-length bv) caller))
	   (start (fix:start-index start end caller))
	   (v (make-vector (fix:lsh (fix:- end start) -1))))
      (do ((i start (fix:+ i 2))
	   (j 0 (fix:+ j 1)))
	  ((not (fix:< (fix:+ i 1) end)))
	(vector-set! bv j
		     (bytes->u16 (bytevector-u8-ref v i)
				 (bytevector-u8-ref v (fix:+ i 1)))))
      v)))

(define bytevector-u16be->vector
  (bytevector-u16->vector-maker bytes->u16be 'bytevector-u16be->vector))

(define bytevector-u16le->vector
  (bytevector-u16->vector-maker bytes->u16le 'bytevector-u16le->vector))

;;;; U32 accessors

(define-syntax select-u32-code
  (er-macro-transformer
   (lambda (form rename compare)
     rename compare
     (syntax-check '(KEYWORD EXPRESSION EXPRESSION) form)
     (if (fix:fixnum? #xFFFFFFFF)
	 (cadr form)
	 (caddr form)))))

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
	  (fix:<= object #xFFFFFFFF))))
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
	  (int:<= object #xFFFFFFFF)))))

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

(define-integrable (list->bytevector-u32-maker u32-byte0 u32-byte1 u32-byte2
					       u32-byte3)
  (lambda (u32s)
    (let ((bv (allocate-bytevector (fix:lsh (length u32s) 2))))
      (do ((u32s u32s (cdr u32s))
	   (i 0 (fix:+ i 4)))
	  ((not (pair? u32s)))
	(bytevector-u8-set! bv i (u32-byte0 (car u32s)))
	(bytevector-u8-set! bv (fix:+ i 1) (u32-byte1 (car u32s)))
	(bytevector-u8-set! bv (fix:+ i 2) (u32-byte2 (car u32s)))
	(bytevector-u8-set! bv (fix:+ i 3) (u32-byte3 (car u32s))))
      bv)))

(define list->bytevector-u32be
  (list->bytevector-u32-maker u32be-byte0 u32be-byte1 u32be-byte2 u32be-byte3))

(define list->bytevector-u32le
  (list->bytevector-u32-maker u32le-byte0 u32le-byte1 u32le-byte2 u32le-byte3))

(define (bytevector-u32be . u32s)
  (list->bytevector-u32be u32s))

(define (bytevector-u32le . u32s)
  (list->bytevector-u32le u32s))

(define-integrable (bytevector-u32->list-maker bytes->u32 caller)
  (lambda (bv #!optional start end)
    (let* ((end (fix:end-index end (bytevector-length bv) caller))
	   (start (fix:start-index start end caller)))
      (do ((i (fix:- end 4) (fix:- i 4))
	   (bytes '()
		  (cons (bytes->u32 (bytevector-u8-ref bv i)
				    (bytevector-u8-ref bv (fix:+ i 1))
				    (bytevector-u8-ref bv (fix:+ i 2))
				    (bytevector-u8-ref bv (fix:+ i 3)))
			bytes)))
	  ((not (fix:>= i start)) bytes)))))

(define bytevector-u32be->list
  (bytevector-u32->list-maker bytes->u32be 'bytevector-u32be->list))

(define bytevector-u32le->list
  (bytevector-u32->list-maker bytes->u32le 'bytevector-u32le->list))

(define-integrable (vector->bytevector-u32-maker u32-byte0 u32-byte1 u32-byte2
						 u32-byte3 caller)
  (lambda (v #!optional start end)
    (let* ((end (fix:end-index end (vector-length v) caller))
	   (start (fix:start-index start end caller))
	   (bv (allocate-bytevector (fix:lsh (fix:- end start) 1))))
      (do ((i start (fix:+ i 1))
	   (j 0 (fix:+ j 4)))
	  ((not (fix:< i end)))
	(bytevector-u8-set! bv j (u32-byte0 (vector-ref v i)))
	(bytevector-u8-set! bv (fix:+ j 1) (u32-byte1 (vector-ref v i)))
	(bytevector-u8-set! bv (fix:+ j 2) (u32-byte2 (vector-ref v i)))
	(bytevector-u8-set! bv (fix:+ j 3) (u32-byte3 (vector-ref v i))))
      bv)))

(define vector->bytevector-u32be
  (vector->bytevector-u32-maker u32be-byte0 u32be-byte1 u32be-byte2 u32be-byte3
				'vector->bytevector-u32be))

(define vector->bytevector-u32le
  (vector->bytevector-u32-maker u32le-byte0 u32le-byte1 u32le-byte2 u32le-byte3
				'vector->bytevector-u32le))

(define-integrable (bytevector-u32->vector-maker bytes->u32 caller)
  (lambda (bv #!optional start end)
    (let* ((end (fix:end-index end (bytevector-length bv) caller))
	   (start (fix:start-index start end caller))
	   (v (make-vector (fix:lsh (fix:- end start) -2))))
      (do ((i start (fix:+ i 4))
	   (j 0 (fix:+ j 1)))
	  ((not (fix:< (fix:+ i 3) end)))
	(vector-set! bv j
		     (bytes->u32 (bytevector-u8-ref v i)
				 (bytevector-u8-ref v (fix:+ i 1))
				 (bytevector-u8-ref v (fix:+ i 2))
				 (bytevector-u8-ref v (fix:+ i 3)))))
      v)))

(define bytevector-u32be->vector
  (bytevector-u32->vector-maker bytes->u32be 'bytevector-u32be->vector))

(define bytevector-u32le->vector
  (bytevector-u32->vector-maker bytes->u32le 'bytevector-u32le->vector))

(define-integrable (string-encoder char-byte-length allocator encode-char!
				   caller)
  (lambda (string #!optional start end)
    (let* ((end (fix:end-index end (string-length string) caller))
	   (start (fix:start-index start end caller)))
      (let ((bytes
	     (allocator
	      (let loop ((index start) (n-bytes 0))
		(if (fix:< index end)
		    (loop (fix:+ index 1)
			  (fix:+ n-bytes
				 (char-byte-length (string-ref string index))))
		    n-bytes)))))
	(let loop ((from start) (to 0))
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
(define string->utf32be)
(define string->utf32le)
(add-boot-init!
 (lambda ()
   (set! string->utf8
	 (string-encoder char-utf8-byte-length utf8-allocator
			 encode-utf8-char! 'string->utf8))
   (set! string->utf16be
	 (string-encoder char-utf16-byte-length allocate-bytevector
			 encode-utf16be-char! 'string->utf16be))
   (set! string->utf16le
	 (string-encoder char-utf16-byte-length allocate-bytevector
			 encode-utf16le-char! 'string->utf16le))
   (set! string->utf32be
	 (string-encoder char-utf32-byte-length allocate-bytevector
			 encode-utf32be-char! 'string->utf32be))
   (set! string->utf32le
	 (string-encoder char-utf32-byte-length allocate-bytevector
			 encode-utf32le-char! 'string->utf32le))
   unspecific))

(define-integrable (bytes-decoder getter initial->length decode-char step noun
				  caller)
  (lambda (bytevector #!optional start end)
    (let* ((end (fix:end-index end (bytevector-length bytevector) caller))
	   (start (fix:start-index start end caller))
	   (builder (string-builder)))
      (let ((truncated
	     (lambda (index)
	       (error (string "Truncated " noun " sequence:")
		      (bytevector-copy bytevector
				       index
				       (fix:min (fix:+ index 4) end))))))
	(let loop ((index start))
	  (if (fix:<= (fix:+ index step) end)
	      (let ((n (initial->length (getter bytevector index))))
		(let ((index* (fix:+ index n)))
		  (if (not (fix:<= index* end))
		      (truncated index))
		  (builder (decode-char bytevector index))
		  (loop index*)))
	      (if (fix:< index end)
		  (truncated index)))))
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
			decode-utf16be-char 1 "UTF-16BE" 'utf16be->string))
   (set! utf16le->string
	 (bytes-decoder bytevector-u16le-ref initial-u16->utf16-char-length
			decode-utf16le-char 1 "UTF-16LE" 'utf16le->string))
   (set! utf32be->string
	 (bytes-decoder bytevector-u32be-ref initial-u32->utf32-char-length
			decode-utf32be-char 1 "UTF-32BE" 'utf32be->string))
   (set! utf32le->string
	 (bytes-decoder bytevector-u32le-ref initial-u32->utf32-char-length
			decode-utf32le-char 1 "UTF-32LE" 'utf32le->string))
   unspecific))

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
	    (UA (char->integer #\A))
	    (UF (char->integer #\F)))
	(cond ((and (fix:<= d0 i) (fix:<= i d9)) (fix:- i d0))
	      ((and (fix:<= la i) (fix:<= i lf)) (fix:+ #xa (fix:- i la)))
	      ((and (fix:<= UA i) (fix:<= i UF)) (fix:+ #xA (fix:- i UA)))
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