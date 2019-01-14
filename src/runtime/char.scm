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

;;;; Character Abstraction
;;; package: (runtime character)

(declare (usual-integrations))

(define-primitives
  (char? 1)
  (char->integer 1)
  (integer->char 1))

(define-integrable char-code-limit #x110000)
(define-integrable char-bits-limit #x10)

(define-guarantee char "character")

(define (make-char code bits)
  (guarantee-limited-index-fixnum code char-code-limit 'make-char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'make-char)
  (%make-char code bits))

(define-integrable (%make-char code bits)
  (integer->char (fix:or (fix:lsh bits 21) code)))

(define (char-code char)
  (fix:and (char->integer char) #x1FFFFF))

(define (char-bits char)
  (fix:lsh (char->integer char) -21))

(define (bitless-char? object)
  (and (char? object)
       (fix:< (char->integer object) char-code-limit)))

(define (char-bits-set? bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'char-bits-set?)
  (fix:= bits (fix:and (char-bits char) bits)))

(define (char-bits-clear? bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'char-bits-clear?)
  (fix:= 0 (fix:and (char-bits char) bits)))

(define (set-char-bits bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'set-char-bits)
  (%make-char (char-code char)
	      (fix:or (char-bits char) bits)))

(define (clear-char-bits bits char)
  (guarantee-limited-index-fixnum bits char-bits-limit 'clear-char-bits)
  (%make-char (char-code char)
	      (fix:andc (char-bits char) bits)))

(define (8-bit-char? object)
  (and (char? object)
       (char-8-bit? object)))

(define-integrable (char-8-bit? char)
  (fix:< (char->integer char) #x100))

(define (ascii-char? object)
  (and (char? object)
       (char-ascii? object)))

(define-integrable (char-ascii? char)
  (fix:< (char->integer char) #x80))

(define (char=-predicate char)
  (guarantee char? char 'char=-predicate)
  (lambda (char*)
    (and (char? char*)
	 (char=? char* char))))

(define (char-ci=-predicate char)
  (guarantee char? char 'char-ci=-predicate)
  (lambda (char*)
    (and (char? char*)
	 (char-ci=? char* char))))

(define-integrable (%char=? x y)
  (fix:= (char->integer x) (char->integer y)))

(define-integrable (%char<? x y)
  (fix:< (char->integer x) (char->integer y)))

(define-integrable (%char<=? x y)
  (fix:<= (char->integer x) (char->integer y)))

(define-integrable (%char>? x y)
  (fix:> (char->integer x) (char->integer y)))

(define-integrable (%char>=? x y)
  (fix:>= (char->integer x) (char->integer y)))

(define (%char-ci=? x y)
  (%char=? (char-foldcase x) (char-foldcase y)))

(define (%char-ci<? x y)
  (%char<? (char-foldcase x) (char-foldcase y)))

(define (%char-ci<=? x y)
  (%char<=? (char-foldcase x) (char-foldcase y)))

(define (%char-ci>? x y)
  (%char>? (char-foldcase x) (char-foldcase y)))

(define (%char-ci>=? x y)
  (%char>=? (char-foldcase x) (char-foldcase y)))

(define-integrable (char-comparison-maker %compare)
  (lambda (char1 char2 . chars)
    (let loop ((char1 char1) (char2 char2) (chars chars))
      (if (pair? chars)
	  (and (%compare char1 char2)
	       (loop char2 (car chars) (cdr chars)))
	  (%compare char1 char2)))))

(define char=? (char-comparison-maker %char=?))
(define char<? (char-comparison-maker %char<?))
(define char<=? (char-comparison-maker %char<=?))
(define char>? (char-comparison-maker %char>?))
(define char>=? (char-comparison-maker %char>=?))

(define char-ci=? (char-comparison-maker %char-ci=?))
(define char-ci<? (char-comparison-maker %char-ci<?))
(define char-ci<=? (char-comparison-maker %char-ci<=?))
(define char-ci>? (char-comparison-maker %char-ci>?))
(define char-ci>=? (char-comparison-maker %char-ci>=?))

(define char-downcase)
(define char-foldcase)
(define char-upcase)
(add-boot-init!
 (lambda ()

   (define (char-mapper mapper)
     (lambda (char)
       (if (fix:= 0 (char-bits char))
	   (mapper char)
	   (%make-char (char-code (mapper (%make-char (char-code char) 0)))
		       (char-bits char)))))

   (set! char-downcase (char-mapper ucd-slc-value))
   (set! char-foldcase (char-mapper ucd-scf-value))
   (set! char-upcase (char-mapper ucd-suc-value))
   unspecific))

(define (digit-value char)
  (and (char-numeric? char)
       (ucd-nv-value char)))

(define (radix? object)
  (and (index-fixnum? object)
       (fix:<= 2 object)
       (fix:<= object 36)))

(define-guarantee radix "radix")

(define (digit->char digit #!optional radix)
  (let ((radix
	 (if (default-object? radix)
	     10
	     (begin
	       (guarantee radix? radix 'digit->char)
	       radix))))
    (guarantee index-fixnum? digit 'digit->char)
    (if (not (fix:< digit radix))
	(error:bad-range-argument digit 'digit->char)))
  (string-ref "0123456789abcdefghijklmnopqrstuvwxyz" digit))

(define (char->digit char #!optional radix)
  (let ((radix
	 (if (default-object? radix)
	     10
	     (begin
	       (guarantee radix? radix 'char->digit)
	       radix)))
	(digit (digit-value char)))
    (if digit
	(and (fix:< digit radix)
	     digit)
	(and (fix:> radix 10)
	     (let ((code (char->integer char)))
	       (let ((n (fix:- code (fix:- (char->integer #\A) 10))))
		 (if (and (fix:<= 10 n) (fix:< n radix))
		     n
		     (let ((n (fix:- code (fix:- (char->integer #\a) 10))))
		       (if (and (fix:<= 10 n) (fix:< n radix))
			   n
			   #f)))))))))

;;;; Character names

(define (name->char string #!optional fold-case?)
  (let ((fold-case? (if (default-object? fold-case?) #t fold-case?))
	(lose (lambda () (error:bad-range-argument string 'name->char))))
    (let ((parse-hex
	   (lambda (string start)
	     (let ((cp (string->number string 16 #t start)))
	       (if (not (unicode-code-point? cp))
		   (lose))
	       cp))))
      (receive (string bits) (match-bucky-bits-prefix string fold-case?)
	(let ((end (string-length string)))
	  (if (fix:= 0 end)
	      (lose))
	  (make-char (cond ((fix:= 1 end)
			    (char-code (string-ref string 0)))
			   ;; R7RS syntax
			   ((char=? #\x (string-ref string 0))
			    (parse-hex string 1))
			   ;; Non-standard syntax (Unicode style)
			   ((and (char-ci=? #\u (string-ref string 0))
				 (char=? #\+ (string-ref string 1)))
			    (parse-hex string 2))
			   ((match-named-code string fold-case?))
			   (else (lose)))
		     bits))))))

(define (char->name char)
  (let ((bits (char-bits char))
	(code (char-code char)))
    (string-append
     (bucky-bits->prefix bits)
     (cond ((code->name code))
	   ((and (fix:> code #x20)
		 (fix:< code #x80))
	    (string (integer->char code)))
	   (else
	    (string-append "x" (number->string code 16)))))))

(define (match-bucky-bits-prefix string fold-case?)
  (let ((match? (if fold-case? string-prefix-ci? string-prefix?)))
    (let per-index ((index 0) (bits 0))
      (let per-entry ((entries named-bits))
	(if (pair? entries)
	    (let* ((entry (car entries))
		   (prefix
		    (find (lambda (prefix)
			    (match? prefix string index))
			  (cdr entry))))
	      (if prefix
		  (per-index (fix:+ index (string-length prefix))
			     (fix:or bits (car entry)))
		  (per-entry (cdr entries))))
	    (values (if (fix:> index 0)
			(string-tail string index)
			string)
		    bits))))))

;; This procedure used by Edwin.
(define (bucky-bits->prefix bits)
  (guarantee index-fixnum? bits 'bucky-bits->prefix)
  (if (not (fix:< bits char-bits-limit))
      (error:bad-range-argument bits 'bucky-bits->prefix))
  (vector-ref bits-prefixes bits))

(define-deferred bits-prefixes
  (list->vector
   (map (lambda (bits)
	  (apply string-append
		 (filter-map (lambda (entry)
			       (if (fix:= 0 (fix:and (car entry) bits))
				   #f
				   (cadr entry)))
			     named-bits)))
	(fix:iota char-bits-limit))))

(define char-bit:meta #x01)
(define char-bit:control #x02)
(define char-bit:super #x04)
(define char-bit:hyper #x08)

(define named-bits
  `((,char-bit:hyper "H-" "h-" "hyper-")
    (,char-bit:super "S-" "s-" "super-")
    (,char-bit:meta "M-" "m-" "meta-")
    (,char-bit:control "C-" "c-" "control-" "ctrl-")))

(define (match-named-code string fold-case?)
  (let ((match? (if fold-case? string-ci=? string=?)))
    (find-map (lambda (entry)
		(and (any (lambda (name)
			    (match? name string))
			  (cdr entry))
		     (car entry)))
	      named-codes)))

(define named-codes
  '((#x00 "null" "nul")
    (#x01 "soh")
    (#x02 "stx")
    (#x03 "etx")
    (#x04 "eot")
    (#x05 "enq")
    (#x06 "ack")
    (#x07 "alarm" "bel")
    (#x08 "backspace" "bs")
    (#x09 "tab" "ht")
    (#x0A "newline" "linefeed" "lfd" "lf")
    (#x0B "vt")
    (#x0C "page" "formfeed" "ff" "np")
    (#x0D "return" "ret" "cr")
    (#x0E "so")
    (#x0F "si")
    (#x10 "dle")
    (#x11 "dc1")
    (#x12 "dc2")
    (#x13 "dc3")
    (#x14 "dc4")
    (#x15 "nak")
    (#x16 "syn")
    (#x17 "etb")
    (#x18 "can")
    (#x19 "em")
    (#x1A "sub" "call")
    (#x1B "escape" "esc" "altmode")
    (#x1C "fs")
    (#x1D "gs")
    (#x1E "rs")
    (#x1F "us" "backnext")
    (#x20 "space" "spc" "sp")
    (#x7F "delete" "del" "rubout")
    (#xA0 "nbsp")
    (#xFEFF "bom")))

;; These are the standard R7RS names.
(define (code->name code)
  (case code
    ((#x00) "null")
    ((#x07) "alarm")
    ((#x08) "backspace")
    ((#x09) "tab")
    ((#x0A) "newline")
    ((#x0D) "return")
    ((#x1B) "escape")
    ((#x20) "space" )
    ((#x7F) "delete")
    (else #f)))

;;;; Unicode characters

(define (unicode-scalar-value? object)
  (and (unicode-code-point? object)
       (not (utf16-surrogate? object))))

(define (unicode-code-point? object)
  (and (index-fixnum? object)
       (fix:< object char-code-limit)))

(define-guarantee unicode-char "a Unicode character")
(define-guarantee unicode-scalar-value "a Unicode scalar value")

(define (%char->scalar-value char #!optional caller)
  (let ((n (char->integer char)))
    (guarantee unicode-scalar-value? n caller)
    n))

(define (char-general-category char)
  (guarantee bitless-char? char 'char-general-category)
  (ucd-gc-value char))

(define (code-point-general-category cp)
  (guarantee unicode-code-point? cp 'code-point-general-category)
  (ucd-gc-value (integer->char cp)))

(define-integrable (utf16-surrogate? cp)
  (fix:= #xD800 (fix:and #xF800 cp)))

(define-integrable (utf16-high-surrogate? cp)
  (fix:= #xD800 (fix:and #xFC00 cp)))

(define-integrable (utf16-low-surrogate? cp)
  (fix:= #xDC00 (fix:and #xFC00 cp)))

(define-integrable (non-character? cp)
  (or (and (fix:<= #xFDD0 cp) (fix:< cp #xFDF0))
      (fix:= #xFFFE (fix:and #xFFFE cp))))

(define-integrable (guarantee-cp-is-character cp)
  (if (non-character? cp)
      (error "Code point is a non-character:" cp)))

(define-integrable (guarantee-cp-in-range cp)
  (if (not (fix:< cp char-code-limit))
      (error "Value is not a code point:" cp)))

(define-integrable (guarantee-cp-not-utf16-surrogate cp)
  (if (utf16-surrogate? cp)
      (error "Code point is a UTF-16 surrogate:" cp)))

(define-integrable (extract-bits word mask shift)
  (fix:lsh (fix:and word mask) shift))

(define-integrable (insert-bits word mask shift)
  (fix:and (fix:lsh word shift) mask))

;;;; UTF-{8,16,32} encoders

(define (char-utf8-byte-length char)
  (let ((sv (%char->scalar-value char 'char-utf8-byte-length)))
    (cond ((fix:< sv #x80) 1)
	  ((fix:< sv #x800) 2)
	  ((fix:< sv #x10000) 3)
	  (else 4))))

(define (encode-utf8-char! bytes index char)
  (let ((sv (%char->scalar-value char 'encode-utf8-char!)))

    (define-integrable (initial-byte leader offset)
      (fix:or leader (fix:lsh sv offset)))

    (define-integrable (trailing-byte offset)
      (fix:or #x80 (insert-bits sv #x3F offset)))

    (cond ((fix:< sv #x00000080)
	   (bytevector-u8-set! bytes index sv)
	   (fix:+ index 1))
	  ((fix:< sv #x00000800)
	   (bytevector-u8-set! bytes index (initial-byte #xC0 -6))
	   (bytevector-u8-set! bytes (fix:+ index 1) (trailing-byte 0))
	   (fix:+ index 2))
	  ((fix:< sv #x00010000)
	   (bytevector-u8-set! bytes index (initial-byte #xE0 -12))
	   (bytevector-u8-set! bytes (fix:+ index 1) (trailing-byte -6))
	   (bytevector-u8-set! bytes (fix:+ index 2) (trailing-byte 0))
	   (fix:+ index 3))
	  (else
	   (bytevector-u8-set! bytes index (initial-byte #xF0 -18))
	   (bytevector-u8-set! bytes (fix:+ index 1) (trailing-byte -12))
	   (bytevector-u8-set! bytes (fix:+ index 2) (trailing-byte -6))
	   (bytevector-u8-set! bytes (fix:+ index 3) (trailing-byte 0))
	   (fix:+ index 4)))))

(define (char-utf16-byte-length char)
  (if (fix:< (%char->scalar-value char 'char-utf16-byte-length) #x10000)
      2
      4))

(define (utf16-char-encoder setter caller)
  (lambda (bytes index char)
    (let ((sv (%char->scalar-value char caller)))
      (cond ((fix:< sv #x10000)
	     (setter bytes index sv)
	     (fix:+ index 2))
	    (else
	     (let ((n (fix:- sv #x10000)))
	       (setter bytes index
		       (fix:or #xD800 (insert-bits n #x3FF -10)))
	       (setter bytes (fix:+ index 2)
		       (fix:or #xDC00 (insert-bits n #x3FF 0))))
	     (fix:+ index 4))))))

(define encode-utf16be-char!
  (utf16-char-encoder bytevector-u16be-set! 'encode-utf16be-char!))

(define encode-utf16le-char!
  (utf16-char-encoder bytevector-u16le-set! 'encode-utf16le-char!))

(define (char-utf32-byte-length char)
  (%char->scalar-value char 'char-utf32-byte-length)
  4)

(define (utf32-char-encoder setter caller)
  (lambda (bytes index char)
    (setter bytes index (%char->scalar-value char caller))))

(define encode-utf32be-char!
  (utf32-char-encoder bytevector-u32be-set! 'encode-utf32be-char!))

(define encode-utf32le-char!
  (utf32-char-encoder bytevector-u32le-set! 'encode-utf32le-char!))

;;;; UTF-{8,16,32} decoders

(define (initial-byte->utf8-char-length b0)
  (guarantee u8? b0 'initial-byte->utf8-char-length)
  (cond ((utf8-initial-byte-1? b0) 1)
	((utf8-initial-byte-2? b0) 2)
	((utf8-initial-byte-3? b0) 3)
	((utf8-initial-byte-4? b0) 4)
	(else (error "Illegal UTF-8 initial byte:" b0))))

(define (next-char-length:utf8 bv bs be)
  (and (fix:<= (fix:+ bs 1) be)
       (initial-byte->utf8-char-length (bytevector-u8-ref bv bs))))

(define (decode-utf8-char bytes index)
  (integer->char
   (let ((b0 (bytevector-u8-ref bytes index)))
     (cond ((utf8-initial-byte-1? b0)
	    b0)
	   ((utf8-initial-byte-2? b0)
	    (let ((b1 (bytevector-u8-ref bytes (fix:+ index 1))))
	      (if (not (valid-utf8-sequence-2? b0 b1))
		  (error "Ill-formed UTF-8 sequence:" b0 b1))
	      (fix:or (extract-bits b0 #x1F 6)
		      (extract-bits b1 #x3F 0))))
	   ((utf8-initial-byte-3? b0)
	    (let ((b1 (bytevector-u8-ref bytes (fix:+ index 1)))
		  (b2 (bytevector-u8-ref bytes (fix:+ index 2))))
	      (if (not (valid-utf8-sequence-3? b0 b1 b2))
		  (error "Ill-formed UTF-8 sequence:" b0 b1 b2))
	      (fix:or (fix:or (extract-bits b0 #x0F 12)
			      (extract-bits b1 #x3F 6))
		      (extract-bits b2 #x3F 0))))
	   ((utf8-initial-byte-4? b0)
	    (let ((b1 (bytevector-u8-ref bytes (fix:+ index 1)))
		  (b2 (bytevector-u8-ref bytes (fix:+ index 2)))
		  (b3 (bytevector-u8-ref bytes (fix:+ index 3))))
	      (if (not (valid-utf8-sequence-4? b0 b1 b2 b3))
		  (error "Ill-formed UTF-8 sequence:" b0 b1 b2 b3))
	      (fix:or (fix:or (extract-bits b0 #x07 18)
			      (extract-bits b1 #x3F 12))
		      (fix:or (extract-bits b2 #x3F 6)
			      (extract-bits b3 #x3F 0)))))
	   (else
	    (error "Illegal UTF-8 initial byte:" b0))))))

(define-integrable (utf8-initial-byte-1? byte)
  (fix:= #x00 (fix:and #x80 byte)))

(define-integrable (utf8-initial-byte-2? byte)
  (and (fix:>= byte #xC2) (fix:<= byte #xDF)))

(define-integrable (utf8-initial-byte-3? byte)
  (fix:= #xE0 (fix:and #xF0 byte)))

(define-integrable (utf8-initial-byte-4? byte)
  (and (fix:>= byte #xF0) (fix:<= byte #xF4)))

;;  code-point range     b0
;; ------------------  ------
;; U+000000..U+00007F  00..7F
(define-integrable (valid-utf8-sequence-1? b0)
  (utf8-initial-byte-1? b0))

;;  code-point range     b0      b1
;; ------------------  ------  ------
;; U+000080..U+0007FF  C2..DF  80..BF
(define-integrable (valid-utf8-sequence-2? b0 b1)
  (and (utf8-initial-byte-2? b0)
       (u8:80..bf? b1)))

;;  code-point range     b0      b1      b2
;; ------------------  ------  ------  ------
;; U+000800..U+000FFF  E0      A0..BF  80..BF
;; U+001000..U+00CFFF  E1..EC  80..BF  80..BF
;; U+00D000..U+00D7FF  ED      80..9F  80..BF
;; U+00E000..U+00FFFF  EE..EF  80..BF  80..BF
(define-integrable (valid-utf8-sequence-3? b0 b1 b2)
  (and (utf8-initial-byte-3? b0)
       (cond ((fix:= b0 #xE0) (u8:a0..bf? b1))
	     ((fix:< b0 #xED) (u8:80..bf? b1))
	     ((fix:= b0 #xED) (u8:80..9f? b1))
	     (else            (u8:80..bf? b1)))
       (u8:80..bf? b2)))

;;  code-point range     b0      b1      b2      b3
;; ------------------  ------  ------  ------  ------
;; U+010000..U+03FFFF  F0      90..BF  80..BF  80..BF
;; U+040000..U+0FFFFF  F1..F3  80..BF  80..BF  80..BF
;; U+100000..U+10FFFF  F4      80..8F  80..BF  80..BF
(define-integrable (valid-utf8-sequence-4? b0 b1 b2 b3)
  (and (utf8-initial-byte-4? b0)
       (cond ((fix:= b0 #xF0) (u8:90..bf? b1))
	     ((fix:< b0 #xF4) (u8:80..bf? b1))
	     (else            (u8:80..8f? b1)))
       (u8:80..bf? b2)
       (u8:80..bf? b3)))

;; Trailing bytes:

(define-integrable (u8:80..8f? byte)
  (fix:= #x80 (fix:and #xF0 byte)))

(define-integrable (u8:80..9f? byte)
  (fix:= #x80 (fix:and #xE0 byte)))

(define-integrable (u8:80..bf? byte)
  (fix:= #x80 (fix:and #xC0 byte)))

(define-integrable (u8:90..bf? byte)
  (and (fix:>= byte #x90) (fix:<= byte #xBF)))

(define-integrable (u8:a0..bf? byte)
  (and (fix:>= byte #xA0) (fix:<= byte #xBF)))

(define (initial-u16->utf16-char-length u16)
  (guarantee u16? u16 'initial-u16->utf16-char-length)
  (if (utf16-low-surrogate? u16)
      (error "Illegal initial UTF-16 unit:" u16))
  (if (utf16-high-surrogate? u16)
      4
      2))

(define (next-char-length:utf16le bv bs be)
  (and (fix:<= (fix:+ bs 2) be)
       (initial-u16->utf16-char-length (bytevector-u16le-ref bv bs))))

(define (next-char-length:utf16be bv bs be)
  (and (fix:<= (fix:+ bs 2) be)
       (initial-u16->utf16-char-length (bytevector-u16be-ref bv bs))))

(define (utf16-char-decoder getter)
  (lambda (bytes index)
    (integer->char
     (let ((d0 (getter bytes index)))
       (if (utf16-low-surrogate? d0)
	   (error "Illegal initial UTF-16 unit:" d0))
       (if (utf16-high-surrogate? d0)
	   (let ((d1 (getter bytes (fix:+ index 2))))
	     (if (not (utf16-low-surrogate? d1))
		 (error "Ill-formed UTF-16 sequence:" d0 d1))
	     (fix:+ (fix:or (extract-bits d0 #x3FF 10)
			    (extract-bits d1 #x3FF 0))
		    #x10000))
	   d0)))))

(define decode-utf16be-char
  (utf16-char-decoder bytevector-u16be-ref))

(define decode-utf16le-char
  (utf16-char-decoder bytevector-u16le-ref))

(define (initial-u32->utf32-char-length u32)
  (guarantee unicode-scalar-value? u32 'initial-u32->utf32-char-length)
  4)

(define (next-char-length:utf32le bv bs be)
  (and (fix:<= (fix:+ bs 2) be)
       (initial-u32->utf32-char-length (bytevector-u32le-ref bv bs))))

(define (next-char-length:utf32be bv bs be)
  (and (fix:<= (fix:+ bs 2) be)
       (initial-u32->utf32-char-length (bytevector-u32be-ref bv bs))))

(define (utf32-char-decoder getter)
  (lambda (bytes index)
    (let ((u32 (getter bytes index)))
      (guarantee unicode-scalar-value? u32 'utf32-char-decoder)
      (integer->char u32))))

(define decode-utf32be-char
  (utf32-char-decoder bytevector-u32be-ref))

(define decode-utf32le-char
  (utf32-char-decoder bytevector-u32le-ref))

;;;; Codecs

(define-record-type <char-codec>
    (make-char-codec encoder decoder)
    char-codec?
  (encoder char-codec-encoder)
  (decoder char-codec-decoder))

(define get-char-codec)
(define set-char-codec!)
(add-boot-init!
 (lambda ()
   (let ((table (make-alist-metadata-table)))
     (set! get-char-codec (bundle-ref table 'get))
     (set! set-char-codec! (bundle-ref table 'put!))
     unspecific)))

(define (define-char-codec name codec)
  (add-boot-init! (lambda () (set-char-codec! name codec))))

(define (unicode-codec char-byte-length encode-char!
		       next-char-length decode-char)
  (make-char-codec
   (lambda (bv bs be char)
     (let ((bs* (fix:+ bs (char-byte-length char))))
       (and (fix:<= bs* be)
	    (begin
	      (encode-char! bv bs char)
	      bs*))))
   (lambda (bv bs be k)
     (let ((n (next-char-length bv bs be)))
       (if (not n)
	   (k #f bs)
	   (let ((bs* (fix:+ bs n)))
	     (k (and (fix:<= bs* be)
		     (decode-char bv bs))
		bs*)))))))

(define-char-codec 'utf8
  (unicode-codec char-utf8-byte-length encode-utf8-char!
		 next-char-length:utf8 decode-utf8-char))

(define-char-codec 'utf16le
  (unicode-codec char-utf16-byte-length encode-utf16le-char!
		 next-char-length:utf16le decode-utf16le-char))

(define-char-codec 'utf16be
  (unicode-codec char-utf16-byte-length encode-utf16be-char!
		 next-char-length:utf16be decode-utf16be-char))

(define-char-codec 'utf32le
  (unicode-codec char-utf32-byte-length encode-utf32le-char!
		 next-char-length:utf32le decode-utf32le-char))

(define-char-codec 'utf32be
  (unicode-codec char-utf32-byte-length encode-utf32be-char!
		 next-char-length:utf32be decode-utf32be-char))

(define-char-codec 'iso-8859-1
  (make-char-codec
   (lambda (bv bs be char)
     (let ((bs* (fix:+ bs 1)))
       (and (fix:<= bs* be)
	    (begin
	      (bytevector-u8-set! bv bs (char->integer char))
	      bs*))))
   (lambda (bv bs be k)
     (let ((bs* (fix:+ bs 1)))
       (k (and (fix:<= bs* be)
	       (integer->char (bytevector-u8-ref bv bs)))
	  bs*)))))