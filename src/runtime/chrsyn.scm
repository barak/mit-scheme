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

;;;; Character-Syntax Tables
;;; package: (runtime char-syntax)

(declare (usual-integrations))

(define-structure (char-syntax-table (constructor %make-char-syntax-table)
				     (conc-name char-syntax-table/))
  (entries #f read-only #t))

(define (guarantee-char-syntax-table table procedure)
  (if (not (char-syntax-table? table))
      (error:wrong-type-argument table "char-syntax table" procedure))
  (char-syntax-table/entries table))

(define (make-char-syntax-table #!optional table)
  (%make-char-syntax-table
   (vector-copy
    (if (or (default-object? table) (not table))
	(char-syntax-table/entries standard-char-syntax-table)
	(guarantee-char-syntax-table table 'make-char-syntax-table)))))

(define (get-char-syntax table char)
  (vector-ref (guarantee-char-syntax-table table 'get-char-syntax)
	      (char->integer char)))

(define (set-char-syntax! table char string)
  (let ((entries (guarantee-char-syntax-table table 'set-char-syntax!))
	(entry (string->char-syntax string)))
    (cond ((char? char)
	   (vector-set! entries (char->integer char) entry))
	  ((char-set? char)
	   (for-each (lambda (char)
		       (vector-set! entries (char->integer char) entry))
		     (char-set-members char)))
	  (else
	   (error:wrong-type-argument char "character" 'set-char-syntax!)))))

(define standard-char-syntax-table)

(define (initialize-package!)
  (let ((table
	  (%make-char-syntax-table
	   (make-vector 256 (string->char-syntax "")))))
    (set-char-syntax! table char-set:alphanumeric "w")
    (set-char-syntax! table #\$ "w")
    (set-char-syntax! table #\% "w")
    (set-char-syntax! table #\( "()")
    (set-char-syntax! table #\) ")(")
    (set-char-syntax! table #\[ "(]")
    (set-char-syntax! table #\] ")[")
    (set-char-syntax! table #\{ "(}")
    (set-char-syntax! table #\} "){")
    (set-char-syntax! table #\" "\"")
    (set-char-syntax! table #\\ "\\")
    (set-char-syntax! table (string->char-set "_-+*/&|<>=") "_")
    (set-char-syntax! table (string->char-set ".,;:?!#@~^'`") ".")

    ;; Use ISO-8859-1 for characters in upper half.
    (let ((iso-setter
	   (lambda (string)
	     (lambda (entry)
	       (if (pair? entry)
		   (do ((i (car entry) (+ i 1)))
		       ((> i (cadr entry)))
		     (set-char-syntax! table (integer->char i) string))
		   (set-char-syntax! table (integer->char entry) string))))))
      (for-each (iso-setter "w")
		'((162 165) 168 170 175 (178 180) 182 (184 186)
			    (192 214) (216 246) (248 254) 255))
      (for-each (iso-setter "_")
		'(166 169 (172 174) (176 177) 181 183 (188 190) 215 247))
      (for-each (iso-setter ".")
		'(161 167 191)))
    (set-char-syntax! table (integer->char 160) " ")
    (set-char-syntax! table (integer->char 171)
		      (string #\( (integer->char 187)))
    (set-char-syntax! table (integer->char 187)
		      (string #\) (integer->char 171)))

    (set! standard-char-syntax-table table)
    unspecific))

(define-primitives
  (string->char-syntax string->syntax-entry))

(define (char-syntax->string entry)
  (guarantee-char-syntax entry 'char-syntax->string)
  (let ((code (fix:and #xf entry)))
    (string-append
     (vector-ref char-syntax-codes code)
     (let ((match (fix:and #xff (fix:lsh entry -4))))
       (if (zero? match)
	   " "
	   (string (integer->char match))))
     (let ((cbits (fix:and #xFF (fix:lsh entry -12))))
       (string-append
	(if (fix:= 0 (fix:and #x40 cbits)) "" "1")
	(if (fix:= 0 (fix:and #x10 cbits)) "" "2")
	(if (fix:= 0 (fix:and #x04 cbits)) "" "3")
	(if (fix:= 0 (fix:and #x01 cbits)) "" "4")
	(if (or (fix:= 0 (fix:and #x80 cbits))
		(and (fix:= code 11)
		     (fix:= #x80 (fix:and #xC0 cbits))))
	    ""
	    "5")
	(if (fix:= 0 (fix:and #x20 cbits)) "" "6")
	(if (or (fix:= 0 (fix:and #x08 cbits))
		(and (fix:= code 12)
		     (fix:= #x08 (fix:and #x0C cbits))))
	    ""
	    "7")
	(if (fix:= 0 (fix:and #x02 cbits)) "" "8")))
     (if (fix:= 0 (fix:and #x100000 entry)) "" "p"))))

(define (guarantee-char-syntax object procedure)
  (if (not (index-fixnum? object))
      (error:wrong-type-argument object "non-negative fixnum" procedure))
  (if (not (and (fix:< object #x200000)
		(fix:<= (fix:and #xf object) 12)))
      (error:bad-range-argument object procedure)))

(define (char-syntax-code? object)
  (and (char? object)
       (let ((n (vector-length char-syntax-codes)))
	 (let loop ((i 0))
	   (and (fix:< i n)
		(or (char=? (string-ref (vector-ref char-syntax-codes i) 0)
			    object)
		    (loop (fix:+ i 1))))))))

(define (char->syntax-code table char)
  (string-ref (vector-ref char-syntax-codes
			  (fix:and #xf (get-char-syntax table char)))
	      0))

(define char-syntax-codes
  '#(" " "." "w" "_" "(" ")" "'" "\"" "$" "\\" "/" "<" ">"))

(define (substring-find-next-char-of-syntax string start end table code)
  (guarantee 8-bit-string? string 'substring-find-next-char-of-syntax)
  (let ((index
	 (string-find-first-index (syntax-code-predicate code table)
				  (string-slice string start end))))
    (and index
	 (fix:+ start index))))

(define (substring-find-next-char-not-of-syntax string start end table code)
  (guarantee 8-bit-string? string 'substring-find-next-char-not-of-syntax)
  (let ((index
	 (string-find-first-index (let ((pred
					 (syntax-code-predicate code table)))
				    (lambda (char)
				      (not (pred char))))
				  (string-slice string start end))))
    (and index
	 (fix:+ start index))))

(define (syntax-code-predicate code #!optional table)
  (guarantee char-syntax-code? code 'syntax-code-predicate)
  (let ((entries
	 (char-syntax-table/entries
	  (if (default-object? table)
	      standard-char-syntax-table
	      (begin
		(guarantee char-syntax-table? table 'syntax-code-predicate)
		table)))))
    (lambda (char)
      (let ((cp (char->integer char)))
	(and (fix:< cp #x100)
	     (char=? (string-ref (vector-ref char-syntax-codes
					     (fix:and #x0F
						      (vector-ref entries cp)))
				 0)
		     code))))))