;;; -*-Scheme-*-
;;;
;;; $Id: chrsyn.scm,v 1.3 2000/11/20 13:25:41 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-2000 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

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
	(guarantee-char-syntax-table table 'MAKE-CHAR-SYNTAX-TABLE)))))

(define (get-char-syntax table char)
  (if (not (char? char))
      (error:wrong-type-argument char "character" 'GET-CHAR-SYNTAX))
  (vector-ref (guarantee-char-syntax-table table 'GET-CHAR-SYNTAX)
	      (char->ascii char)))

(define (set-char-syntax! table char string)
  (let ((entries (guarantee-char-syntax-table table 'SET-CHAR-SYNTAX!))
	(entry (string->char-syntax string)))
    (cond ((char? char)
	   (vector-set! entries (char->ascii char) entry))
	  ((char-set? char)
	   (for-each (lambda (char)
		       (vector-set! entries (char->ascii char) entry))
		     (char-set-members char)))
	  (else
	   (error:wrong-type-argument char "character" 'SET-CHAR-SYNTAX!)))))

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
    (set! standard-char-syntax-table table)
    unspecific))

(define-primitives
  (string->char-syntax string->syntax-entry))

(define (char-syntax->string entry)
  (guarantee-char-syntax entry 'CHAR-SYNTAX->STRING)
  (let ((code (fix:and #xf entry)))
    (string-append
     (vector-ref char-syntax-codes code)
     (let ((match (fix:and #xff (fix:lsh entry -4))))
       (if (zero? match)
	   " "
	   (string (ascii->char match))))
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

(define char-syntax-codes
  '#(" " "." "w" "_" "(" ")" "'" "\"" "$" "\\" "/" "<" ">"))

(define (substring-find-next-char-of-syntax string start end table code)
  (guarantee-substring string start end 'SUBSTRING-FIND-NEXT-CHAR-OF-SYNTAX)
  (let loop ((index start))
    (and (fix:< index end)
	 (if (char=? code (char->syntax-code table (string-ref string index)))
	     index
	     (loop (fix:+ index 1))))))

(define (substring-find-next-char-not-of-syntax string start end table code)
  (guarantee-substring string start end
		       'SUBSTRING-FIND-NEXT-CHAR-NOT-OF-SYNTAX)
  (let loop ((index start))
    (and (fix:< index end)
	 (if (char=? code (char->syntax-code table (string-ref string index)))
	     (loop (fix:+ index 1))
	     index))))

(define (char->syntax-code table char)
  (string-ref (vector-ref char-syntax-codes
			  (fix:and #xf (get-char-syntax table char)))
	      0))