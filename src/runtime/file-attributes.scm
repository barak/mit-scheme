#| -*- Mode: Scheme -*-

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

;;;; File attributes parser
;;; package: (runtime file-attributes)

(declare (usual-integrations))

;;; This code will parse a "file attributes line" found in the first line of a
;;; file and delimited by the special -*- sequence.

;;; The most general case is a series of key/value pairs where the key
;;; is followed by a colon and the pairs are separated or delimited by
;;; semicolons.  Whitespace is optional and cannot be relied upon to
;;; delimit the end of a key or a value.

(define (parse-file-attributes-string string)
  (parse-file-attributes (string->parser-buffer string)))

(define (parse-file-attributes parser-buffer)
  (let ((v (parse:attributes-line parser-buffer)))
    (and v
	 (filter-map (lambda (p)
		       (let ((value
			      (ignore-errors
			       (lambda ()
				 (read (open-input-string (cdr p)))))))
			 (and (not (condition? value))
			      (cons (intern (car p))
				    value))))
		     (vector-ref v 0)))))

(define parse:attributes-line
  (*parser
   (encapsulate vector->list
     (seq (noise (* (alt (char-set not-hyphen)
			 (seq #\- (char-set not-asterisk))
			 (seq #\- #\* (char-set not-hyphen)))))
	  (noise match:leader/trailer)
          (alt (seq parse:key/value-pair
                    (* (seq ";"
                            parse:key/value-pair))
                    (? (seq ";"
                            (noise (* (char-set char-set:whitespace))))))
               (encapsulate (lambda (v)
                              (cons "mode" (vector-ref v 0)))
		 (seq (match (+ (char-set name-chars)))
		      (noise (* (char-set char-set:whitespace))))))
          (noise match:leader/trailer)))))

(define match:leader/trailer
  (*matcher (seq "-*-" (* "*-"))))

(define parse:key/value-pair
  (*parser
   (encapsulate (lambda (v)
		  (cons (vector-ref v 0)
			(string-trim (vector-ref v 1))))
     (seq (noise (* (char-set char-set:whitespace)))
	  (match (+ (char-set name-chars)))
	  (noise (* (char-set char-set:whitespace)))
	  ":"
	  (match match:value)))))

(define match:value
  (*matcher
   (+ (alt (char-set value-chars)
	   (seq #\- (char-set not-asterisk))
	   (seq #\- #\* (char-set not-hyphen))
	   (seq #\\ (char-set char-set:unicode))
	   (seq #\"
		(* (alt (char-set string-chars)
			(seq #\\ (char-set char-set:unicode))))
		#\")))))

(define-deferred name-chars
  (char-set-difference char-set:symbol-constituent (char-set #\:)))

(define-deferred value-chars
  (char-set-difference char-set:unicode (char-set #\; #\" #\\ #\- #\*)))

(define-deferred not-hyphen
  (char-set-difference char-set:unicode (char-set #\-)))

(define-deferred not-asterisk
  (char-set-difference char-set:unicode (char-set #\*)))

(define-deferred string-chars
  (char-set-difference char-set:unicode (char-set #\" #\\)))