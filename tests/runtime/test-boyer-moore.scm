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

;;;; Tests for Boyer-Moore String Search

(declare (usual-integrations))

(define (dice-test text die-length die-skew)
  (newline)
  (let ((ok 0)
	(not-ok 0))
    (for-each
     (lambda (entry)
       (write-char #\+)
       (let ((fr (string-search-forward (car entry) text))
	     (br (string-search-backward (car entry) text))
             (all (string-search-all (car entry) text)))
	 (if (and (eqv? (cadr entry) fr)
		  (eqv? (fix:+ (last entry) die-length) br)
                  (equal? (cdr entry) all))
	     (begin
	       (set! ok (fix:+ ok 1))
	       unspecific)
	     (begin
	       (set! not-ok (fix:+ not-ok 1))
	       (write-line (list (car entry) (cdr entry) fr br))))))
     (dice-text text die-length die-skew))
    (write-line (list 'OK= ok 'NOT-OK= not-ok))))

(define (dice-text text die-length die-skew)
  (let ((end (string-length text))
	(table (make-string-hash-table)))
    (define (record! s die)
      (let ((entry
	     (or (hash-table-ref/default table die #f)
		 (let ((entry (list 'ENTRY)))
		   (hash-table-set! table die entry)
		   entry))))
	(set-cdr! entry (cons s (cdr entry)))))
    (let loop ((s 0))
      (let ((e (fix:+ s die-length)))
	(if (fix:<= e end)
	    (begin
	      (record! s (substring text s e))
	      (let ((s (fix:+ s die-skew)))
		(if (fix:< s end)
		    (loop s)))))))
    (map (lambda (entry)
	   (cons (car entry)
		 (reverse! (cddr entry))))
	 (hash-table->alist table))))

(define (file->string filename)
  (call-with-input-file filename
    (lambda (port)
      (let ((builder (string-builder))
            (buffer (make-string #x1000)))
        (let loop ()
          (let ((n (read-string! buffer port)))
            (if (> n 0)
                (begin
                  (builder (substring buffer 0 n))
                  (loop)))))
        (builder 'immutable)))))

(define (search-speed-test text die-length die-skew procedure n-repeats)
  (let ((entries (map car (dice-text text die-length die-skew))))
    (show-time
     (lambda ()
       (do ((i 0 (fix:+ i 1)))
	   ((fix:= i n-repeats))
	 (do ((entries entries (cdr entries)))
	     ((null? entries))
	   (procedure (car entries) text)))))))

(define (dummy-search pattern text)
  (if (not (string? pattern))
      (error:wrong-type-argument pattern "string" 'BM-SUBSTRING?))
  (if (not (string? text))
      (error:wrong-type-argument text "string" 'BM-SUBSTRING?))
  #f)