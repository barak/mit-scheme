#| -*-Scheme-*-

$Id$

Copyright (c) 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Test code for Boyer-Moore String Search

(declare (usual-integrations))

(define (dice-test text die-length die-skew)
  (newline)
  (let ((ok 0)
	(not-ok 0))
    (for-each
     (lambda (entry)
       (write-char #\+)
       (let ((fr (string-search-forward text (car entry)))
	     (br (string-search-backward text (car entry))))
	 (if (and (eqv? (cadr entry) fr)
		  (eqv? (fix:+ (car (last-pair entry)) die-length) br))
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
	     (or (hash-table/get table die #f)
		 (let ((entry (list 'ENTRY)))
		   (hash-table/put! table die entry)
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
      ((port/operation port 'REST->STRING) port))))

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