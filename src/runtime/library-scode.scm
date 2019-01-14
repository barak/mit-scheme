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

;;;; R7RS libraries: SCode representation
;;; package: (runtime library scode)

(declare (usual-integrations))

(define (make-scode-library metadata contents)
  (make-scode-declaration `((target-metadata ,metadata))
			  (make-scode-quotation contents)))

(define (scode-library? object)
  (and (scode-declaration? object)
       (let ((text (scode-declaration-text object)))
	 (and (singleton-list? text)
	      (target-metadata? (car text))
	      (let ((metadata-values (metadata-elt-values (car text))))
		(and (singleton-list? metadata-values)
		     (scode-library-metadata? (car metadata-values))))))
       (scode-quotation? (scode-declaration-expression object))))

(define (scode-library-metadata library)
  (car (metadata-elt-values (car (scode-declaration-text library)))))

(define (scode-library-contents library)
  (scode-quotation-expression (scode-declaration-expression library)))

(define (map-scode-library procedure library)
  (make-scode-library (scode-library-metadata library)
		      (procedure (scode-library-contents library))))

(define (scode-library-property keyword library)
  (metadata-elt-values
   (find (lambda (metadata)
	   (eq? (metadata-elt-keyword metadata) keyword))
	 (metadata-elt-values (scode-library-metadata library)))))

(define (scode-library-name library)
  (car (scode-library-property 'name library)))

(define (scode-library-imports library)
  (map list->library-import (scode-library-property 'imports library)))

(define (scode-library-imports-used library)
  (map list->library-import (scode-library-property 'imports-used library)))

(define (scode-library-exports library)
  (map list->library-export (scode-library-property 'exports library)))

(define (singleton-list? object)
  (and (pair? object)
       (null? (cdr object))))

(define (specific-metadata-predicate keyword)
  (lambda (object)
    (and (metadata-elt? object)
	 (eq? (metadata-elt-keyword object) keyword)
	 (every metadata-elt? (metadata-elt-values object)))))

(define target-metadata? (specific-metadata-predicate 'target-metadata))
(define scode-library-metadata? (specific-metadata-predicate 'scode-library))

(define (metadata-elt? object)
  (and (pair? object)
       (symbol? (car object))
       (list? (cdr object))))
(register-predicate! metadata-elt? 'metadata-elt)

(define (metadata-elt-keyword elt)
  (guarantee metadata-elt? elt 'metadata-elt-keyword)
  (car elt))

(define (metadata-elt-values elt)
  (guarantee metadata-elt? elt 'metadata-elt-values)
  (cdr elt))

(define (library->scode-library library)
  (make-scode-library
   `(scode-library
     (name ,(library-name library))
     (imports ,@(map library-import->list (library-imports library)))
     (imports-used ,@(map library-import->list (library-imports-used library)))
     (exports ,@(map library-export->list (library-exports library))))
   (library-contents library)))

(define (scode-library->library library filename)
  (guarantee scode-library? library 'scode-library->library)
  (make-library (scode-library-name library)
		'imports (scode-library-imports library)
		'imports-used (scode-library-imports-used library)
		'exports (scode-library-exports library)
		'contents (scode-library-contents library)
		'filename filename))

(define (make-r7rs-scode-file libraries)
  (guarantee-list-of scode-library? libraries 'make-r7rs-scode-file)
  (make-scode-sequence libraries))

(define (r7rs-scode-file? scode)
  (let ((scode (strip-comments scode)))
    (or (scode-library? scode)
	(and (scode-sequence? scode)
	     (every scode-library? (scode-sequence-actions scode))))))
(register-predicate! r7rs-scode-file? 'r7rs-scode-file)

(define (r7rs-scode-file-libraries scode)
  (let ((scode (strip-comments scode)))
    (if (scode-library? scode)
	(list scode)
	(scode-sequence-actions scode))))

(define (strip-comments object)
  (if (and (scode-comment? object)
	   (not (scode-declaration? object)))
      (strip-comments (scode-comment-expression object))
      object))

;; Unlike map, guarantees that procedure is called on the libraries in order.
(define (map-r7rs-scode-file procedure scode)
  (guarantee r7rs-scode-file? scode 'map-r7rs-scode-file)
  (let loop ((libraries (r7rs-scode-file-libraries scode)) (results '()))
    (if (pair? libraries)
	(loop (cdr libraries)
	      (cons (procedure (car libraries)) results))
	(make-scode-sequence (reverse results)))))