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

;;;; Simple trie implementation
;;; package: (runtime trie)

(declare (usual-integrations))

(define-record-type <trie>
    (%make-trie =? value edge-alist)
    trie?
  (=? trie-=?)
  (value %trie-value set-trie-value!)
  (edge-alist %trie-edge-alist %set-trie-edge-alist!))

(define (make-trie #!optional =?)
  (%make-trie (if (default-object? =?) equal? =?)
              the-unset-value
              '()))

(define (trie-has-value? trie)
  (not (eq? the-unset-value (%trie-value trie))))

(define (trie-value trie)
  (let ((value (%trie-value trie)))
    (if (eq? the-unset-value value)
        (error:bad-range-argument trie 'trie-value))
    value))

(define the-unset-value
  (list 'the-unset-value))

(define-print-method trie?
  (standard-print-method 'trie
    (lambda (trie)
      (if (trie-has-value? trie)
          (list (trie-value trie))
          '()))))

(define (trie-find trie path)
  (let loop ((path path) (trie trie))
    (if (null-list? path 'trie-find)
        trie
        (let ((trie* (%trie-find trie (car path))))
          (and trie*
               (loop (cdr path) trie*))))))

(define (trie-intern! trie path)
  (let loop ((path path) (trie trie))
    (if (null-list? path 'trie-intern!)
        trie
        (loop (cdr path)
              (%trie-intern! trie (car path))))))

(define (%trie-find trie key)
  (let ((p (assoc key (%trie-edge-alist trie) (trie-=? trie))))
    (and p
         (cdr p))))

(define (%trie-intern! trie key)
  (or (%trie-find trie key)
      (let ((trie* (make-trie (trie-=? trie))))
        (%set-trie-edge-alist! trie
                               (append! (%trie-edge-alist trie)
                                        (list (cons key trie*))))
        trie*)))

(define (trie-values trie)
  (let ((tail
         (append-map (lambda (p)
                       (trie-values (cdr p)))
                     (%trie-edge-alist trie))))
    (if (trie-has-value? trie)
        (cons (trie-value trie) tail)
        tail)))

(define (trie->alist trie)
  (let loop ((trie trie) (path '()))
    (let ((tail
           (append-map (lambda (p)
                         (loop (cdr p)
                               (cons (car p) path)))
                       (%trie-edge-alist trie))))
      (if (trie-has-value? trie)
          (cons (cons (reverse path) (trie-value trie)) tail)
          tail))))

(define (alist->trie alist #!optional =?)
  (guarantee alist? alist 'alist->trie)
  (let ((trie (make-trie =?)))
    (for-each (lambda (p)
                (set-trie-value! (trie-intern! trie (car p)) (cdr p)))
              alist)
    trie))