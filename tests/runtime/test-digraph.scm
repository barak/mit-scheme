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

;;;; Tests for directed graphs

(declare (usual-integrations))

(define (entries->digraph entries)
  (make-digraph (map car entries)
                (lambda (vertex)
                  (cdr (assv vertex entries)))))

(define-test 'topological-sort
  (lambda ()
    (define graph
      (entries->digraph
       '((belt jacket)
         (jacket)
         (pants belt shoes)
         (shirt belt tie)
         (shoes)
         (socks shoes)
         (tie jacket)
         (undershorts pants shoes)
         (watch))))

    (define ordering
      (graph 'topological-sort))

    (assert-eqv (length (graph 'vertices)) (length ordering))
    (assert-lset= eq? (graph 'vertices) ordering)
    (let loop ((ordering ordering))
      (if (pair? ordering)
          (let ((vertex (car ordering))
                (remaining (cdr ordering)))
            (for-each (lambda (neighbor)
                        (assert-true (memq neighbor remaining)))
                      (graph 'neighbors-of vertex))
            (loop remaining))))))

(define-test 'strong-components
  (lambda ()
    (define graph
      (entries->digraph
       '((a b c)
         (b c d)
         (c)
         (d c e)
         (e b f)
         (f c d))))

    (define expected
      (let ((n1 '(a))
            (n2 '(b d e f))
            (n3 '(c)))
        (list (list n1 n2 n3)
              (list n2 n3)
              (list n3))))

    (define actual
      (graph 'strong-components))

    (assert-eqv 3 (length (actual 'vertices)))
    (assert-lset= (let ((node=
                         (lambda (n1 n2)
                           (lset= eq? n1 n2))))
                    (lambda (a b)
                      (and (node= (car a) (car b))
                           (lset= node= (cdr a) (cdr b)))))
                  expected
                  (map (lambda (v)
                         (cons v (actual 'neighbors-of v)))
                       (actual 'vertices)))))