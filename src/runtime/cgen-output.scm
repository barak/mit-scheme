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

;;;; Code generation library: Scheme generation
;;; package: (runtime cgen output)

(declare (usual-integrations))

(add-boot-deps! '(runtime cgen))

(define (cgen-expr->scheme rename compare expr)
  (optimize-output rename compare
    (expr-map (lambda (expr path)
                (declare (ignore path))
                expr)
              expr
              (lambda (keyword args)
                (case keyword
                  ((call) args)
                  ((pcall) `(,(rename (car args)) ,@(cdr args)))
                  ((rename) (rename (car args)))
                  (else `(,(rename keyword) ,@args)))))))

(define (optimize-output rename compare expr)
  (let ((apply-rules
         (cgen-rules-applier output-optimizer-rules
                             (lambda (result) (cons rename result))
                             (lambda (cid uid) (compare (rename cid) uid)))))
    (let loop ((expr expr))
      (if (non-empty-list? expr)
          (apply-rules (map loop expr))
          expr))))

(define-deferred output-optimizer-rules
  (make-cgen-rule-set

   (define-cgen-rule `((lambda ((?? names)) (? body)) (?? vals))
     (lambda (rename names body vals)
       `(,(rename 'let)
         ,(map list names vals)
         ,body)))

   (define-cgen-rule `(if (? p) #t #f)
     (lambda (rename p)
       (declare (ignore rename))
       p))

   (define-cgen-rule `(if (? p) (? c) #f)
     (lambda (rename p c)
       `(,(rename 'and) ,p ,c)))

   (define-cgen-rule `(and)
     (lambda (rename)
       (declare (ignore rename))
       '#t))

   (define-cgen-rule `(and (? a))
     (lambda (rename a)
       (declare (ignore rename))
       a))

   (define-cgen-rule `(and (?? a) (and (?? b)) (?? c))
     (lambda (rename a b c)
       `(,(rename 'and) ,@a ,@b ,@c)))

   (define-cgen-rule `(and (?? a) #t (?? b))
     (lambda (rename a b)
       `(,(rename 'and) ,@a ,@b)))

   (define-cgen-rule `(let (((? name) (? p)))
                        (if (? name) (? name) (? a)))
     (lambda (rename name p a)
       (declare (ignore name))
       `(,(rename 'or) ,p ,a)))

   (define-cgen-rule `(or)
     (lambda (rename)
       (declare (ignore rename))
       '#f))

   (define-cgen-rule `(or (? a))
     (lambda (rename a)
       (declare (ignore rename))
       a))

   (define-cgen-rule `(or (?? a) (or (?? b)) (?? c))
     (lambda (rename a b c)
       `(,(rename 'or) ,@a ,@b ,@c)))

   (define-cgen-rule `(or (?? a) #f (?? b))
     (lambda (rename a b)
       `(,(rename 'or) ,@a ,@b)))

   ))