#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Code to install syntax keywords in global environment
;;; package: (runtime syntax definitions)

(declare (usual-integrations))

(add-boot-init!
 (lambda ()

   (define (define-classifier name classifier)
     (environment-define-macro system-global-environment
			       name
			       (classifier-item classifier)))

   (define-classifier 'begin classifier:begin)
   (define-classifier 'declare classifier:declare)
   (define-classifier 'define-syntax classifier:define-syntax)
   (define-classifier 'er-macro-transformer classifier:er-macro-transformer)
   (define-classifier 'if classifier:if)
   (define-classifier 'let-syntax classifier:let-syntax)
   (define-classifier 'letrec-syntax classifier:letrec-syntax)
   (define-classifier 'or classifier:or)
   (define-classifier 'quote classifier:quote)
   (define-classifier 'quote-identifier classifier:quote-identifier)
   (define-classifier 'rsc-macro-transformer classifier:rsc-macro-transformer)
   (define-classifier 'sc-macro-transformer classifier:sc-macro-transformer)
   (define-classifier 'set! classifier:set!)
   (define-classifier 'the-environment classifier:the-environment)
   (define-classifier 'delay classifier:delay)
   (define-classifier 'lambda classifier:lambda)
   (define-classifier 'named-lambda classifier:named-lambda)))