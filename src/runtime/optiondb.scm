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

(declare (usual-integrations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file contains Scheme code to define a set of load options.  Each
;; time a new (not-yet-loaded) load option is requested, this file is
;; loaded into a fresh environment which has the system global
;; environment as parent.
;;
;; Procedures used for defining load options:
;;
;; (DEFINE-LOAD-OPTION 'NAME loader loader ...)
;;   Defines a load option (NAME) which is loaded by the execution of its
;;   loaders.  Loaders are executed left to right.  Loaders are thunks.
;;
;; (STANDARD-OPTION-LOADER 'PACKAGE-NAME 'EXPR file file ...)
;;   Creates a loader that loads the files (strings relative to
;;   $MITSCHEME_LIBRARY_PATH/runtime/) into the environment of the
;;   package named PACKAGE-NAME, and then evaluates EXPR in that load
;;   environment. If EXPR is #F of course evaluating it has no effect.
;;
;; (FURTHER-LOAD-OPTIONS EXPR)
;;   EXPR is the place to look next for the load options.  Useful objects
;;   are #T (load options supplied in the next optiondb file in the
;;   library directory path), STANDARD-LOAD-OPTIONS (those supplied with
;;   the MIT-Scheme distribution) and LOCAL-LOAD-OPTIONS (those supplied
;;   for every user of your architecture at your site).  If not
;;   specified, or is #F, then this file is the last options database
;;   that is searched.

;; Standard load options are defined like this:

(define-load-option 'arithmetic-interface
  (standard-option-loader '(runtime number interface) #f "numint"))

;; We can use programming to make the definitions less noisy and tedious:

(for-each
 (lambda (spec)
   (define-load-option (car spec) (apply standard-option-loader (cdr spec))))
 '((compress	(runtime compress)	#f			"cpress")
   (format	(runtime format)	(initialize-package!)	"format")
   (mime-codec	(runtime mime-codec)	#f			"mime-codec")
   (ordered-vector (runtime ordered-vector) #f			"ordvec")
   (rb-tree	(runtime rb-tree)	#f			"rbtree")
   (stepper	(runtime stepper)	#f			"ystep")
   (subprocess	(runtime subprocess)	(initialize-package!)	"process")
   (synchronous-subprocess (runtime synchronous-subprocess) #f	"syncproc")
   (wt-tree	(runtime wt-tree)	#f			"wttree")
   ))

(define-load-option 'regular-expression
  (standard-option-loader '(runtime regular-expression-compiler)
			  #f
			  "rgxcmp")
  (standard-option-loader '(runtime char-syntax)
			  '(initialize-package!)
			  "chrsyn")
  (standard-option-loader '(runtime regular-expression)
			  #f
			  "regexp")
  (standard-option-loader '(runtime rexp)
			  #f
			  "rexp"))

;; HASH-TABLE is now always loaded.
(define-load-option 'hash-table dummy-option-loader)