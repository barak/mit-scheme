#| -*-Scheme-*-

$Id: optiondb.scm,v 1.17 2003/06/08 05:07:04 cph Exp $

Copyright 1994,1995,1996,1999,2000,2001 Massachusetts Institute of Technology
Copyright 2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
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
;;   $MITSCHEME_LIBRARY_PATH/options) into the environment of the
;;   package named PACKAGE-NAME, and then evaluates EXPR in that load
;;   environment. If EXPR is #F of course evaluating it has no effect.
;; 
;; (FURTHER-LOAD-OPTIONS EXPR)
;;   EXPR is the place to look next for the load options.  Useful objects
;;   are STANDARD-LOAD-OPTIONS (load options supplied with the
;;   MIT-Scheme distribution) and LOCAL-LOAD-OPTIONS (load options
;;   supplied for every user of your architecture at your site).  If
;;   not specified, or is #F, then this file is the last options
;;   database that is searched.

;; Standard load options are defined like this:

(define-load-option 'ARITHMETIC-INTERFACE
  (standard-option-loader '(RUNTIME NUMBER INTERFACE) #F "numint"))

;; We can use programming to make the definitions less noisy and tedious:

(for-each
 (lambda (spec)
   (define-load-option (car spec) (apply standard-option-loader (cdr spec))))
 '((COMPRESS	(RUNTIME COMPRESS)	#F			"cpress")
   (DOSPROCESS	()			#F			"dosproc")
   (FORMAT	(RUNTIME FORMAT)	(INITIALIZE-PACKAGE!)	"format")
   (GDBM	(RUNTIME GDBM)		(INITIALIZE-PACKAGE!)	"gdbm")
   (KRYPT	(RUNTIME KRYPT)		#F			"krypt")
   (MIME-CODEC	(RUNTIME MIME-CODEC)	#F			"mime-codec")
   (ORDERED-VECTOR (RUNTIME ORDERED-VECTOR) #F			"ordvec")
   (POSTGRESQL	(RUNTIME POSTGRESQL)	#F			"pgsql")
   (RB-TREE	(RUNTIME RB-TREE)	#F			"rbtree")
   (STEPPER	(RUNTIME STEPPER)	#F			"ystep")
   (SUBPROCESS	(RUNTIME SUBPROCESS)	(INITIALIZE-PACKAGE!)	"process")
   (SYNCHRONOUS-SUBPROCESS (RUNTIME SYNCHRONOUS-SUBPROCESS) #F	"syncproc")
   (WT-TREE	(RUNTIME WT-TREE)	#F			"wttree")
   ))

(define-load-option 'REGULAR-EXPRESSION
  (standard-option-loader '(RUNTIME REGULAR-EXPRESSION-COMPILER)
			  #F
			  "rgxcmp")
  (standard-option-loader '(RUNTIME CHAR-SYNTAX)
			  '(INITIALIZE-PACKAGE!)
			  "chrsyn")
  (standard-option-loader '(RUNTIME REGULAR-EXPRESSION)
			  '(INITIALIZE-PACKAGE!)
			  "regexp")
  (standard-option-loader '(RUNTIME REXP)
			  #F
			  "rexp"))

;; HASH-TABLE is now always loaded.
(define-load-option 'HASH-TABLE dummy-option-loader)