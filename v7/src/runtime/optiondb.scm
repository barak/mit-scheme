#| -*-Scheme-*-

$Id: optiondb.scm,v 1.3 1995/05/03 07:34:40 cph Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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
 '((FORMAT      (RUNTIME FORMAT)     (INITIALIZE-PACKAGE!) "format")
   (COMPRESS    (RUNTIME COMPRESS)   #F                    "cpress")
   (HASH-TABLE  (RUNTIME HASH-TABLE) (INITIALIZE-PACKAGE!) "hashtb")
   (RB-TREE     (RUNTIME RB-TREE)    #F                    "rbtree")
   (WT-TREE     (RUNTIME WT-TREE)    #F                    "wttree")
   (SUBPROCESS  (RUNTIME SUBPROCESS) (INITIALIZE-PACKAGE!) "process")
   (STEPPER     (RUNTIME STEPPER)    #F                    "ystep")
   (ORDERED-VECTOR (RUNTIME ORDERED-VECTOR) #F "ordvec")
   ))

(define-load-option 'DOSPROCESS
  (standard-option-loader '() #F "dosproc"))