#| -*-Scheme-*-

$Id: load.scm,v 1.1 1994/11/19 02:04:29 adams Exp $

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

;;;; Load script

(declare (usual-integrations))

(define (reload file)
  (fluid-let ((syntaxer/default-environment (nearest-repl/environment)))
    (load-latest file)))

(define (loadup)
  (load-option 'HASH-TABLE)
  (load "synutl")
  (fluid-let ((syntaxer/default-environment (nearest-repl/environment)))
    (load "midend")			; top level
    (load "utils")
    (load "fakeprim")			; pseudo primitives
    (load "dbgstr")
    (load "inlate")
    (load "envconv")
    (load "expand")
    (load "assconv")
    (load "cleanup")
    (load "earlyrew")
    (load "lamlift")
    (load "closconv")
    ;; (load "staticfy")		; broken, for now
    (load "applicat")
    (load "simplify")
    (load "cpsconv")
    (load "laterew")
    (load "compat")			; compatibility with current code
    (load "stackopt")
    (load "indexify")
    (load "rtlgen")
    ;; The following are not necessary for execution
    (load "debug")
    (load "triveval")))

(define (load.scm:init)
  (if (not (environment-bound? (nearest-repl/environment) 'execute))
      (load/push-hook! loadup)))