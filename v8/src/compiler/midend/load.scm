#| -*-Scheme-*-

$Id: b794230b88055814f505a76bbc9564b51d54f096 $

Copyright (c) 1994, 1999 Massachusetts Institute of Technology

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