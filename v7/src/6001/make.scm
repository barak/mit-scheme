#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/6001/make.scm,v 15.8 1992/08/18 22:13:27 cph Exp $

Copyright (c) 1991-92 Massachusetts Institute of Technology

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

;;;; 6.001: System Construction

(declare (usual-integrations))

(package/system-loader "6001" '() 'QUERY)
((access initialize-package! (->environment '(student scode-rewriting))))
(add-system! (make-system "6.001" 15 6 '()))

(set! repl:allow-restart-notifications? false)
(set! repl:write-result-hash-numbers? false)
(set! *unparse-disambiguate-null-as-itself?* false)
(set! *unparse-compound-procedure-names?* false)
(set! *pp-default-as-code?* true)
(set! *pp-named-lambda->define?* 'LAMBDA)
(set! x-graphics:auto-raise? true)
(set! (access write-result:undefined-value-is-special?
	      (->environment '(runtime user-interface)))
      false)

(in-package (->environment '(edwin))
  (set! editor-can-exit? false)
  (set! scheme-can-quit? false)
  (set! paranoid-exit? true)
  (set! x-screen-auto-raise true)
  (set-variable-value! edwin-variable$enable-transcript-buffer true)
  (set-variable-value! edwin-variable$evaluate-in-inferior-repl true)
  (set-variable-value! edwin-variable$repl-error-decision true)
  (set-variable-value! version-control true)
  (set-variable-value! trim-versions-without-asking true)
  (set-variable-value!
   mail-default-reply-to
   (lambda ()
     (let ((reply-to
	    (prompt-for-string "Please enter an email address for replies")))
       (set-variable-value! mail-default-reply-to reply-to)
       reply-to))))

(ge '(student))