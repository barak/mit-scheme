#| -*-Scheme-*-

$Id: comhst.scm,v 1.1 1992/09/23 23:03:31 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
MIT in each case.

NOTE: Parts of this program (Edwin) were created by translation from
corresponding parts of GNU Emacs.  Users should be aware that the GNU
GENERAL PUBLIC LICENSE may apply to these parts.  A copy of that
license should have been included along with this file. |#

;;;; Command interpreter history
;;; Translated from "comint.el", by Olin Shivers.

(declare (usual-integrations))

(define-variable-per-buffer comint-input-ring-size
  "Size of input history ring."
  30)

(define-variable comint-input-ring "" false)

(define comint-input-ring-tag
  '(COMINT-INPUT-RING))

(define-command comint-previous-input
  "Cycle backwards through input history."
  "*p"
  (lambda (argument)
    (let ((point (current-point))
	  (ring (ref-variable comint-input-ring)))
      (let ((size (+ (ring-size ring) 1)))
	(let ((index
	       (modulo (+ argument
			  (command-message-receive comint-input-ring-tag
			    (lambda (index)
			      (delete-string (current-mark) point)
			      index)
			    (lambda ()
			      (push-current-mark! point)
			      (cond ((positive? argument) 0)
				    ((negative? argument) 2)
				    (else 1)))))
		       size)))
	  (message (number->string index))
	  (if (positive? index)
	      (insert-string (ring-ref ring (- index 1)) point))
	  (set-command-message! comint-input-ring-tag index))))))
	 
(define-command comint-next-input
  "Cycle forwards through input history."
  "*p"
  (lambda (argument)
    ((ref-command comint-previous-input) (- argument))))

(define-variable comint-last-input-match "" false)

(define-command comint-history-search-backward
  "Search backwards through the input history for a matching substring."
  (lambda ()
    (list (prompt-for-string "History search backward"
			     (ref-variable comint-last-input-match))))
  (lambda (string)
    (comint-history-search string true)))

(define-command comint-history-search-forward
  "Search forwards through the input history for a matching substring."
  (lambda ()
    (list (prompt-for-string "History search forward"
			     (ref-variable comint-last-input-match))))
  (lambda (string)
    (comint-history-search string false)))

(define (comint-history-search string backward?)
  (let ((ring (ref-variable comint-input-ring))
	(syntax-table (ref-variable syntax-table))
	(pattern (re-compile-pattern (re-quote-string string) false)))
    (let ((size (+ (ring-size ring) 1)))
      (let ((start
	     (command-message-receive comint-input-ring-tag
	       (lambda (index) index)
	       (lambda () (if backward? 0 size)))))
	(let loop ((index start))
	  (let ((index (+ index (if backward? 1 -1))))
	    (cond ((if backward? (>= index size) (< index 0))
		   (set-command-message! comint-input-ring-tag start)
		   (editor-failure "Not found"))
		  ((re-search-string-forward pattern
					     false
					     syntax-table
					     (ring-ref ring (- index 1)))
		   (set-variable! comint-last-input-match string)
		   ((ref-command comint-previous-input) (- index start)))
		  (else
		   (loop index)))))))))