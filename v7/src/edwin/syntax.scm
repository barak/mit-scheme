;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Syntax tables for Edwin

(declare (usual-integrations))
(using-syntax edwin-syntax-table
(let-syntax ((make-primitive (macro (name) (make-primitive-procedure name))))

;;;; Syntax Tables

(define-variable "Syntax Table"
  "The syntax-table used for word and list parsing.")

(define-variable "Syntax Ignore Comments Backwards"
  "If true, ignore comments in backwards expression parsing.
This should be false for comments that end in Newline, like Lisp.
It can be true for comments that end in }, like Pascal.
This is because Newline occurs often when it doesn't
indicate a comment ending."
  #!FALSE)

(define make-syntax-table)
(define syntax-table?)
(define syntax-table-copy vector-copy)
(define modify-syntax-entry!)
(define modify-syntax-entries!)
(let ()

(define standard-syntax-table)
(define key-type)

(define string->syntax-entry
  (make-primitive string->syntax-entry))

(set! make-syntax-table
(named-lambda (make-syntax-table)
  (vector-copy standard-syntax-table)))

;;; **** Fucking compiler miscompiles PRIMITIVE-TYPE? here,
;;; so flush this randomness for now.
;(set! syntax-table?
;(named-lambda (syntax-table? object)
;  (and (vector? object)
;       (= 256 (vector-length object))
;       (primitive-type? key-type (vector-ref object 0)))))

(set! modify-syntax-entry!
(named-lambda (modify-syntax-entry! syntax-table char string)
;  (if (not (syntax-table? syntax-table))
;      (error "Not a syntax table" syntax-table))
  (vector-set! syntax-table (char->ascii char) (string->syntax-entry string))))

(set! modify-syntax-entries!
(named-lambda (modify-syntax-entries! syntax-table cl ch string)
;  (if (not (syntax-table? syntax-table))
;      (error "Not a syntax table" syntax-table))
  (let ((ah (char->ascii ch))
	(entry (string->syntax-entry string)))
    (define (loop a)
      (vector-set! syntax-table a entry)
      (if (< a ah) (loop (1+ a))))
    (loop (char->ascii cl)))))

(let ((entry (string->syntax-entry "")))
  (set! key-type (primitive-type entry))
  (let ((table (vector-cons 256 entry)))
    (modify-syntax-entries! table #\0 #\9 "w")
    (modify-syntax-entries! table #\A #\Z "w")
    (modify-syntax-entries! table #\a #\z "w")
    (modify-syntax-entry! table #\$ "w")
    (modify-syntax-entry! table #\% "w")
    (modify-syntax-entry! table #\( "()")
    (modify-syntax-entry! table #\) ")(")
    (modify-syntax-entry! table #\[ "(]")
    (modify-syntax-entry! table #\] ")[")
    (modify-syntax-entry! table #\{ "(}")
    (modify-syntax-entry! table #\} "){")
    (modify-syntax-entry! table #\" "\"")
    (modify-syntax-entry! table #\\ "\\")
    (for-each (lambda (char)
		(modify-syntax-entry! table char "_"))
	      (string->list "_-+*/&|<>="))
    (for-each (lambda (char)
		(modify-syntax-entry! table char "."))
	      (string->list ".,;:?!#@~^'`"))
    (set! standard-syntax-table table)
    (set-variable! "Syntax Table" table)))

;; **** compiler complains about assignment to unassigned variable for
;; value unless this is here.
'DONE
)

;;;; Word Parsing

(define forward-word)
(define backward-word)
(define forward-to-word)
(let ()

(set! forward-word
(named-lambda (forward-word mark n #!optional limit?)
  (if (unassigned? limit?) (set! limit? #!FALSE))
  (cond ((positive? n) (%forward-word mark n limit?))
	((negative? n) (%backward-word mark (- n) limit?))
	(else mark))))

(set! backward-word
(named-lambda (backward-word mark n #!optional limit?)
  (if (unassigned? limit?) (set! limit? #!FALSE))
  (cond ((positive? n) (%backward-word mark n limit?))
	((negative? n) (%forward-word mark (- n) limit?))
	(else mark))))

(set! forward-to-word
(named-lambda (forward-to-word mark #!optional limit?)
  (if (unassigned? limit?) (set! limit? #!FALSE))
  (let ((index (scan-forward-to-word (ref-variable "Syntax Table")
				     (mark-group mark)
				     (mark-index mark)
				     (mark-index (group-end mark)))))
    (if (not index)
	(limit-mark-motion limit? (group-end mark))
	(make-mark (mark-group mark) index)))))

(define (%forward-word mark n limit?)
  (let ((group (mark-group mark))
	(end (mark-index (group-end mark))))
    (define (loop start n)
      (let ((m (scan-word-forward (ref-variable "Syntax Table")
				  group start end)))
	(cond ((not m) (limit-mark-motion limit? (make-mark group start)))
	      ((= n 1) (make-mark group m))
	      (else (loop m (-1+ n))))))
    (loop (mark-index mark) n)))

(define (%backward-word mark n limit?)
  (let ((group (mark-group mark))
	(end (mark-index (group-start mark))))
    (define (loop start n)
      (let ((m (scan-word-backward (ref-variable "Syntax Table")
				   group start end)))
	(cond ((not m) (limit-mark-motion limit? (make-mark group start)))
	      ((= n 1) (make-mark group m))
	      (else (loop m (-1+ n))))))
    (loop (mark-index mark) n)))

(define scan-word-forward
  (make-primitive scan-word-forward))

(define scan-forward-to-word
  (make-primitive scan-forward-to-word))

(define scan-word-backward
  (make-primitive scan-word-backward))

;; **** compiler complains about assignment to unassigned variable for
;; value unless this is here.
'DONE
)

;;;; Lisp Parsing

(define forward-one-sexp)
(define backward-one-sexp)
(define backward-prefix-chars)
(define forward-one-list)
(define backward-one-list)
(define forward-up-one-list)
(define backward-up-one-list)
(define forward-down-one-list)
(define backward-down-one-list)
(define mark-right-char-quoted?)
(let ()

(set! forward-one-sexp
(named-lambda (forward-one-sexp start #!optional end)
  (cond ((unassigned? end) (set! end (group-end start)))
	((not (mark<= start end)) (error "END less than START" end)))
  (%forward-list start end 0 #!TRUE)))

(set! backward-one-sexp
(named-lambda (backward-one-sexp start #!optional end)
  (cond ((unassigned? end) (set! end (group-start start)))
	((not (mark>= start end)) (error "END greater than START" end)))
  (let ((mark (%backward-list start end 0 #!TRUE)))
    (and mark (backward-prefix-chars mark end)))))

(set! backward-prefix-chars
(named-lambda (backward-prefix-chars start #!optional end)
  (cond ((unassigned? end) (set! end (group-start start)))
	((not (mark>= start end)) (error "END greater than START" end)))
  (make-mark (mark-group start)
	     (scan-backward-prefix-chars (ref-variable "Syntax Table")
					 (mark-group start)
					 (mark-index start)
					 (mark-index end)))))

(set! forward-one-list
(named-lambda (forward-one-list start #!optional end)
  (cond ((unassigned? end) (set! end (group-end start)))
	((not (mark<= start end)) (error "END less than START" end)))
  (%forward-list start end 0 #!FALSE)))

(set! backward-one-list
(named-lambda (backward-one-list start #!optional end)
  (cond ((unassigned? end) (set! end (group-start start)))
	((not (mark>= start end)) (error "END greater than START" end)))
  (%backward-list start end 0 #!FALSE)))

(set! forward-up-one-list
(named-lambda (forward-up-one-list start #!optional end)
  (cond ((unassigned? end) (set! end (group-end start)))
	((not (mark<= start end)) (error "END less than START" end)))
  (%forward-list start end 1 #!FALSE)))

(set! backward-up-one-list
(named-lambda (backward-up-one-list start #!optional end)
  (cond ((unassigned? end) (set! end (group-start start)))
	((not (mark>= start end)) (error "END greater than START" end)))
  (%backward-list start end 1 #!FALSE)))

(set! forward-down-one-list
(named-lambda (forward-down-one-list start #!optional end)
  (cond ((unassigned? end) (set! end (group-end start)))
	((not (mark<= start end)) (error "END less than START" end)))
  (%forward-list start end -1 #!FALSE)))

(set! backward-down-one-list
(named-lambda (backward-down-one-list start #!optional end)
  (cond ((unassigned? end) (set! end (group-start start)))
	((not (mark>= start end)) (error "END greater than START" end)))
  (%backward-list start end -1 #!FALSE)))

(set! mark-right-char-quoted?
(named-lambda (mark-right-char-quoted? mark)
  (quoted-char? (ref-variable "Syntax Table")
		(mark-group mark)
		(mark-index mark)
		(group-start-index (mark-group mark)))))

(define (%forward-list start end depth sexp?)
  (let ((index (scan-list-forward (ref-variable "Syntax Table")
				  (mark-group start)
				  (mark-index start) (mark-index end)
				  depth sexp? #!TRUE)))
    (and index (make-mark (mark-group start) index))))

(define (%backward-list start end depth sexp?)
  (let ((index (scan-list-backward (ref-variable "Syntax Table")
				   (mark-group start)
				   (mark-index start) (mark-index end)
				   depth sexp?
				   (ref-variable
				    "Syntax Ignore Comments Backwards"))))
    (and index (make-mark (mark-group start) index))))

(define scan-list-forward
  (make-primitive scan-list-forward))

(define scan-list-backward
  (make-primitive scan-list-backward))

(define scan-backward-prefix-chars
  (make-primitive scan-backward-prefix-chars))

(define quoted-char?
  (make-primitive quoted-char?))

;; **** compiler complains about assignment to unassigned variable for
;; value unless this is here.
'DONE
)

(define (mark-left-char-quoted? mark)
  (if (not (group-start? mark))
      (mark-right-char-quoted? (mark-1+ mark))
      (error "Mark has no left char" mark)))

(define (parse-state-depth state)
  (vector-ref state 0))

(define (parse-state-in-string? state)	;#!FALSE or ASCII delimiter.
  (vector-ref state 1))

(define (parse-state-in-comment? state)	;#!FALSE or 1 or 2.
  (vector-ref state 2))

(define (parse-state-quoted? state)
  (vector-ref state 3))

(define (parse-state-last-sexp state)
  (vector-ref state 4))
(define (set-parse-state-last-sexp! state value)
  (vector-set! state 4 value))

(define (parse-state-containing-sexp state)
  (vector-ref state 5))
(define (set-parse-state-containing-sexp! state value)
  (vector-set! state 5 value))

(define (parse-state-location state)
  (vector-ref state 6))
(define (set-parse-state-location! state value)
  (vector-set! state 6 value))

(define (forward-to-sexp-start mark end)
  (parse-state-location (parse-partial-sexp mark end 0 #!TRUE)))

(define parse-partial-sexp)
(define char->syntax-code)
(let ()

(set! parse-partial-sexp
(named-lambda (parse-partial-sexp start end #!optional
				  target-depth stop-before? old-state)
  (if (or (unassigned? target-depth) (not target-depth))
      (set! target-depth -1000000))
  (if (unassigned? stop-before?) (set! stop-before? #!FALSE))
  (if (unassigned? old-state) (set! old-state #!FALSE))
  (if (not (mark<= start end)) (error "Marks incorrectly related" start end))
  (let ((group (mark-group start)))
    (let ((state (scan-sexps-forward (ref-variable "Syntax Table")
				     group
				     (mark-index start)
				     (mark-index end)
				     target-depth stop-before? old-state)))
      ;; Convert the returned indices to marks.
      (if (parse-state-last-sexp state)
	  (set-parse-state-last-sexp! 
	   state 
	   (make-mark group (parse-state-last-sexp state))))
      (if (parse-state-containing-sexp state)
	  (set-parse-state-containing-sexp! 
	   state 
	   (make-mark group (parse-state-containing-sexp state))))
      (set-parse-state-location! 
       state
       (make-mark group (parse-state-location state)))
      state))))

(set! char->syntax-code
(named-lambda (char->syntax-code char)
  (%char->syntax-code (ref-variable "Syntax Table") char)))

(define scan-sexps-forward
  (make-primitive scan-sexps-forward))

(define %char->syntax-code
  (make-primitive char->syntax-code))

;; **** compiler complains about assignment to unassigned variable for
;; value unless this is here.
'DONE
)


;;;; Definition Start/End

(define-variable "Definition Start"
  "Regexp to match start of a definition."
  "^\\s(")

(define (definition-start? mark)
  (re-match-forward (ref-variable "Definition Start") mark))

(define (forward-one-definition-start mark)
  (and (re-search-forward (ref-variable "Definition Start")
			  (if (line-start? mark) (line-end mark 0) mark))
       (re-match-start 0)))

(define (backward-one-definition-start mark)
  (re-search-backward (ref-variable "Definition Start") mark))

(define (forward-one-definition-end mark)
  (define (loop start)
    (let ((end (forward-one-list start)))
      (and end
	   (let ((end*
		  (let ((end (horizontal-space-end end)))
		    (if (re-match-forward "[;\n]" end)
			(line-start end 1 'LIMIT)
			end))))
	     (if (mark> end* mark)
		 end*
		 (loop (forward-one-definition-start end)))))))
  (and (not (group-end? mark))
       (loop 
	(or (backward-one-definition-start (mark1+ mark))
	    (forward-one-definition-start (group-start mark))))))

(define (backward-one-definition-end mark)
  (let ((start (backward-one-definition-start mark)))
    (and start
	 (let ((end (forward-one-definition-end start)))
	   (and end
		(if (mark< end mark)
		    end
		    (let ((start (backward-one-definition-start start)))
		      (and start (forward-one-definition-end start)))))))))

;;; end USING-SYNTAX
))

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
