#| -*-Scheme-*-

$Id$

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

;;;; LAP Generator
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Constants

(define *next-constant*)
(define *interned-constants*)
(define *interned-variables*)
(define *interned-assignments*)
(define *interned-uuo-links*)
(define *interned-global-links*)
(define *interned-static-variables*)
(define *block-profiles*)

(define (allocate-named-label prefix)
  (let ((label
	 (string->uninterned-symbol
	  (string-append prefix (number->string *next-constant*)))))
    (set! *next-constant* (1+ *next-constant*))
    label))

(define (allocate-constant-label)
  (allocate-named-label "CONSTANT-"))

(define (warning-assoc obj pairs)
  (define (local-eqv? obj1 obj2)
    (or (eqv? obj1 obj2)
	(and (string? obj1)
	     (string? obj2)
	     (zero? (string-length obj1))
	     (zero? (string-length obj2)))))

  (let ((pair (assoc obj pairs)))
    (if (and compiler:coalescing-constant-warnings?
	     (pair? pair)
	     (not (local-eqv? obj (car pair))))
	(warn "Coalescing two copies of constant object" obj))
    pair))

(define-integrable (object->label find read write allocate-label)
  (lambda (object)
    (let ((entry (find object (read))))
      (if entry
	  (cdr entry)
	  (let ((label (allocate-label object)))
	    (write (cons (cons object label)
			 (read)))
	    label)))))

(let-syntax ((->label
	      (macro (find var #!optional suffix)
		`(object->label ,find
				(lambda () ,var)
				(lambda (new)
				  (declare (integrate new))
				  (set! ,var new))
				,(if (default-object? suffix)
				     `(lambda (object)
					object ; ignore
					(allocate-named-label "OBJECT-"))
				     `(lambda (object)
					(allocate-named-label
					 (string-append (symbol->string object)
							,suffix))))))))
(define constant->label
  (->label warning-assoc *interned-constants*))

(define free-reference-label
  (->label assq *interned-variables* "-READ-CELL-"))

(define free-assignment-label
  (->label assq *interned-assignments* "-WRITE-CELL-"))

(define free-static-label
  (->label assq *interned-static-variables* "-HOME-"))

;; End of let-syntax
)

;; These are different because different uuo-links are used for different
;; numbers of arguments.

(define (allocate-uuo-link-label prefix name frame-size)
  (allocate-named-label
   (string-append prefix
		  (symbol->string name)
		  "-"
		  (number->string (-1+ frame-size))
		  "-ARGS-")))

(define-integrable (uuo-link-label read write! prefix)
  (lambda (name frame-size)
    (let* ((all (read))
	   (entry (assq name all)))
      (if entry
	  (let ((place (assv frame-size (cdr entry))))
	    (if place
		(cdr place)
		(let ((label (allocate-uuo-link-label prefix name frame-size)))
		  (set-cdr! entry
			    (cons (cons frame-size label)
				  (cdr entry)))
		  label)))
	  (let ((label (allocate-uuo-link-label prefix name frame-size)))
	    (write! (cons (list name (cons frame-size label))
			  all))
	    label)))))

(define free-uuo-link-label
  (uuo-link-label (lambda () *interned-uuo-links*)
		  (lambda (new)
		    (set! *interned-uuo-links* new))
		  ""))

(define global-uuo-link-label
  (uuo-link-label (lambda () *interned-global-links*)
		  (lambda (new)
		    (set! *interned-global-links* new))
		  "GLOBAL-"))

(define (prepare-constants-block)
  (generate/constants-block *interned-constants*
			    *interned-variables*
			    *interned-assignments*
			    *interned-uuo-links*
			    *interned-global-links*
			    *interned-static-variables*))


(define *current-profile-info*)

(define (profile-info/start)
  (set! *current-profile-info* (cons #f '())))

(define (profile-info/end)
  (set! *block-profiles* (cons *current-profile-info* *block-profiles*))
  ;;(pp *current-profile-info*)
  (set! *current-profile-info*)
  unspecific)

(define (profile-info/declare label)
  (set-car! *current-profile-info* label)
  unspecific)

(define (profile-info/add data)
  (define (merge-profile-data d1 d2)
    (cond ((symbol? d1)  (cons (cons d1 1) d2))
	  ((and (pair? d1)
		(symbol? (car d1))) (cons d1 d2))
	  ((append d1 d2))))
  (set-cdr! *current-profile-info*
	    (merge-profile-data data (cdr *current-profile-info*)))
  unspecific)


(define profile-info/offsets-tag (string->symbol "#[(?)profile-info/offsets]"))
(define profile-info/data-tag    (string->symbol "#[(?)profile-info/data]"))

(define (profile-info-key<? u v) (symbol<? u v))
(define (profile-info-key=? u v) (eq? u v))

(define (profile-info/insert-info! block label->offset)
  (define (format-data data)
    (let loop ((data (sort data (lambda (u v)
				  ;; reverse order:
				  (profile-info-key<? (car v) (car u)))))
	       (result '())
	       (current #F)
	       (count   0))
      (define (add name count result)
	(if name
	    (if (= count 1)
		(cons name result)
		(cons* count name result))
	    result))
      (cond ((null? data)
	     (list->vector (add current count result)))
	    ((profile-info-key=? current (caar data))
	     (loop (cdr data) result current (+ (cdar data) count)))
	    (else
	     (loop (cdr data) (add current count result) (caar data) (cdar data))))))
  (let* ((processed
	  (map (lambda (info)
		 (cons (label->offset (car info))
		       (format-data (cdr info))))
	       *block-profiles*))
	 (sorted   (sort processed (lambda (x y) (fix:< (car x) (car y)))))
	 (offsets  (list->vector
		    (cons profile-info/offsets-tag (map car sorted))))
	 (counts   (list->vector
		    (cons profile-info/data-tag (map cdr sorted)))))
    (system-vector-set! block
			(- (system-vector-length block) 4)
			offsets)
    (system-vector-set! block
			(- (system-vector-length block) 3)
			counts)))


;; These belong in the runtime system

(define (compiled-code-block/read-profile-count block count)
  block
  count
  0)

(define (compiled-code-block/write-profile-count block count value)
  block
  count
  value
  0)
