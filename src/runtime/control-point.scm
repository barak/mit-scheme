#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Control Points
;;; package: (runtime new-control-point)

(declare (usual-integrations))

(add-boot-deps! '(runtime microcode-tables))

(define-primitives
  (control-point-next-frame 2)
  (return-frame-type 2)
  (primitive-datum-ref 2))

(define-deferred return-frame-types
  (microcode-return-frame-types))

(define (control-point? object)
  (object-type? (ucode-type control-point) object))
(register-predicate! control-point? 'control-point)

(define (make-control-point raw-frames)
  (object-new-type (ucode-type control-point)
		   (vector-concatenate (cons '#(#f 0) raw-frames))))

(define-integrable (control-point-start-index)
  2)

(define-integrable (control-point-length control-point)
  (system-vector-length control-point))

(define (control-point-frames control-point)
  (generator->list (control-point->frame-generator control-point)))

(define (control-point->frame-generator control-point)
  (gmap decode-raw-control-point-frame
	(control-point->raw-frame-generator control-point)))

(define (control-point->raw-frame-generator control-point)
  (let ((index)
	(end))

    (define (new-cp! cp)
      (set! control-point cp)
      (set! index (control-point-start-index))
      (set! end (control-point-length cp)))

    (define (generator)
      (if (fix:< index end)
	  (let* ((index* (control-point-next-frame control-point index))
		 (frame (make-vector (fix:- index* index)))
		 (result (vector index end frame)))
	    (do ((i index (fix:+ i 1))
		 (j 0 (fix:+ j 1)))
		((not (fix:< i index*))
		 (set! index index*))
	      (vector-set! frame j (system-vector-ref control-point i)))
	    (if (eq? (ucode-return-address join-stacklets)
		     (vector-ref frame 0))
		(begin
		  (assert (fix:= index* end))
		  (new-cp! (vector-ref frame 1))))
	    result)
	  (eof-object)))

    (new-cp! control-point)
    generator))

(define (decode-raw-control-point-frame raw-result)
  (let ((findex (vector-ref raw-result 0))
	(cpend (vector-ref raw-result 1))
	(raw (vector-ref raw-result 2)))
    (let ((info (vector-ref return-frame-types (return-frame-type raw 0)))
	  (return-code-name
	   (let ((address (vector-ref raw 0)))
	     (and (interpreter-return-address? address)
		  (return-address/name address)))))

      (define (make ftype . alist)
	(make-cpoint-frame ftype findex cpend raw info alist))

      (define-integrable (name index)
	(vector-ref info (fix:+ 3 (fix:* 2 index))))

      (define-integrable (val-loc index)
	(vector-ref info (fix:+ 4 (fix:* 2 index))))

      (define-integrable (elt index)
	(cons (name index) (vector-ref raw (val-loc index))))

      (define-integrable (rest-elts index)
	(cons (name index) (vector->list raw (val-loc index))))

      (let ((frame-type-name (vector-ref info 0)))
	(case frame-type-name
	  ((with-arg)
	   (let ((name
		  (case return-code-name
		    ((join-stacklets) 'control-point)
		    ((access-continue) 'expression)
		    ((force-snap-thunk) 'delayed)
		    ((normal-garbage-collect-done) 'gc-result)
		    ((restore-value pop-return-error) 'value)
		    ((restore-interrupt-mask) 'interrupt-mask)
		    ((halt) 'termination-code)
		    (else #f))))
	     (if name
		 (make return-code-name
		       (cons name (vector-ref raw (val-loc 0))))
		 (make return-code-name))))
	  ((exp+env history stack-marker)
	   (make return-code-name (elt 0) (elt 1)))
	  ((apply)
	   (make return-code-name (elt 0) (rest-elts 1)))
	  ((return-to-compiled-code)
	   (apply make return-code-name (elt 0)
		  (return-to-cc-extra-fields return-code-name raw)))
	  ((compiled-address)
	   (apply make frame-type-name (cc-address-extra-fields raw 0)))
	  ((combination-save)
	   ;; The index to primitive-datum-ref is relative to the address of the
	   ;; object.  For a vector, that's one greater than the vector index.
	   (let ((n-blanks
		  (primitive-datum-ref raw (fix:+ 1 (val-loc 2)))))
	     (make return-code-name
		   (elt 0)
		   (elt 1)
		   (cons (name 2) n-blanks)
		   (cons (name 3)
			 (vector->list raw (fix:+ (val-loc 3) n-blanks))))))
	  ((hardware-trap)
	   (make return-code-name (elt 0) (elt 1) (elt 2) (elt 3)
		 (elt 4) (elt 5) (elt 6) (elt 7)))
	  ((return-to-interpreter)
	   (make frame-type-name))
	  ((cc-internal-apply cc-bkpt cc-invocation)
	   (make frame-type-name (elt 0) (rest-elts 1)))
	  ((cc-restore-interrupt-mask)
	   (make frame-type-name (elt 0)))
	  ((cc-stack-marker)
	   (make frame-type-name (elt 0) (elt 1)))
	  (else
	   (error "Unknown return-frame-type code:" frame-type-name)))))))

(define (cc-address-extra-fields raw index)
  (let ((entry (vector-ref raw index)))
    (if (compiled-procedure? entry)
	(list (cons 'procedure entry)
	      (cons 'arguments (vector->list raw (fix:+ index 1))))
	(list (cons 'entry entry)))))

(define (return-to-cc-extra-fields return-code-name raw)
  (case return-code-name
    ((compiler-reference-trap-restart
      compiler-safe-reference-trap-restart
      compiler-unassigned?-trap-restart)
     (list (cons 'variable (vector-ref raw 2))
	   (cons 'environment (vector-ref raw 3))))
    ((compiler-assignment-trap-restart)
     (list (cons 'variable (vector-ref raw 2))
	   (cons 'environment (vector-ref raw 3))
	   (cons 'value (safe-system-vector-ref raw 4))))
    ((compiler-lookup-apply-trap-restart
      compiler-operator-lookup-trap-restart)
     (cons* (cons 'variable (vector-ref raw 2))
	    (cons 'environment (vector-ref raw 3))
	    (cc-address-extra-fields raw 4)))
    ((compiler-error-restart)
     (list (cons 'primitive (vector-ref raw 2))))
    ((compiler-interrupt-restart)
     (cons (cons 'state (vector-ref raw 2))
	   (cc-address-extra-fields raw 3)))
    (else
     '())))

;;;; Frame abstraction

(define-record-type <cpoint-frame>
    make-cpoint-frame
    cpoint-frame?
  (type cpoint-frame-type)
  (index cpoint-frame-start)		;index of frame within control point
  (cpoint-end cpoint-frame-cpoint-end)	;length of control point
  (raw cpoint-frame-raw)
  (info cpoint-frame-info)
  (fields cpoint-frame-fields))

(define (cpoint-frame-end cpoint)
  (fix:+ (cpoint-frame-start cpoint)
	 (cpoint-frame-length cpoint)))

(define (cpoint-frame-length frame)
  (vector-length (cpoint-frame-raw frame)))

(define (cpoint-frame-ref frame index)
  (vector-ref (cpoint-frame-raw frame) index))

(define (cpoint-frame-return-address frame)
  (vector-ref (cpoint-frame-raw frame) 0))

(define (cpoint-frame-return-code frame)
  (let ((return-address (cpoint-frame-return-address frame)))
    (and (interpreter-return-address? return-address)
	 (return-address/code return-address))))

(define (cpoint-frame-return-type frame)
  (vector-ref (cpoint-frame-info frame) 0))

(define (cpoint-frame:subproblem? frame)
  (vector-ref (cpoint-frame-info frame) 1))

(define (cpoint-frame:history-subproblem? frame)
  (vector-ref (cpoint-frame-info frame) 2))

(define (cpoint-frame-field-value frame name)
  (let ((p (assq name (cpoint-frame-fields frame))))
    (if (not p)
	(error "Unknown frame field name:" name))
    (cdr p)))

(define (cpoint-frame-field-name? frame name)
  (and (assq name (cpoint-frame-fields frame)) #t))

(define (cpoint-frame-field-names frame)
  (map car (cpoint-frame-fields frame)))

(define (cpoint-frame:compiled-address? frame)
  (eq? (cpoint-frame-type frame) 'compiled-address))

(define (cpoint-frame:compiled-code? frame)
  (compiled-return-address? (cpoint-frame-return-address frame)))

(define (cpoint-frame:hardware-trap? frame)
  (eq? (cpoint-frame-type frame) 'hardware-trap))

(define (cpoint-frame:join-stacklets? frame)
  (eq? (cpoint-frame-type frame) 'join-stacklets))

(define (cpoint-frame:restore-interrupt-mask? frame)
  (let ((type (cpoint-frame-type frame)))
    (or (eq? type 'restore-interrupt-mask)
	(eq? type 'cc-restore-interrupt-mask))))

(define (cpoint-frame:restore-history? frame)
  (let ((type (cpoint-frame-type frame)))
    (or (eq? type 'restore-history)
	(eq? type 'restore-dont-copy-history))))

(define (cpoint-frame:return-to-compiled-code? frame)
  (eq? (cpoint-frame-type frame) 'return-to-compiled-code))

(define (cpoint-frame:stack-marker? frame)
  (let ((type (cpoint-frame-type frame)))
    (or (eq? type 'stack-marker)
	(eq? type 'cc-stack-marker))))

(define (cpoint-frame:stack-marker-of-type? marker-type frame)
  (and (cpoint-frame:stack-marker? frame)
       (eq? marker-type (cpoint-frame-field-value frame 'marker-type))))

(define (cpoint-frame:repl-eval-boundary? frame)
  (cpoint-frame:stack-marker-of-type? with-repl-eval-boundary frame))

(define (cpoint-frames->control-point frames)
  (make-control-point (cpoint-frames-raw-prefix frames)))

(define (cpoint-frames-raw-prefix frames)
  (let ((join (find cpoint-frame:join-stacklets? frames)))
    (if join
	(let loop ((frames frames) (raw '()))
	  (let ((raw (cons (cpoint-frame-raw (car frames)) raw)))
	    (if (eq? (car frames) join)
		(reverse raw)
		(loop (cdr frames) raw))))
	(map cpoint-frame-raw frames))))

(define (old-control-point frames interrupt-mask history
			   previous-restore-history-offset)
  (make-control-point
   (old-raw-frames (cpoint-frames-raw-prefix frames)
		   interrupt-mask history previous-restore-history-offset)))

(define (old-raw-frames frames interrupt-mask history
			previous-restore-history-offset)
  (if (and (pair? frames)
	   (pair? (cdr frames))
	   (let ((f1 (car frames))
		 (f2 (car frames)))
	     (and (eq? (vector-ref f1 0)
		       (ucode-return-address restore-interrupt-mask))
		  (eqv? (vector-ref f1 1) interrupt-mask)
		  (eq? (vector-ref f2 0) (ucode-return-address restore-history))
		  (eq? (vector-ref f2 1) history)
		  (eqv? (vector-ref f2 2) previous-restore-history-offset))))
      frames
      (cons* (vector (ucode-return-address restore-interrupt-mask)
		     interrupt-mask)
	     (vector (ucode-return-address restore-history)
		     history
		     previous-restore-history-offset
		     #f)
	     frames)))

(define (describe-cpoint-hardware-trap-frame cpoint verbose? port)

  (define (write-hex value port)
    (if (< value #x10)
	(write value port)
	(begin
	  (write-string "#x" port)
	  (write-string (number->string value #x10) port))))

  (let ((name (cpoint-frame-field-value cpoint 'signal-name))
	(state (cpoint-frame-field-value cpoint 'recovery-state)))
    (if (not name)
	(write-string "User microcode reset" port)
	(let ((code (cpoint-frame-field-value cpoint 'code-name)))
	  (write-string "Hardware trap " port)
	  (write-string name port)
	  (write-string " (")
	  (if (and (pair? code) (cdr code))
	      (write-string (cdr code) port)
	      (begin
		(write-string "code = " port)
		(write-hex (if (pair? code) (car code) code) port)))
	  (write-string ")" port)))
    (if verbose?
	(let ((pc-info-1 (cpoint-frame-field-value cpoint 'pc-info-1))
	      (pc-info-2 (cpoint-frame-field-value cpoint 'pc-info-2)))
	  (case state
	    ((0)				; unknown
	     (write-string " at an unknown location." port))
	    ((1)				; primitive
	     (write-string " within " port)
	     (write pc-info-1 port))
	    ((2)				; compiled code
	     (write-string " at offset " port)
	     (write-hex pc-info-2 port)
	     (newline port)
	     (write-string "within " port)
	     (let ((block pc-info-1))
	       (write block port)
	       (let-values (((filename index library)
			     (compiled-code-block/filename-and-index block)))
		 (declare (ignore index library))
		 (if filename
		     (begin
		       (write-string " (" port)
		       (write filename port)
		       (write-string ")" port))))))
	    ((3)				; probably compiled-code
	     (write-string " at an unknown compiled-code location." port))
	    ((4)				; builtin (i.e. hook)
	     (let ((name ((ucode-primitive builtin-index->name 1) pc-info-1)))
	       (if name
		   (begin
		     (write-string " in assembly-language utility " port)
		     (write-string name port))
		   (begin
		     (write-string " in unknown assembly-language utility " port)
		     (write-hex pc-info-1 port)))))
	    ((5)				; utility
	     (let ((name ((ucode-primitive utility-index->name 1) pc-info-1)))
	       (if name
		   (begin
		     (write-string " in compiled-code utility " port)
		     (write-string name port))
		   (begin
		     (write-string " in unknown compiled-code utility " port)
		     (write-hex pc-info-1 port)))))
	    (else
	     (error "Unknown state:" state)))))))