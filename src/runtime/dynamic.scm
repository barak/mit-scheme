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

;;;; Parameters
;;; package: (runtime dynamic)

(declare (usual-integrations))

(define bindings '())	    ; The current thread's parameter bindings.
(define parameter?)
(define parameter-metadata)
(define set-parameter-metadata!)
(define get-metadata-alist)

(define (use-metadata-implementation! implementation)
  (set! parameter? (bundle-ref implementation 'has?))
  (set! parameter-metadata (bundle-ref implementation 'get))
  (set! set-parameter-metadata! (bundle-ref implementation 'put!))
  (set! get-metadata-alist (bundle-ref implementation 'get-alist))
  unspecific)

;; Use alist for cold-load.
(use-metadata-implementation! (make-alist-metadata-table))

;; Later move metadata to hash table.
(add-boot-init!
 (lambda ()
   (let ((implementation (make-hashed-metadata-table)))
     (implementation 'put-alist! (get-metadata-alist))
     (use-metadata-implementation! implementation))))

(define-guarantee parameter "parameter")

(define (make-unsettable-parameter initial-value #!optional converter)
  (make-parameter-internal initial-value converter #f))

(define (make-settable-parameter initial-value #!optional converter)
  (make-parameter-internal initial-value converter #t))

(define (make-parameter-internal initial-value converter settable?)
  (make-general-parameter initial-value
			  (if (default-object? converter)
			      default-parameter-converter
			      (guarantee unary-procedure? converter))
			  default-parameter-merger
			  default-parameter-getter
			  (and settable? default-parameter-setter)))

(define (default-parameter-converter value) value)
(define (default-parameter-merger old-value new-value) old-value new-value)
(define (default-parameter-getter value) value)
(define (default-parameter-setter set-param value) (set-param value))

(define (make-general-parameter initial-value converter merger getter setter)
  (guarantee procedure? converter 'make-general-parameter)
  (guarantee procedure? getter 'make-general-parameter)
  (if setter (guarantee procedure? setter 'make-general-parameter))
  (make-general-parameter-1 (converter initial-value)
			    converter
			    merger
			    getter
			    setter))

(define (make-general-parameter-1 initial-value converter merger getter setter)
  (let* ((metadata (make-metadata initial-value converter merger getter setter))
	 (parameter
	  (if setter
	      (lambda (#!optional new-value)
		(if (default-object? new-value)
		    (getter (get-value metadata))
		    (setter (lambda (value)
			      (set-value! metadata value))
			    (convert metadata new-value))))
	      (lambda ()
		(getter (get-value metadata))))))
    (set-parameter-metadata! parameter metadata)
    parameter))

(define (parameterize* new-bindings thunk)
  (guarantee alist? new-bindings 'parameterize*)
  (let ((temp
	 (map* bindings
	       (lambda (p) (create-binding (car p) (cdr p)))
	       new-bindings)))
    (let ((swap!
	   (lambda ()
	     (set! bindings (set! temp (set! bindings)))
	     unspecific)))
      (shallow-fluid-bind swap! thunk swap!))))

(define (create-binding parameter value)
  (let ((metadata (parameter-metadata parameter)))
    (if (forwarder? metadata)
	(create-binding (forwarder-parameter metadata)
			((forwarder-convert-to metadata) value))
	(cons metadata (convert metadata value)))))

(define (parameter-converter parameter)
  (let ((metadata (parameter-metadata parameter)))
    (if (forwarder? metadata)
	(let ((converter1 (forwarder-convert-to metadata))
	      (converter2 (parameter-converter parameter)))
	  (lambda (value)
	    (converter2 (converter1 value))))
	(metadata-converter metadata))))

(define-record-type <metadata>
    (make-metadata value converter merger getter setter)
    metadata?
  (value metadata-value set-metadata-value!)
  (converter metadata-converter)
  (merger metadata-merger)
  (getter metadata-getter)
  (setter metadata-setter))

(define (get-value metadata)
  (let ((p (assq metadata bindings)))
    (if p
	(cdr p)
	(metadata-value metadata))))

(define (set-value! metadata value)
  (let ((p (assq metadata bindings)))
    (if p
	(set-cdr! p value)
	(set-metadata-value! metadata value))))

(define (convert metadata value)
  ((metadata-merger metadata)
   (get-value metadata)
   ((metadata-converter metadata) value)))

(define (make-forwarding-parameter parameter convert-to convert-from)
  (guarantee parameter? parameter 'make-forwarding-parameter)
  (guarantee unary-procedure? convert-to 'make-forwarding-parameter)
  (guarantee unary-procedure? convert-from 'make-forwarding-parameter)
  (make-forwarding-parameter-1 parameter convert-to convert-from))

(define (make-forwarding-parameter-1 parameter convert-to convert-from)
  (let ((parameter*
	 (if (metadata-setter (skip-forwarders parameter))
	     (lambda (#!optional new-value)
	       (if (default-object? new-value)
		   (convert-from (parameter))
		   (parameter (convert-to new-value))))
	     (lambda ()
	       (convert-from (parameter))))))
    (set-parameter-metadata! parameter*
			     (make-forwarder parameter convert-to convert-from))
    parameter*))

(define-record-type <forwarder>
    (make-forwarder parameter convert-to convert-from)
    forwarder?
  (parameter forwarder-parameter)
  (convert-to forwarder-convert-to)
  (convert-from forwarder-convert-from))

(define (skip-forwarders parameter)
  (let ((metadata (parameter-metadata parameter)))
    (if (forwarder? metadata)
	(skip-forwarders (forwarder-parameter metadata))
	metadata)))

(define (copy-parameter parameter)
  (let ((metadata (parameter-metadata parameter)))
    (if (forwarder? metadata)
	(make-forwarding-parameter-1 (copy-parameter
				      (forwarder-parameter metadata))
				     (forwarder-convert-to metadata)
				     (forwarder-convert-from metadata))
	(make-general-parameter-1 (metadata-value metadata)
				  (metadata-converter metadata)
				  (metadata-merger metadata)
				  (metadata-getter metadata)
				  (metadata-setter metadata)))))