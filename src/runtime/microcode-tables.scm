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

;;;; Microcode shared object tables
;;; package: (runtime microcode-tables)

(declare (usual-integrations))

(define-integrable fixed-objects-slot 15)

(define (fixed-object/name->code name)
  (microcode-table-search fixed-objects-slot name))

(define (fixed-objects-vector-slot name)
  (or (fixed-object/name->code name)
      (error:bad-range-argument name 'fixed-objects-vector-slot)))

(define (fixed-objects-accessor name)
  (let ((index (fixed-objects-vector-slot name)))
    (lambda ()
      (vector-ref (get-fixed-objects-vector) index))))

(define (fixed-objects-modifier name)
  (let ((index (fixed-objects-vector-slot name)))
    (lambda (object)
      (vector-set! (get-fixed-objects-vector) index object))))

(define (fixed-objects-updater name)
  (let ((index (fixed-objects-vector-slot name)))
    (lambda (updater)
      (let ((v (get-fixed-objects-vector)))
	(vector-set! v index (updater (vector-ref v index)))))))

(define (fixed-objects-item name)
  ((fixed-objects-accessor name)))

(define (set-fixed-objects-item! name object)
  ((fixed-objects-modifier name) object))

(define (update-fixed-objects-item! name updater)
  ((fixed-objects-updater name) updater))

(define (microcode-table-search slot name)
  (let ((vector (vector-ref (get-fixed-objects-vector) slot)))
    (let ((end (vector-length vector)))
      (let loop ((i 0))
	(and (fix:< i end)
	     (let ((entry (vector-ref vector i)))
	       (if (if (pair? entry)
		       (memq name entry)
		       (eq? name entry))
		   i
		   (loop (fix:+ i 1)))))))))

(define (microcode-table-ref slot index)
  (let ((v (vector-ref (get-fixed-objects-vector) slot)))
    (and (fix:< index (vector-length v))
	 (let ((entry (vector-ref v index)))
	   (if (pair? entry)
	       (car entry)
	       entry)))))

(define-deferred returns-slot
  (fixed-object/name->code 'microcode-returns-vector))

(define (microcode-return/name->code name)
  (microcode-table-search returns-slot name))

(define (microcode-return/code->name code)
  (microcode-table-ref returns-slot code))

(define (microcode-return/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) returns-slot)))

(define-deferred errors-slot
  (fixed-object/name->code 'microcode-errors-vector))

(define (microcode-error/name->code name)
  (microcode-table-search errors-slot name))

(define (microcode-error/code->name code)
  (microcode-table-ref errors-slot code))

(define (microcode-error/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) errors-slot)))

(define-deferred terminations-slot
  (fixed-object/name->code 'microcode-terminations-vector))

(define (microcode-termination/name->code name)
  (microcode-table-search terminations-slot name))

(define (microcode-termination/code->name code)
  (microcode-table-ref terminations-slot code))

(define (microcode-termination/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) terminations-slot)))

(define-deferred system-call-names-slot
  (fixed-object/name->code 'system-call-names))

(define (microcode-system-call/name->code name)
  (microcode-table-search system-call-names-slot name))

(define (microcode-system-call/code->name code)
  (microcode-table-ref system-call-names-slot code))

(define-deferred system-call-errors-slot
  (fixed-object/name->code 'system-call-errors))

(define (microcode-system-call-error/name->code name)
  (microcode-table-search system-call-errors-slot name))

(define (microcode-system-call-error/code->name code)
  (microcode-table-ref system-call-errors-slot code))

(define-deferred types-slot
  (fixed-object/name->code 'microcode-types-vector))

(define (microcode-type/name->code name)
  (microcode-table-search types-slot
			  (let ((p
				 (find (lambda (p)
					 (memq name (cdr p)))
				       type-aliases)))
			    (if p
				(car p)
				name))))

(define (microcode-type/code->name code)
  (microcode-table-ref types-slot code))

(define (microcode-type/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) types-slot)))

(define type-aliases
  '((false manifest-vector global-environment)
    (pair list)
    (flonum big-flonum)
    (constant true)
    (return-code return-address)
    (bignum big-fixnum)
    (promise delayed)
    (fixnum address positive-fixnum negative-fixnum)
    (string character-string vector-8b)
    (hunk3-a unmarked-history)
    (triple hunk3 hunk3-b marked-history)
    (reference-trap unassigned)
    (recnum complex)))

;;;; Microcode identification

(define-deferred identifications-slot
  (fixed-object/name->code 'microcode-identification-vector))

(define identification-vector)
(define microcode-version-string)
(define microcode-id/floating-mantissa-bits)
(define microcode-id/floating-epsilon)
(define microcode-id/floating-exponent-min)
(define microcode-id/floating-exponent-max)
(define microcode-id/operating-system)
(define microcode-id/operating-system-name)
(define microcode-id/operating-system-variant)
(define microcode-id/machine-type)
(define microcode-id/compiled-code-type)
(define (read-microcode-identification!)
  (set! identification-vector
	((ucode-primitive microcode-identify)))
  (set! microcode-version-string
	(microcode-identification-item 'microcode-version))
  (set! microcode-id/floating-mantissa-bits
	(microcode-identification-item 'flonum-mantissa-length))
  (set! microcode-id/floating-epsilon
	(microcode-identification-item 'flonum-epsilon))
  (set! microcode-id/floating-exponent-min
        (microcode-identification-item 'flonum-exponent-min))
  (set! microcode-id/floating-exponent-max
        (microcode-identification-item 'flonum-exponent-max))
  (set! microcode-id/operating-system-name
	(microcode-identification-item 'os-name-string))
  (set! microcode-id/operating-system
	(intern microcode-id/operating-system-name))
  (set! microcode-id/operating-system-variant
	(microcode-identification-item 'os-variant-string))
  (set! microcode-id/machine-type
	(microcode-identification-item 'machine-type-string "unknown"))
  (set! microcode-id/compiled-code-type
	(intern (microcode-identification-item 'cc-arch-string "unknown")))
  unspecific)

(add-boot-init!
 (lambda ()
   (read-microcode-identification!)
   (run-deferred-boot-actions 'fixed-objects)))

(define (microcode-identification-item name #!optional default-value)
  (let ((index (microcode-table-search identifications-slot name)))
    (if index
	(vector-ref identification-vector index)
	(begin
	  (if (default-object? default-value)
	      (error:bad-range-argument name 'microcode-identification-item))
	  default-value))))

(define (get-microcode-version-numbers)
  (map (lambda (s) (or (string->number s) s))
       (burst-string microcode-version-string #\. #f)))

(define (microcode-id/operating-system-suffix #!optional os-type)
  (case (if (default-object? os-type)
	    microcode-id/operating-system
	    os-type)
    ((nt) "w32")
    ((unix) "unx")
    (else (error "Unknown operating system:" os-type))))