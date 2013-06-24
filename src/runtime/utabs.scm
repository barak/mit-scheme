#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Microcode Name <-> Code Maps
;;; package: (runtime microcode-tables)

(declare (usual-integrations))

(define (read-microcode-tables!)
  (set! identification-vector ((ucode-primitive microcode-identify)))
  (set! errors-slot (fixed-object/name->code 'MICROCODE-ERRORS-VECTOR))
  (set! identifications-slot
	(fixed-object/name->code 'MICROCODE-IDENTIFICATION-VECTOR))
  (set! returns-slot (fixed-object/name->code 'MICROCODE-RETURNS-VECTOR))
  (set! terminations-slot
	(fixed-object/name->code 'MICROCODE-TERMINATIONS-VECTOR))
  (set! types-slot (fixed-object/name->code 'MICROCODE-TYPES-VECTOR))
  (set! non-object-slot (fixed-object/name->code 'NON-OBJECT))
  (set! system-call-names-slot (fixed-object/name->code 'SYSTEM-CALL-NAMES))
  (set! system-call-errors-slot (fixed-object/name->code 'SYSTEM-CALL-ERRORS))
  (set! microcode-version-string
	(microcode-identification-item 'MICROCODE-VERSION))
  (set! char:newline (microcode-identification-item 'NEWLINE-CHAR))
  (set! microcode-id/floating-mantissa-bits
	(microcode-identification-item 'FLONUM-MANTISSA-LENGTH))
  (set! microcode-id/floating-epsilon
	(microcode-identification-item 'FLONUM-EPSILON))
  (let ((name (microcode-identification-item 'OS-NAME-STRING)))
    (set! microcode-id/operating-system (intern name))
    (set! microcode-id/operating-system-name name))
  (set! microcode-id/operating-system-variant
	(microcode-identification-item 'OS-VARIANT-STRING))
  (set! microcode-id/stack-type
	(let ((string (microcode-identification-item 'STACK-TYPE-STRING)))
	  (cond ((string? string) (intern string))
		((not string) 'STANDARD)
		(else (error "Illegal stack type:" string)))))
  (set! microcode-id/machine-type
	(or (microcode-identification-item 'MACHINE-TYPE-STRING #f)
	    "unknown-machine"))
  (set! microcode-id/compiled-code-type
	(intern (or (microcode-identification-item 'CC-ARCH-STRING #f)
		    "unknown")))
  (set! microcode-id/tty-x-size
	(microcode-identification-item 'CONSOLE-WIDTH))
  (set! microcode-id/tty-y-size
	(microcode-identification-item 'CONSOLE-HEIGHT))
  unspecific)

(define (intern string)
  ((ucode-primitive string->symbol)
   (let ((size (string-length string)))
     (let ((result (string-allocate size)))
       ((ucode-primitive substring-move-right!) string 0 size result 0)
       ((ucode-primitive substring-downcase!) result 0 size)
       result))))

(define (get-microcode-version-string)
  microcode-version-string)

(define (get-microcode-version-numbers)
  (map (lambda (s) (or (string->number s) s))
       (burst-string microcode-version-string #\. #f)))

(define microcode-version-string)
(define char:newline)
(define microcode-id/tty-x-size)
(define microcode-id/tty-y-size)
(define microcode-id/floating-mantissa-bits)
(define microcode-id/floating-epsilon)
(define microcode-id/operating-system)
(define microcode-id/operating-system-name)
(define microcode-id/operating-system-variant)
(define microcode-id/stack-type)
(define microcode-id/machine-type)
(define microcode-id/compiled-code-type)

(define-integrable fixed-objects-slot 15)
(define non-object-slot)

(define (fixed-object/name->code name)
  (microcode-table-search fixed-objects-slot name))

(define (fixed-object/code->name code)
  (microcode-table-ref fixed-objects-slot code))

(define (fixed-object/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) fixed-objects-slot)))

(define (fixed-objects-vector-slot name)
  (or (fixed-object/name->code name)
      (error:bad-range-argument name 'FIXED-OBJECTS-VECTOR-SLOT)))

(define (fixed-objects-item name)
  (vector-ref (get-fixed-objects-vector) (fixed-objects-vector-slot name)))

(define (microcode-object/unassigned)
  (vector-ref (get-fixed-objects-vector) non-object-slot))

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

(define (microcode-table-entry slot index)
  (let ((vector (vector-ref (get-fixed-objects-vector) slot)))
    (and (fix:< index (vector-length vector))
	 (vector-ref vector index))))

(define (microcode-table-ref slot index)
  (let ((entry (microcode-table-entry slot index)))
    (if (pair? entry)
	(car entry)
	entry)))

(define returns-slot)

(define (microcode-return/name->code name)
  (microcode-table-search returns-slot name))

(define (microcode-return/code->name code)
  (microcode-table-ref returns-slot code))

(define (microcode-return/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) returns-slot)))

(define errors-slot)

(define (microcode-error/name->code name)
  (microcode-table-search errors-slot name))

(define (microcode-error/code->name code)
  (microcode-table-ref errors-slot code))

(define (microcode-error/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) errors-slot)))

(define terminations-slot)

(define (microcode-termination/name->code name)
  (microcode-table-search terminations-slot name))

(define (microcode-termination/code->name code)
  (microcode-table-ref terminations-slot code))

(define (microcode-termination/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) terminations-slot)))

(define identifications-slot)
(define identification-vector)

(define (microcode-identification-vector-slot name #!optional error?)
  (let ((v (microcode-table-search identifications-slot name)))
    (if (and (not v) (if (default-object? error?) #t error?))
	(error:bad-range-argument name 'MICROCODE-IDENTIFICATION-VECTOR-SLOT))
    v))

(define (microcode-identification-item name #!optional error?)
  (let ((slot (microcode-identification-vector-slot name error?)))
    (and slot
	 (vector-ref identification-vector slot))))

(define system-call-names-slot)

(define (microcode-system-call/name->code name)
  (microcode-table-search system-call-names-slot name))

(define (microcode-system-call/code->name code)
  (microcode-table-ref system-call-names-slot code))

(define system-call-errors-slot)

(define (microcode-system-call-error/name->code name)
  (microcode-table-search system-call-errors-slot name))

(define (microcode-system-call-error/code->name code)
  (microcode-table-ref system-call-errors-slot code))

(define types-slot)

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

(define (microcode-type/code->names code)
  (let ((name (microcode-table-entry types-slot code)))
    (if name
	(or (assq name type-aliases)
	    (list name))
	'())))

(define (microcode-type/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) types-slot)))

(define type-aliases
  '((FALSE MANIFEST-VECTOR GLOBAL-ENVIRONMENT)
    (PAIR LIST)
    (FLONUM BIG-FLONUM)
    (CONSTANT TRUE)
    (RETURN-CODE RETURN-ADDRESS)
    (BIGNUM BIG-FIXNUM)
    (PROMISE DELAYED)
    (FIXNUM ADDRESS POSITIVE-FIXNUM NEGATIVE-FIXNUM)
    (STRING CHARACTER-STRING VECTOR-8B)
    (HUNK3-A UNMARKED-HISTORY)
    (TRIPLE HUNK3 HUNK3-B MARKED-HISTORY)
    (REFERENCE-TRAP UNASSIGNED)
    (RECNUM COMPLEX)))