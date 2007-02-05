#| -*-Scheme-*-

$Id: utabs.scm,v 14.22 2007/01/05 21:19:28 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

(define (re-read-microcode-tables!)
  (let ((file-name ((ucode-primitive microcode-tables-filename))))
    (if (file-exists? file-name)
	(read-microcode-tables! file-name)
	(let ((new-identification ((ucode-primitive microcode-identify))))
	  (let ((new-vector (vector-copy new-identification))
		(old-vector (vector-copy identification-vector)))
	    (let loop ((fields '(CONSOLE-WIDTH CONSOLE-HEIGHT)))
	      (if (pair? fields)
		  (let ((slot
			 (microcode-identification-vector-slot (car fields))))
		    (vector-set! old-vector slot #f)
		    (vector-set! new-vector slot #f)
		    (loop (cdr fields)))))
	    (if (not (equal? new-vector old-vector))
		(error "Missing microcode description:" file-name))
	    (set! identification-vector new-identification)
	    (set! microcode-id/tty-x-size
		  (microcode-identification-item 'CONSOLE-WIDTH))
	    (set! microcode-id/tty-y-size
		  (microcode-identification-item 'CONSOLE-HEIGHT))
	    unspecific)))))

(define (read-microcode-tables! #!optional filename)
  (set! microcode-tables-identification
	(scode-eval
	 (or (let ((prim ((ucode-primitive get-primitive-address)
			  'initialize-c-compiled-block
			  #f)))
	       (and prim
		    (prim "microcode_utabmd")))
	     ((ucode-primitive binary-fasload)
	      (if (default-object? filename)
		  ((ucode-primitive microcode-tables-filename))
		  filename)))
	 system-global-environment))
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
	(let ((version (microcode-identification-item 'MICROCODE-VERSION)))
	  (if (string? version)
	      version
	      (string-append
	       (number->string version)
	       "."
	       (number->string
		(microcode-identification-item 'MICROCODE-MODIFICATION))))))
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
	(if (microcode-table-search identifications-slot 'MACHINE-TYPE-STRING)
	    (microcode-identification-item 'MACHINE-TYPE-STRING)
	    "unknown-machine"))
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

(define microcode-tables-identification)
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

(define types-slot)

(define (microcode-type/name->code name)
  (microcode-table-search types-slot name))

(define (microcode-type/code->name code)
  (microcode-table-ref types-slot code))

(define (microcode-type/code->names code)
  (let ((entry (microcode-table-entry types-slot code)))
    (cond ((not entry) '())
	  ((list? entry) entry)
	  (else (list entry)))))

(define (microcode-type/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) types-slot)))

(define identifications-slot)
(define identification-vector)

(define (microcode-identification-vector-slot name)
  (or (microcode-table-search identifications-slot name)
      (error:bad-range-argument name 'MICROCODE-IDENTIFICATION-VECTOR-SLOT)))

(define (microcode-identification-item name)
  (vector-ref identification-vector
	      (microcode-identification-vector-slot name)))

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