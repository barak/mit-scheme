#| -*-Scheme-*-

$Id: utabs.scm,v 14.14 2001/09/25 05:08:40 cph Exp $

Copyright (c) 1988-1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
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
	      (if (not (null? fields))
		  (let ((slot
			 (microcode-identification-vector-slot (car fields))))
		    (vector-set! old-vector slot false)
		    (vector-set! new-vector slot false)
		    (loop (cdr fields)))))
	    (if (not (equal? new-vector old-vector))
		(error
		 "re-read-microcode-tables!: Missing microcode description"
		 file-name)
		(begin
		  (set! identification-vector new-identification)
		  (set! microcode-id/tty-x-size
			(microcode-identification-item 'CONSOLE-WIDTH))
		  (set! microcode-id/tty-y-size
			(microcode-identification-item 'CONSOLE-HEIGHT))
		  unspecific)))))))

(define (read-microcode-tables! #!optional filename)
  (set! microcode-tables-identification
	(scode-eval ((ucode-primitive binary-fasload)
		     (if (default-object? filename)
			 ((ucode-primitive microcode-tables-filename))
			 filename))
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
  (set! microcode-id/version
	(microcode-identification-item 'MICROCODE-VERSION))
  (set! microcode-id/modification
	(microcode-identification-item 'MICROCODE-MODIFICATION))
  (set! microcode-id/release-string
	(microcode-identification-item 'SYSTEM-RELEASE-STRING))
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
		(else (error "illegal stack type" string)))))
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

(define microcode-tables-identification)
(define microcode-id/version)
(define microcode-id/modification)
(define microcode-id/release-string)
(define char:newline)
(define microcode-id/tty-x-size)
(define microcode-id/tty-y-size)
(define microcode-id/floating-mantissa-bits)
(define microcode-id/floating-epsilon)
(define microcode-id/operating-system)
(define microcode-id/operating-system-name)
(define microcode-id/operating-system-variant)
(define microcode-id/stack-type)

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
      (error "FIXED-OBJECTS-VECTOR-SLOT: Unknown name" name)))

(define (fixed-objects-item name)
  (vector-ref (get-fixed-objects-vector) (fixed-objects-vector-slot name)))

(define (microcode-object/unassigned)
  (vector-ref (get-fixed-objects-vector) non-object-slot))

(define (microcode-table-search slot name)
  (let ((vector (vector-ref (get-fixed-objects-vector) slot)))
    (let ((end (vector-length vector)))
      (define (loop i)
	(and (not (= i end))
	     (let ((entry (vector-ref vector i)))
	       (if (if (pair? entry)
		       (memq name entry)
		       (eq? name entry))
		   i
		   (loop (1+ i))))))
      (loop 0))))

(define (microcode-table-ref slot index)
  (let ((vector (vector-ref (get-fixed-objects-vector) slot)))
    (and (< index (vector-length vector))
	 (let ((entry (vector-ref vector index)))
	   (if (pair? entry)
	       (car entry)
	       entry)))))

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

(define (microcode-type/code-limit)
  (vector-length (vector-ref (get-fixed-objects-vector) types-slot)))

(define identifications-slot)
(define identification-vector)

(define (microcode-identification-vector-slot name)
  (or (microcode-table-search identifications-slot name)
      (error "Unknown microcode identification item" name)))

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