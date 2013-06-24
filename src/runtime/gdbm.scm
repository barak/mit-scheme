#| -*-Scheme-*-

$Id: gdbm.scm,v 1.3 2000/04/10 18:32:32 cph Exp $

Copyright (c) 1996, 1999, 2000 Massachusetts Institute of Technology

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

;;;; gdbm Database Library Interface
;;; package: (runtime gdbm)

(declare (usual-integrations))

(define (gdbm-available?)
  (implemented-primitive-procedure? (ucode-primitive gdbm-open 4)))

;; Parameters to gdbm_open for READERS, WRITERS, and WRITERS who can
;; create the database.
(define GDBM_READER  0)		;A reader.
(define GDBM_WRITER  1)		;A writer.
(define GDBM_WRCREAT 2)		;A writer.  Create the db if needed.
(define GDBM_NEWDB   3)		;A writer.  Always create a new db.
(define GDBM_FAST    16)	;Write fast! => No fsyncs.

(define (gdbm-open filename block-size flags mode)
  (let ((filename (->namestring (merge-pathnames filename))))
    (without-interrupts
     (lambda ()
       (let ((descriptor
	      (gdbm-error ((ucode-primitive gdbm-open 4)
			   filename block-size flags mode))))
	 (let ((gdbf (make-gdbf descriptor filename)))
	   (add-to-gc-finalizer! gdbf-finalizer gdbf descriptor)
	   gdbf))))))

(define (gdbm-close gdbf)
  (if (not (gdbf? gdbf))
      (error:wrong-type-argument gdbf "gdbm handle" 'GDBM-CLOSE))
  (without-interrupts
   (lambda ()
     (if (gdbf-descriptor gdbf)
	 (begin
	   (remove-from-gc-finalizer! gdbf-finalizer gdbf)
	   (set-gdbf-descriptor! gdbf #f))))))

;; Parameters to gdbm_store for simple insertion or replacement in the
;; case that the key is already in the database.
(define GDBM_INSERT  0)		;Never replace old data with new.
(define GDBM_REPLACE 1)		;Always replace old data with new.

(define (gdbm-store gdbf key datum flags)
  (gdbm-error
   ((ucode-primitive gdbm-store 4) (guarantee-gdbf gdbf 'GDBM-STORE)
				   key datum flags)))

(define (gdbm-fetch gdbf key)
  ((ucode-primitive gdbm-fetch 2) (guarantee-gdbf gdbf 'GDBM-FETCH) key))

(define (gdbm-exists? gdbf key)
  ((ucode-primitive gdbm-exists 2) (guarantee-gdbf gdbf 'GDBM-EXISTS?) key))

(define (gdbm-delete gdbf key)
  (gdbm-error
   ((ucode-primitive gdbm-delete 2) (guarantee-gdbf gdbf 'GDBM-DELETE) key)))

(define (gdbm-firstkey gdbf)
  ((ucode-primitive gdbm-firstkey 1) (guarantee-gdbf gdbf 'GDBM-FIRSTKEY)))

(define (gdbm-nextkey gdbf key)
  ((ucode-primitive gdbm-nextkey 2) (guarantee-gdbf gdbf 'GDBM-NEXTKEY) key))

(define (gdbm-reorganize gdbf)
  (gdbm-error
   ((ucode-primitive gdbm-reorganize 1)
    (guarantee-gdbf gdbf 'GDBM-REORGANIZE))))

(define (gdbm-sync gdbf)
  ((ucode-primitive gdbm-sync 1) (guarantee-gdbf gdbf 'GDBM-SYNC)))

(define (gdbm-version)
  ((ucode-primitive gdbm-version 0)))

;; Parameters to gdbm_setopt, specifing the type of operation to perform.
(define GDBM_CACHESIZE 1)       ;Set the cache size.
(define GDBM_FASTMODE  2)       ;Toggle fast mode.

(define (gdbm-setopt gdbf opt val)
  (gdbm-error
   ((ucode-primitive gdbm-setopt 3) (guarantee-gdbf gdbf 'GDBM-SETOPT)
				    opt val)))

(define-structure (gdbf
		   (print-procedure (standard-unparser-method 'GDBF
				      (lambda (gdbf port)
					(write-char #\space port)
					(write (gdbf-filename gdbf) port)))))
  descriptor
  (filename #f read-only #t))

(define (guarantee-gdbf gdbf procedure)
  (if (gdbf? gdbf)
      (or (gdbf-descriptor gdbf) (error:bad-range-argument gdbf procedure))
      (error:wrong-type-argument gdbf "gdbm handle" procedure)))

(define (gdbm-error object)
  (if (string? object) (error "gdbm error:" object))
  object)

(define gdbf-finalizer)
(define (initialize-package!)
  (set! gdbf-finalizer (make-gc-finalizer (ucode-primitive gdbm-close 1)))
  unspecific)