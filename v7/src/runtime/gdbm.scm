#| -*-Scheme-*-

$Id: gdbm.scm,v 1.9 2007/01/05 15:33:09 cph Exp $

Copyright 1996,1999,2000,2003 Massachusetts Institute of Technology

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

;;;; gdbm Database Library Interface
;;; package: (runtime gdbm)

(declare (usual-integrations))

(define gdbm-initialized? #f)
(define gdbf-finalizer)

(define (gdbm-available?)
  (load-library-object-file "prgdbm" #f)
  (and (implemented-primitive-procedure? (ucode-primitive gdbm-open 4))
       (begin
	 (if (not gdbm-initialized?)
	     (begin
	       (set! gdbf-finalizer
		     (make-gc-finalizer (ucode-primitive gdbm-close 1)
					gdbf?
					gdbf-descriptor
					set-gdbf-descriptor!))
	       (set! gdbm-initialized? #t)))
	 #t)))

;; Parameters to gdbm_open for READERS, WRITERS, and WRITERS who can
;; create the database.
(define GDBM_READER  0)		;A reader.
(define GDBM_WRITER  1)		;A writer.
(define GDBM_WRCREAT 2)		;A writer.  Create the db if needed.
(define GDBM_NEWDB   3)		;A writer.  Always create a new db.
(define GDBM_FAST    16)	;Write fast! => No fsyncs.

(define (gdbm-open filename block-size flags mode)
  (if (not (gdbm-available?))
      (error "No gdbm support in this sytem."))
  (let ((filename (->namestring (merge-pathnames filename))))
    (without-interrupts
     (lambda ()
       (add-to-gc-finalizer!
	gdbf-finalizer
	(make-gdbf (gdbm-error ((ucode-primitive gdbm-open 4)
				filename block-size flags mode))
		   filename))))))

(define (gdbm-close gdbf)
  (if (not (gdbf? gdbf))
      (error:wrong-type-argument gdbf "gdbm handle" 'GDBM-CLOSE))
  (remove-from-gc-finalizer! gdbf-finalizer gdbf))

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