#| -*-Scheme-*-

$Id: gdbm.scm,v 1.1 1996/04/23 20:37:04 cph Exp $

Copyright (c) 1996 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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
	   (add-to-protection-list! gdbf-list gdbf descriptor)
	   gdbf))))))

(define (gdbm-close gdbf)
  (if (not (gdbf? gdbf))
      (error:wrong-type-argument gdbf "gdbm handle" 'GDBM-CLOSE))
  (let ((descriptor (gdbf-descriptor gdbf)))
    (if descriptor
	(without-interrupts
	 (lambda ()
	   ((ucode-primitive gdbm-close 1) descriptor)
	   (remove-from-protection-list! gdbf-list gdbf)
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

(define gdbf-list)
(define (initialize-package!)
  (set! gdbf-list (make-protection-list))
  (add-gc-daemon!
   (lambda ()
     (clean-lost-protected-objects gdbf-list (ucode-primitive gdbm-close 1))))
  (add-event-receiver! event:after-restore
		       (lambda () (drop-all-protected-objects gdbf-list))))