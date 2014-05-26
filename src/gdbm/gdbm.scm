#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

;;;; GDBM wrapper
;;; package: (gdbm)

(declare (usual-integrations))

(C-include "gdbm")

(define (gdbm-available?)
  (let ((path (ignore-errors (lambda ()
			       (system-library-pathname "gdbm-shim.so")))))
    (and (pathname? path)
	 (file-loadable? path))))

;; Parameters to gdbm_open for READERS, WRITERS, and WRITERS who can
;; create the database.
(define GDBM_READER (C-enum "GDBM_READER"))	;A reader.
(define GDBM_WRITER (C-enum "GDBM_WRITER"))	;A writer.
(define GDBM_WRCREAT(C-enum "GDBM_WRCREAT"))	;A writer.  Create the db if needed.
(define GDBM_NEWDB  (C-enum "GDBM_NEWDB"))	;A writer.  Always create a new db.
(define GDBM_FAST   (C-enum "GDBM_FAST"))	;Write fast! => No fsyncs.

(define (gdbm-open filename block-size flags mode)
  (guarantee-integer block-size 'GDBM-OPEN)
  (guarantee-integer mode 'GDBM-OPEN)
  (let ((args (make-alien '|gdbm_args|))
	(flagsnum (guarantee-gdbm-open-flags flags)))
    (let ((gdbf (make-gdbf args (make-thread-mutex) filename)))
      (if (not (gdbm-available?))
	  (error "GDBM support is not installed."))
      (add-open-gdbf-cleanup gdbf)
      (with-gdbf-locked
       gdbf
       (lambda ()
	 (C-call "do_gdbm_open" args filename block-size flagsnum mode)
	 (if (alien-null? args)
	     (error "gdbm_open failed: malloc failed")
	     (if (alien-null? (C-> args "gdbm_args dbf"))
		 (gdbm-error gdbf "gdbm_open")))))
      gdbf)))

(define (guarantee-gdbm-open-flags flags)
  (define (flag->number flag)
    (case flag
      ((READER) (C-enum "GDBM_READER"))
      ((WRITER) (C-enum "GDBM_WRITER"))
      ((WRCREAT) (C-enum "GDBM_WRCREAT"))
      ((NEWDB) (C-enum "GDBM_NEWDB"))
      ((FAST) (C-enum "GDBM_FAST"))
      (else (error:wrong-type-argument flags "gdbm-open flags" 'GDBM-OPEN))))
  (cond ((integer? flags) flags)
	((symbol? flags) (flag->number flags))
	((list-of-type? flags symbol?)
	 (reduce + 0 (map flag->number flags)))
	(else (error:wrong-type-argument flags "gdbm-open flags" 'GDBM-OPEN))))

(define (gdbm-close gdbf)
  (guarantee-gdbf gdbf 'GDBM-CLOSE)
  (with-gdbf-locked
   gdbf
   (lambda ()
     (let ((args (gdbf-args gdbf)))
       (if (not (alien-null? args))
	   (begin
	     (C-call "do_gdbm_close" args)
	     (alien-null! args)
	     (remove-open-gdbf-cleanup gdbf))))
     unspecific)))

;; Parameters to gdbm_store for simple insertion or replacement in the
;; case that the key is already in the database.
(define GDBM_INSERT  (C-enum "GDBM_INSERT"))	;Never replace old data.
(define GDBM_REPLACE (C-enum "GDBM_REPLACE"))	;Always replace old data.

(define (gdbm-store gdbf key content flag)
  (guarantee-gdbf gdbf 'GDBM-STORE)
  (guarantee-nonnull-string key 'GDBM-STORE)
  (guarantee-nonnull-string content 'GDBM-STORE)
  (let ((flagnum (cond ((= flag GDBM_INSERT) flag)
		       ((= flag GDBM_REPLACE) flag)
		       ((eq? flag 'INSERT) (C-enum "GDBM_INSERT"))
		       ((eq? flag 'REPLACE) (C-enum "GDBM_REPLACE"))
		       (else (error:wrong-type-argument flag "gdbm-store flag"
							'GDBM-STORE)))))
    (with-gdbf-locked-open
     gdbf 'GDBM-STORE
     (lambda (args)
       (gdbf-args-put-key! args key)
       (gdbf-args-put-content! args content)
       (let ((ret (C-call "do_gdbm_store" args flagnum)))
	 (cond ((fix:zero? ret) #t)
	       ((fix:< 0 ret) #f)
	       (else (gdbm-error gdbf "gdbm_store"))))))))

(define (gdbm-fetch gdbf key)
  (guarantee-gdbf gdbf 'GDBM-FETCH)
  (guarantee-nonnull-string key 'GDBM-FETCH)
  (with-gdbf-locked-open
   gdbf 'GDBM-FETCH
   (lambda (args)
     (gdbf-args-put-key! args key)
     (C-call "do_gdbm_fetch" args)
     (gdbf-args-get-content args))))

(define (gdbm-exists? gdbf key)
  (guarantee-gdbf gdbf 'GDBM-EXISTS?)
  (guarantee-nonnull-string key 'GDBM-EXISTS?)
  (with-gdbf-locked-open
   gdbf 'GDBM-EXISTS
   (lambda (args)
     (gdbf-args-put-key! args key)
     (not (zero? (C-call "do_gdbm_exists" args))))))

(define (gdbm-delete gdbf key)
  (guarantee-gdbf gdbf 'GDBM-DELETE)
  (guarantee-nonnull-string key 'GDBM-DELETE)
  (with-gdbf-locked-open
   gdbf 'GDBM-DELETE
   (lambda (args)
     (gdbf-args-put-key! args key)
     (zero? (C-call "do_gdbm_delete" (gdbf-args gdbf))))))

(define (gdbm-keys gdbf)
  (guarantee-gdbf gdbf 'GDBM-KEYS)
  (with-gdbf-locked-open
   gdbf 'GDBM-KEYS
   (lambda (args)
     (C-call "do_gdbm_firstkey" args)
     (let ((key (gdbf-args-get-key args)))
       (if (not key)
	   '()
	   (let loop ((keys (list key)))
	     (if (zero? (C-call "do_gdbm_nextkey" args))
		 (loop (cons (gdbf-args-get-key args) keys))
		 keys)))))))

(define (gdbm-firstkey gdbf)
  (guarantee-gdbf gdbf 'GDBM-FIRSTKEY)
  (with-gdbf-locked-open
   gdbf 'GDBM-FIRSTKEY
   (lambda (args)
     (C-call "do_gdbm_firstkey" args)
     (gdbf-args-get-key args))))

(define (gdbm-nextkey gdbf key)
  ;; Returns #f if KEY is not (or no longer) in the database.  Use
  ;; gdbm-keys to read a complete list despite deletes.  Gdbm-keys
  ;; also avoids copying the keys back for gdbm_nextkey.
  (guarantee-gdbf gdbf 'GDBM-NEXTKEY)
  (guarantee-nonnull-string key 'GDBM-NEXTKEY)
  (with-gdbf-locked-open
   gdbf 'GDBM-NEXTKEY
   (lambda (args)
     (gdbf-args-put-key! args key)
     (if (zero? (C-call "do_gdbm_nextkey" args))
	 (gdbf-args-get-key args)
	 #f))))

(define (gdbm-reorganize gdbf)
  (guarantee-gdbf gdbf 'GDBM-REORGANIZE)
  (with-gdbf-locked-open
   gdbf 'GDBM-REORGANIZE
   (lambda (args)
     (if (not (zero? (C-call "do_gdbm_reorganize" args)))
	 (gdbm-error gdbf "gdbm_reorganize")))))

(define (gdbm-sync gdbf)
  (guarantee-gdbf gdbf 'GDBM-SYNC)
  (with-gdbf-locked-open
   gdbf 'GDBM-SYNC
   (lambda (args)
     (C-call "do_gdbm_sync" args))))

(define (gdbm-strerror errno)
  (guarantee-fixnum errno 'GDBM-STRERROR)
  (c-peek-cstring (C-call "gdbm_strerror" (make-alien '(* char)) errno)))

(define (strerror errno)
  (guarantee-fixnum errno 'STRERROR)
  (c-peek-cstring (C-call "strerror" (make-alien '(* char)) errno)))

;; Parameters to gdbm_setopt, specifing the type of operation to perform.
(define GDBM_CACHESIZE (C-enum "GDBM_CACHESIZE"))	;Set the cache size.
(define GDBM_SYNCMODE  (C-enum "GDBM_SYNCMODE"))	;Toggle fast mode.

(define (gdbm-setopt gdbf opt val)
  (guarantee-gdbf gdbf 'GDBM-SETOPT)
  (let* ((optnum
	  (cond ((eq? opt 'SYNCMODE) (C-enum "GDBM_SYNCMODE"))
		((eq? opt 'CACHESIZE) (C-enum "GDBM_CACHESIZE"))
		((and (number? opt) (= opt GDBM_SYNCMODE)) opt)
		((and (number? opt) (= opt GDBM_CACHESIZE)) opt)
		(else (error:wrong-type-argument opt "option" 'GDBM-SETOPT))))
	 (valnum
	  (cond ((= optnum GDBM_SYNCMODE)
		 (cond ((not val) 0)
		       ((eq? val #t) 1)
		       ((zero? val) val)
		       ((= val 1) val)
		       (else (error:wrong-type-argument val "SYNCMODE boolean"
							'GDBM-SETOPT))))
		((= optnum GDBM_CACHESIZE)
		 (guarantee-integer val 'GDBM-SETOPT)
		 val))))
    (with-gdbf-locked-open
     gdbf 'GDBM-SETOPT
     (lambda (args)
       (if (not (zero? (C-call "do_gdbm_setopt" args optnum valnum)))
	   (gdbm-error gdbf "gdbm_setopt"))))))

(define (gdbm-version)
  (c-peek-cstring (C-call "get_gdbm_version" (make-alien '(* char)))))

(define (guarantee-nonnull-string obj procedure)
  (if (or (not (string? obj)) (string-null? obj))
      (error:wrong-type-argument obj "non-null string" procedure)))

(define-structure (gdbf (constructor make-gdbf)
			(print-procedure
			 (standard-unparser-method
			  'GDBF
			  (lambda (gdbf port)
			    (write-char #\space port)
			    (write (gdbf-filename gdbf) port)))))
  ;; Note that communicating through this malloced-per-GDBM_FILE
  ;; helper struct assumes there are no callbacks possible during gdbm
  ;; operations (via which this procedure could be called multiple
  ;; times [requiring a malloc per operation]).  The per-gdbf lock is
  ;; probably already be poised to deadlock any thread trying it.
  (args #f read-only #t)
  (mutex #f read-only #t)
  (filename #f read-only #t))

(define (guarantee-gdbf gdbf procedure)
  (if (gdbf? gdbf)
      (or (not (alien-null? (gdbf-args gdbf)))
	  (error:bad-range-argument gdbf procedure))
      (error:wrong-type-argument gdbf "gdbm handle" procedure)))

(define-integrable (with-gdbf-locked gdbf thunk)
  (with-thread-mutex-locked (gdbf-mutex gdbf) thunk))

(define (with-gdbf-locked-open gdbf operator receiver)
  (with-thread-mutex-locked
   (gdbf-mutex gdbf)
   (lambda ()
     (let ((args (gdbf-args gdbf)))
       (if (alien-null? args)
	   (error (string-append (symbol-name operator) " failed: closed")))
       (receiver args)))))

(define (gdbm-error gdbf msg)
  (let ((args (gdbf-args gdbf)))
    (error (string-append msg " failed:")
	   (gdbm-strerror (C-> args "gdbm_args gdbm_errno"))
	   (strerror (C-> args "gdbm_args sys_errno")))))

(define (gdbf-args-put-key! args key)
  (let ((size (string-length key))
	(dptr (make-alien '(* char))))
    (if (< size 1)
	(error "empty key:" key))
    (C-call "alloc_gdbm_key" dptr args size)
    (if (alien-null? dptr)
	(error "gdbf-args-put-key!: malloc failed" key))
    (c-poke-bytes dptr 0 size key 0)))

(define (gdbf-args-put-content! args content)
  (let ((size (string-length content))
	(dptr (make-alien '(* char))))
    (if (< size 1)
	(error "empty content:" content))
    (C-call "alloc_gdbm_content" dptr args size)
    (if (alien-null? dptr)
	(error "gdbf-args-put-content!: malloc failed" size))
    (c-poke-bytes dptr 0 size content 0)))

(define (gdbf-args-get-key args)
  (let ((data (C-> args "gdbm_args key dptr")))
    (if (alien-null? data)
	#f
	(let* ((size (C-> args "gdbm_args key dsize"))
	       (string (string-allocate size)))
	  (c-peek-bytes data 0 size string 0)
	  string))))

(define (gdbf-args-get-content args)
  (let ((data (C-> args "gdbm_args content dptr")))
    (if (alien-null? data)
	#f
	(let* ((size (C-> args "gdbm_args content dsize"))
	       (string (string-allocate size)))
	  (c-peek-bytes data 0 size string 0)
	  string))))

(define open-gdbfs '())
(define open-gdbfs-mutex)

(define (add-open-gdbf-cleanup gdbf)
  (with-thread-mutex-locked
   open-gdbfs-mutex
   (lambda ()
     (set! open-gdbfs (cons (weak-cons gdbf (gdbf-args gdbf))
			    open-gdbfs)))))

(define (remove-open-gdbf-cleanup gdbf)
  (with-thread-mutex-locked
   open-gdbfs-mutex
   (lambda ()
     (let ((entry (weak-assq gdbf open-gdbfs)))
       (if entry
	   (set! open-gdbfs (delq! entry open-gdbfs)))))))

(define (weak-assq obj alist)
  (let loop ((alist alist))
    (if (null? alist) #f
	(let* ((entry (car alist))
	       (key (weak-car entry)))
	  (if (eq? obj key) entry
	      (loop (cdr alist)))))))

(define (cleanup-open-gdbfs)
  (if (not (thread-mutex-owner open-gdbfs-mutex))
      (let loop ((entries open-gdbfs)
		 (prev #f))
	(if (pair? entries)
	    (let ((entry (car entries))
		  (next (cdr entries)))
	      (if (weak-pair/car? entry)
		  (loop next entries)
		  (let ((args (weak-cdr entry)))
		    (if prev
			(set-cdr! prev next)
			(set! open-gdbfs next))
		    (if (not (alien-null? args))
			(begin
			  (C-call "do_gdbm_close" args)
			  (alien-null! args)))
		    (loop next prev))))))))

(define (reset-open-gdbfs)
  (for-each (lambda (weak) (alien-null! (weak-cdr weak))) open-gdbfs)
  (set! open-gdbfs '()))

(define (initialize-package!)
  (set! open-gdbfs-mutex (make-thread-mutex))
  (set! open-gdbfs '())
  (add-gc-daemon! cleanup-open-gdbfs)
  (add-event-receiver! event:after-restart reset-open-gdbfs))