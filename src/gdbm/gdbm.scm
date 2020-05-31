#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; The gdbm option.
;;; package: (gdbm)

(declare (usual-integrations))

(define (import-gdbm)
  (let ((target-environment (nearest-repl/environment))
	(source-environment (->environment '(gdbm))))
    (for-each (lambda (name)
		(link-variables target-environment name
				source-environment name))
	      '(gdbm-close
		gdbm-delete
		gdbm-exists?
		gdbm-fetch
		gdbm-firstkey
		gdbm-nextkey
		gdbm-open
		gdbm-reorganize
		gdbm-setopt
		gdbm-store
		gdbm-sync
		gdbm-version
		gdbm_cachesize
		gdbm_fast
		;;gdbm_fastmode obsolete
		gdbm_insert
		gdbm_newdb
		gdbm_reader
		gdbm_replace
		gdbm_wrcreat
		gdbm_writer))))

(C-include "gdbm")

(define-integrable (every-loop proc ref string start end)
  (let loop ((i start))
    (if (fix:< i end)
	(and (proc (ref string i))
	     (loop (fix:+ i 1)))
	#t)))

(define (->bytes string)
  ;; NOT necessarily null terminated
  (if (and (or (bytevector? string)
	       (and (ustring? string)
		    (fix:= 1 (ustring-cp-size string))))
	   (let ((end (string-length string)))
	     (every-loop (lambda (cp) (fix:< cp #x80))
			 cp1-ref string 0 end)))
      string
      (string->utf8 string)))

(declare (integrate-operator bytes-length))
(define (bytes-length bytes)
  (if (bytevector? bytes)
      (bytevector-length bytes)
      (string-length bytes)))

;; Parameters to gdbm_open for readers, writers, and writers who can
;; create the database.
(define gdbm_reader  (C-enum "GDBM_READER"))	;A reader.
(define gdbm_writer  (C-enum "GDBM_WRITER"))	;A writer.
(define gdbm_wrcreat (C-enum "GDBM_WRCREAT"))	;A writer.  Create the db if needed.
(define gdbm_newdb   (C-enum "GDBM_NEWDB"))	;A writer.  Always create a new db.
(define gdbm_fast    (C-enum "GDBM_FAST"))	;Write fast! => No fsyncs.

(define (gdbm-open filename block-size flags mode)
  (guarantee integer? block-size 'gdbm-open)
  (guarantee integer? mode 'gdbm-open)
  (let ((args (make-alien '|gdbm_args|))
	(flagsnum (guarantee-gdbm-open-flags flags))
	(filename (->namestring (merge-pathnames filename))))
    (let ((gdbf (make-gdbf args (make-thread-mutex) filename)))
      (add-open-gdbf-cleanup gdbf)
      (with-gdbf-locked
       gdbf
       (lambda ()
	 (C-call "do_gdbm_open"
		 args (string->utf8 filename) block-size flagsnum mode)
	 (if (alien-null? args)
	     (error "gdbm_open failed: malloc failed")
	     (if (alien-null? (C-> args "gdbm_args dbf"))
		 (gdbm-error gdbf "gdbm_open")))))
      gdbf)))

(define (guarantee-gdbm-open-flags flags)
  (define (flag->number flag)
    (case flag
      ((reader) (C-enum "GDBM_READER"))
      ((writer) (C-enum "GDBM_WRITER"))
      ((wrcreat) (C-enum "GDBM_WRCREAT"))
      ((newdb) (C-enum "GDBM_NEWDB"))
      ((fast) (C-enum "GDBM_FAST"))
      (else (error:wrong-type-argument flags "gdbm-open flags" 'gdbm-open))))
  (cond ((integer? flags) flags)
	((symbol? flags) (flag->number flags))
	((list-of-type? flags symbol?)
	 (reduce + 0 (map flag->number flags)))
	(else (error:wrong-type-argument flags "gdbm-open flags" 'gdbm-open))))

(define (gdbm-close gdbf)
  (guarantee-gdbf gdbf 'gdbm-close)
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
(define gdbm_insert  (C-enum "GDBM_INSERT"))	;Never replace old data.
(define gdbm_replace (C-enum "GDBM_REPLACE"))	;Always replace old data.

(define (gdbm-store gdbf key content flag)
  (guarantee-gdbf gdbf 'gdbm-store)
  (guarantee-nonnull-string key 'gdbm-store)
  (guarantee-nonnull-string content 'gdbm-store)
  (let ((flagnum (cond ((= flag gdbm_insert) flag)
		       ((= flag gdbm_replace) flag)
		       ((eq? flag 'insert) (C-enum "GDBM_INSERT"))
		       ((eq? flag 'replace) (C-enum "GDBM_REPLACE"))
		       (else (error:wrong-type-argument flag "gdbm-store flag"
							'gdbm-store)))))
    (with-gdbf-locked-open
     gdbf 'gdbm-store
     (lambda (args)
       (gdbf-args-put-key! args key)
       (gdbf-args-put-content! args content)
       (let ((ret (C-call "do_gdbm_store" args flagnum)))
	 (cond ((fix:zero? ret) #t)
	       ((fix:< 0 ret) #f)
	       (else (gdbm-error gdbf "gdbm_store"))))))))

(define (gdbm-fetch gdbf key)
  (guarantee-gdbf gdbf 'gdbm-fetch)
  (guarantee-nonnull-string key 'gdbm-fetch)
  (with-gdbf-locked-open
   gdbf 'gdbm-fetch
   (lambda (args)
     (gdbf-args-put-key! args key)
     (C-call "do_gdbm_fetch" args)
     (gdbf-args-get-content args))))

(define (gdbm-exists? gdbf key)
  (guarantee-gdbf gdbf 'gdbm-exists?)
  (guarantee-nonnull-string key 'gdbm-exists?)
  (with-gdbf-locked-open
   gdbf 'gdbm-exists
   (lambda (args)
     (gdbf-args-put-key! args key)
     (not (zero? (C-call "do_gdbm_exists" args))))))

(define (gdbm-delete gdbf key)
  (guarantee-gdbf gdbf 'gdbm-delete)
  (guarantee-nonnull-string key 'gdbm-delete)
  (with-gdbf-locked-open
   gdbf 'gdbm-delete
   (lambda (args)
     (gdbf-args-put-key! args key)
     (zero? (C-call "do_gdbm_delete" (gdbf-args gdbf))))))

(define (gdbm-keys gdbf)
  (guarantee-gdbf gdbf 'gdbm-keys)
  (with-gdbf-locked-open
   gdbf 'gdbm-keys
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
  (guarantee-gdbf gdbf 'gdbm-firstkey)
  (with-gdbf-locked-open
   gdbf 'gdbm-firstkey
   (lambda (args)
     (C-call "do_gdbm_firstkey" args)
     (gdbf-args-get-key args))))

(define (gdbm-nextkey gdbf key)
  ;; Returns #f if key is not (or no longer) in the database.  Use
  ;; gdbm-keys to read a complete list despite deletes.  Gdbm-keys
  ;; also avoids copying the keys back for gdbm_nextkey.
  (guarantee-gdbf gdbf 'gdbm-nextkey)
  (guarantee-nonnull-string key 'gdbm-nextkey)
  (with-gdbf-locked-open
   gdbf 'gdbm-nextkey
   (lambda (args)
     (gdbf-args-put-key! args key)
     (if (zero? (C-call "do_gdbm_nextkey" args))
	 (gdbf-args-get-key args)
	 #f))))

(define (gdbm-reorganize gdbf)
  (guarantee-gdbf gdbf 'gdbm-reorganize)
  (with-gdbf-locked-open
   gdbf 'gdbm-reorganize
   (lambda (args)
     (if (not (zero? (C-call "do_gdbm_reorganize" args)))
	 (gdbm-error gdbf "gdbm_reorganize")))))

(define (gdbm-sync gdbf)
  (guarantee-gdbf gdbf 'gdbm-sync)
  (with-gdbf-locked-open
   gdbf 'gdbm-sync
   (lambda (args)
     (C-call "do_gdbm_sync" args))))

(define (gdbm-strerror errno)
  (guarantee fixnum? errno 'gdbm-strerror)
  (c-peek-cstring (C-call "gdbm_strerror" (make-alien '(* char)) errno)))

(define (strerror errno)
  (guarantee fixnum? errno 'strerror)
  (c-peek-cstring (C-call "strerror" (make-alien '(* char)) errno)))

;; Parameters to gdbm_setopt, specifing the type of operation to perform.
(define gdbm_cachesize (C-enum "GDBM_CACHESIZE"))	;Set the cache size.
(define gdbm_syncmode  (C-enum "GDBM_SYNCMODE"))	;Toggle fast mode.

(define (gdbm-setopt gdbf opt val)
  (guarantee-gdbf gdbf 'gdbm-setopt)
  (let* ((optnum
	  (cond ((eq? opt 'syncmode) (C-enum "GDBM_SYNCMODE"))
		((eq? opt 'cachesize) (C-enum "GDBM_CACHESIZE"))
		((and (number? opt) (= opt gdbm_syncmode)) opt)
		((and (number? opt) (= opt gdbm_cachesize)) opt)
		(else (error:wrong-type-argument opt "option" 'gdbm-setopt))))
	 (valnum
	  (cond ((= optnum gdbm_syncmode)
		 (cond ((eq? val #f) 0)
		       ((eq? val #t) 1)
		       ((zero? val) val)
		       ((= val 1) val)
		       (else (error:wrong-type-argument val "syncmode"
							'gdbm-setopt))))
		((= optnum gdbm_cachesize)
		 (guarantee integer? val 'gdbm-setopt)
		 val))))
    (with-gdbf-locked-open
     gdbf 'gdbm-setopt
     (lambda (args)
       (if (not (zero? (C-call "do_gdbm_setopt" args optnum valnum)))
	   (gdbm-error gdbf "gdbm_setopt"))))))

(define (gdbm-version)
  (c-peek-cstring (C-call "get_gdbm_version" (make-alien '(* char)))))

(define (guarantee-nonnull-string obj procedure)
  (guarantee string? obj procedure)
  (if (string-null? obj)
      (error:wrong-type-argument obj "non-null string" procedure)))

(define-structure (gdbf (constructor make-gdbf)
			(print-procedure
			 (standard-print-method 'gdbf
			   (lambda (gdbf)
			     (list (gdbf-filename gdbf))))))
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
  (with-thread-mutex-lock (gdbf-mutex gdbf) thunk))

(define (with-gdbf-locked-open gdbf operator receiver)
  (with-thread-mutex-lock
   (gdbf-mutex gdbf)
   (lambda ()
     (let ((args (gdbf-args gdbf)))
       (if (alien-null? args)
	   (error (string-append (symbol->string operator) " failed: closed")))
       (receiver args)))))

(define (gdbm-error gdbf msg)
  (let ((args (gdbf-args gdbf)))
    (error (string-append msg " failed:")
	   (gdbm-strerror (C-> args "gdbm_args errno_gdbm"))
	   (strerror (C-> args "gdbm_args errno_sys")))))

(define (gdbf-args-put-key! args key)
  (let ((bytes (->bytes key)))
    (let ((size (bytes-length bytes))
	  (dptr (make-alien '(* char))))
      (if (< size 1)
	  (error "empty key:" key))
      (C-call "alloc_gdbm_key" dptr args size)
      (if (alien-null? dptr)
	  (error "gdbf-args-put-key!: malloc failed" key))
      (c-poke-bytes dptr 0 size bytes 0))))

(define (gdbf-args-put-content! args content)
  (let ((bytes (->bytes content)))
    (let ((size (bytes-length bytes))
	  (dptr (make-alien '(* char))))
      (if (< size 1)
	  (error "empty content:" content))
      (C-call "alloc_gdbm_content" dptr args size)
      (if (alien-null? dptr)
	  (error "gdbf-args-put-content!: malloc failed" size))
      (c-poke-bytes dptr 0 size bytes 0))))

(define (gdbf-args-get-key args)
  (let ((data (C-> args "gdbm_args key dptr")))
    (if (alien-null? data)
	#f
	(let* ((size (C-> args "gdbm_args key dsize"))
	       (bytes ((ucode-primitive c-peek-csubstring 3) data 0 size)))
	  (if (string? bytes)
	      bytes
	      (let ((s (utf8->string bytes)))
		(outf-error ";got a utf8 key: "s"\n")
		s))))))

(define (gdbf-args-get-content args)
  (let ((data (C-> args "gdbm_args content dptr")))
    (if (alien-null? data)
	#f
	(let* ((size (C-> args "gdbm_args content dsize"))
	       (bytes ((ucode-primitive c-peek-csubstring 3) data 0 size)))
	  (if (string? bytes)
	      bytes
	      (let ((s (utf8->string bytes)))
		(outf-error ";got utf8 content: "s"\n")
		s))))))

(define open-gdbfs '())
(define open-gdbfs-mutex)

(define (add-open-gdbf-cleanup gdbf)
  (with-thread-mutex-lock open-gdbfs-mutex
    (lambda ()
      (set! open-gdbfs
	    (cons (weak-cons gdbf (gdbf-args gdbf))
		  open-gdbfs)))))

(define (remove-open-gdbf-cleanup gdbf)
  (with-thread-mutex-lock open-gdbfs-mutex
    (lambda ()
      (let ((entry (weak-assq gdbf open-gdbfs)))
	(if entry
	    (set! open-gdbfs (delq! entry open-gdbfs)))))))

(define (cleanup-open-gdbfs)
  (with-thread-mutex-try-lock
   open-gdbfs-mutex
   (lambda ()
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
		   (loop next prev)))))))
   (lambda ()
     unspecific)))

(define (reset-open-gdbfs)
  (for-each (lambda (weak) (alien-null! (weak-cdr weak))) open-gdbfs)
  (set! open-gdbfs '()))

(define (initialize-package!)
  (set! open-gdbfs-mutex (make-thread-mutex))
  (set! open-gdbfs '())
  (add-gc-daemon! cleanup-open-gdbfs)
  (add-event-receiver! event:after-restart reset-open-gdbfs))