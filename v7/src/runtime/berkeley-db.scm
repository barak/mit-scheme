#| -*-Scheme-*-

$Id: berkeley-db.scm,v 1.1 2005/01/11 03:12:26 cph Exp $

Copyright 2004,2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Berkeley DB Interface
;;; package: (runtime berkeley-db)

(declare (usual-integrations))

(define-primitives
  (db4:db-close 2)
  (db4:db-create 3)
  (db4:db-del 4)
  (db4:db-env-close 2)
  (db4:db-env-create 3)
  (db4:db-env-get-home 2)
  (db4:db-env-get-open-flags 2)
  (db4:db-env-lock-get 6)
  (db4:db-env-lock-id 2)
  (db4:db-env-lock-id-free 2)
  (db4:db-env-lock-put 2)
  (db4:db-env-open 4)
  (db4:db-env-txn-begin 4)
  (db4:db-get 5)
  (db4:db-get-dbname 2)
  (db4:db-get-env 2)
  (db4:db-get-open-flags 2)
  (db4:db-get-pagesize 2)
  (db4:db-get-transactional 2)
  (db4:db-get-type 2)
  (db4:db-open 7)
  (db4:db-put 5)
  (db4:db-strerror 1)
  (db4:db-txn-abort 1)
  (db4:db-txn-commit 2)
  (db4:dbt-size 1)
  (db4:init-dbt 4)
  (db4:name->rc 1)
  (db4:rc->name 1)
  (db4:sizeof-db-lock 0)
  (db4:sizeof-dbt 0))

(define-integrable DB_CXX_NO_EXCEPTIONS	#x00000002)
(define-integrable DB_FORCE		#x00000004)
(define-integrable DB_NOMMAP		#x00000008)
(define-integrable DB_RDONLY		#x00000010)
(define-integrable DB_RECOVER		#x00000020)
(define-integrable DB_THREAD		#x00000040)
(define-integrable DB_TRUNCATE		#x00000080)
(define-integrable DB_TXN_NOSYNC	#x00000100)
(define-integrable DB_TXN_NOT_DURABLE	#x00000200)
(define-integrable DB_USE_ENVIRON	#x00000400)
(define-integrable DB_USE_ENVIRON_ROOT	#x00000800)
(define-integrable DB_AUTO_COMMIT	#x01000000)
(define-integrable DB_DIRTY_READ	#x02000000)
(define-integrable DB_NO_AUTO_COMMIT	#x04000000)

;; Flags for DB4:DB-ENV-CREATE
(define-integrable DB_RPCCLIENT		#x00000001)

;; Flags for DB4:DB-CREATE
(define-integrable DB_REP_CREATE	#x00000001)
(define-integrable DB_XA_CREATE		#x00000002)

;; Flags for DB4:DB-ENV-OPEN
(define-integrable DB_INIT_CDB		#x00001000)
(define-integrable DB_INIT_LOCK		#x00002000)
(define-integrable DB_INIT_LOG		#x00004000)
(define-integrable DB_INIT_MPOOL	#x00008000)
(define-integrable DB_INIT_REP		#x00010000)
(define-integrable DB_INIT_TXN		#x00020000)
(define-integrable DB_JOINENV		#x00040000)
(define-integrable DB_LOCKDOWN		#x00080000)
(define-integrable DB_PRIVATE		#x00100000)
(define-integrable DB_RECOVER_FATAL	#x00200000)
(define-integrable DB_SYSTEM_MEM	#x00400000)

;; Flags for DB4:DB-OPEN
(define-integrable DB_EXCL		#x00001000)
(define-integrable DB_FCNTL_LOCKING	#x00002000)
(define-integrable DB_RDWRMASTER	#x00004000)
(define-integrable DB_WRITEOPEN		#x00008000)

;; Flags for DB4:DB-ENV-TXN-BEGIN
(define-integrable DB_TXN_NOWAIT	#x00001000)
(define-integrable DB_TXN_SYNC		#x00002000)

;; Flags for DB4:DB-GET, DB4:DB-PUT, DB4:DB-DEL
#;(define-integrable DB_DIRTY_READ	#x02000000)
(define-integrable DB_MULTIPLE		#x04000000)
(define-integrable DB_MULTIPLE_KEY	#x08000000)
(define-integrable DB_RMW		#x10000000)

;; db_locktype_t enumeration:
(define-integrable DB_LOCK_NG 0)
(define-integrable DB_LOCK_READ 1)
(define-integrable DB_LOCK_WRITE 2)
(define-integrable DB_LOCK_WAIT 3)
(define-integrable DB_LOCK_IWRITE 4)
(define-integrable DB_LOCK_IREAD 5)
(define-integrable DB_LOCK_IWR 6)
(define-integrable DB_LOCK_DIRTY 7)
(define-integrable DB_LOCK_WWRITE 8)

(define-syntax pcall
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(identifier * expression) (cdr form))
	 `(LET ((RC
		 (,(close-syntax (cadr form) environment)
		  ,@(map (lambda (expr)
			   (close-syntax expr environment))
			 (cddr form))))))
	    (IF (NOT (= RC 0))
		(BDB-ERROR RC ',(cadr form)))))))

(define condition-type:bdb-error
  (make-condition-type 'BDB-ERROR condition-type:error '(RC PRIMITIVE)
    (lambda (condition port)
      (let ((rc (access-condition condition 'RC)))
	(write-string "Berkeley DB error in primitive " port)
	(write (access-condition condition 'PRIMITIVE) port)
	(write-string ": " port)
	(write-string (db4:db-strerror rc) port)
	(write-string " (" port)
	(write (or (db4:rc->name rc) rc) port)
	(write-string ")." port)))))

(define bdb-error
  (condition-signaller condition-type:bdb-error
		       '(RC PRIMITIVE)
		       standard-error-handler))

(define-record-type <bdb>
    (make-bdb handle)
    bdb?
  (handle bdb-handle set-bdb-handle!))

(define-record-type <bdb-env>
    (make-bdb-env handle)
    bdb-env?
  (handle bdb-env-handle set-bdb-env-handle!)
  (ids bdb-env-ids))

(define-record-type <bdb-txn>
    (make-bdb-txn handle)
    bdb-txn?
  (handle bdb-txn-handle set-bdb-txn-handle!))

(define-record-type <bdb-id>
    (make-bdb-id handle)
    bdb-id?
  (handle bdb-id-handle set-bdb-id-handle!))

(define interface-initialized? #f)
(define dbs)
(define envs)
(define txns)
(define dbt-length)
(define db-lock-length)

(define (bdb-available?)
  (load-library-object-file "prdb4" #f)
  (and (implemented-primitive-procedure? db4:db-create)
       (begin
	 (if (not interface-initialized?)
	     (begin
	       (set! dbs
		     (make-gc-finalizer db4:db-close
					bdb?
					bdb-handle
					set-bdb-handle!))
	       (set! envs
		     (make-gc-finalizer db4:db-env-close
					bdb-env?
					bdb-env-handle
					set-bdb-env-handle!))
	       (set! txns
		     (make-gc-finalizer db4:db-txn-abort
					bdb-txn?
					bdb-txn-handle
					set-bdb-txn-handle!))
	       (set! dbt-length (db4:sizeof-dbt))
	       (set! db-lock-length (db4:sizeof-db-lock))
	       (set! interface-initialized? #t)))
	 #t)))

(define (guarantee-bdb-available)
  (if (not (bdb-available?))
      (error "No Berkeley DB support in this sytem.")))

(define (create-bdb env flags)
  (guarantee-bdb-available)
  (make-gc-finalized-object dbs
			    (lambda (p)
			      (pcall db4:db-create
				     (and env (bdb-env-handle env))
				     flags
				     p))
			    make-bdb))

(define (open-bdb db txn filename db-name type flags mode)
  (pcall db4:db-open
	 (bdb-handle db)
	 (and txn (bdb-txn-handle txn))
	 (and filename (->namestring (merge-pathnames filename)))
	 db-name
	 type
	 flags
	 mode))

(define (bdb-names db)
  (let ((p (cons #f #f)))
    (pcall db4:db-get-dbname (bdb-handle db) p)
    (values (car p) (cdr p))))

(define (bdb-open-flags db)
  (let ((p (cons #f #f)))
    (pcall db4:db-get-open-flags (bdb-handle db) p)
    (car p)))

(define (bdb-transactional? db)
  (let ((p (cons #f #f)))
    (pcall db4:db-get-transactional (bdb-handle db) p)
    (car p)))

(define (close-bdb db flags)
  (pcall db4:db-close (bdb-handle db) flags))

(define (string->dbt string)
  (let ((dbt (make-dbt)))
    (db4:init-dbt dbt string #f #f)
    dbt))

(define (string->dbt-partial string start length)
  (let ((dbt (make-dbt)))
    (db4:init-dbt dbt string start length)
    dbt))

(define (make-dbt)
  (make-string dbt-length))

(define rc:db_notfound
  (db4:name->rc 'db_notfound))

(define rc:enomem
  (db4:name->rc 'enomem))

(define (bdb-get db txn key flags)
  (let ((db (bdb-handle db))
	(txn (and txn (bdb-txn-handle txn)))
	(key (string->dbt key))
	(datum (make-dbt)))
    (db4:init-dbt datum "" #f #f)
    (let ((rc (db4:db-get db txn key datum flags)))
      (cond ((= rc rc:db_notfound)
	     #f)
	    ((= rc rc:enomem)
	     (let ((string (make-string (db4:dbt-size datum))))
	       (db4:init-dbt datum string #f #f)
	       (pcall db4:db-get db txn key datum flags)
	       string))
	    ((= rc 0)
	     (make-string 0))
	    (else
	     (bdb-error rc 'db4:db-get))))))

(define (bdb-get-partial db txn key flags start length)
  (let ((string (make-string length)))
    (let ((rc
	   (db4:db-get (bdb-handle db)
		       (and txn (bdb-txn-handle txn))
		       (string->dbt key)
		       (string->dbt-partial string start length)
		       flags)))
      (cond ((= rc 0) string)
	    ((= rc rc:db_notfound) #f)
	    (else (bdb-error rc 'db4:db-get))))))

(define (bdb-put db txn key datum flags)
  (pcall db4:db-put
	 (bdb-handle db)
	 (and txn (bdb-txn-handle txn))
	 (string->dbt key)
	 (string->dbt datum)
	 flags))

(define (bdb-put-partial db txn key datum flags start length)
  (pcall db4:db-put
	 (bdb-handle db)
	 (and txn (bdb-txn-handle txn))
	 (string->dbt key)
	 (string->dbt-partial datum start length)
	 flags))

(define (bdb-delete db txn key flags)
  (let ((rc
	 (db4:db-del (bdb-handle db)
		     (and txn (bdb-txn-handle txn))
		     (string->dbt key)
		     flags)))
    (cond ((= rc 0) #t)
	  ((= rc rc:db_notfound) #f)
	  (else (bdb-error rc 'db4:db-del)))))

(define (create-bdb-env flags)
  (guarantee-bdb-available)
  (make-gc-finalized-object
   envs
   (lambda (p) (pcall db4:db-env-create flags p))
   (lambda (handle)
     (make-bdb-env handle
		   (make-gc-finalizer (lambda (id)
					(db4:db-env-lock-id-free handle id))
				      bdb-id?
				      bdb-id-handle
				      set-bdb-id-handle!)))))

(define (open-bdb-env env home flags mode)
  (pcall db4:db-open
	 (bdb-env-handle env)
	 (->namestring (merge-pathnames home))
	 flags
	 mode))

(define (bdb-env-home env)
  (let ((p (cons #f #f)))
    (pcall db4:db-env-get-home (bdb-env-handle env) p)
    (car p)))

(define (bdb-env-open-flags env)
  (let ((p (cons #f #f)))
    (pcall db4:db-env-get-open-flags (bdb-env-handle env) p)
    (car p)))

(define (close-bdb-env env flags)
  (pcall db4:db-env-close (bdb-env-handle env) flags))

(define (bdb-env-lock-id env)
  (make-gc-finalized-object
   (bdb-env-ids env)
   (lambda (p) (pcall db4:db-env-lock-id (bdb-env-handle env) p))
   make-bdb-id))

(define (bdb-env-lock-id-free env id)
  (pcall db4:db-env-lock-id-free (bdb-env-handle env) id))

(define (bdb-env-lock-get env id flags object lock-mode)
  (let ((lock (make-string db-lock-length)))
    (pcall db4:db-env-lock-get
	   (bdb-env-handle env)
	   id
	   flags
	   (string->dbt object)
	   lock-mode
	   lock)
    lock))

(define (bdb-env-lock-put env lock)
  (pcall db4:db-env-lock-get (bdb-env-handle env) lock))

(define (bdb-env-txn-begin env txn flags)
  (make-gc-finalized-object
   txns
   (lambda (p)
     (pcall db4:db-env-txn-begin
	    (bdb-env-handle env)
	    (and txn (bdb-txn-handle txn))
	    flags
	    p))
   make-bdb-txn))

(define (bdb-txn-commit txn flags)
  (pcall db4:db-txn-commit (bdb-txn-handle txn) flags))

(define (bdb-txn-abort txn)
  (pcall db4:db-txn-abort (bdb-txn-handle txn)))