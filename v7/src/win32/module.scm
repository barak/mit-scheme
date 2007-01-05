#| -*-Scheme-*-

$Id: module.scm,v 1.7 2007/01/05 15:33:10 cph Exp $

Copyright (c) 1993, 1999 Massachusetts Institute of Technology

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

;;;; Scheme interface to win32 modules
;;; package: (win32)

(declare (usual-integrations))

;;
;; Scheme level operations for windows modules (.EXEs and .DLLs)
;; All this gubbins is needed for calling functions in DLLs, especially
;; the case when a band is loaded and the DLLs have been mapped to a new
;; place in the address space, or even worse, the DLLs can't be loaded
;; (e.g. not enough memory or the files can't be found).


;; Package: (win32 module)

(define-structure
  (module
    (conc-name module/)
    (constructor %make-module)
    (print-procedure
     (unparser/standard-method 'MODULE
       (lambda (state module)
	 (unparse-object state (module/load-name module))))))
  load-name
  handle
  entries	;; a protection list of all the functions from this module
  finalization-info
)

;; handle      entry/address  description
;;   integer?    integer?       module loaded, handle valid, address valid
;;   integer?    #f             module loaded, handle valid, entry absent
;;   'bogus      #f             could not load module
;;   'unloaded   #f             should autoload module
;;
;; Thus if a module is loaded then all the entries hand been found or marked
;; absent.

;; The set of modules we know about
;; --------------------------------
;; The modules are kept in a protection list.  The associated finalization
;; is a cell containing the handle if the module is curremtly loaded
;; (by load-library) and thus requires a call to free-library, otherwise it
;; contains a non-handle.

(define *modules*)

(define (*modules*/lookup load-name)  ;; -> #f or module
  (protection-list/find
    (lambda (module) (string-ci=? load-name (module/load-name module)))
    *modules*))

(define (make-module load-name handle)
  (let* ((finfo  (make-cell handle))
         (module (%make-module
	           load-name handle (make-entries-list) finfo)))
    (protection-list/add! *modules* module finfo)
    module))

;;------------------------------------------------------------------------------

(define (find-module load-name)
  ;; find a linked in module.  Return module if found or linked, otherwise #f
  (let  ((module (*modules*/lookup load-name)))
    (or module
        (let ((handle  (load-library load-name)))
	  (if (= handle 0)
	      (make-module load-name 'bogus)
	      (make-module load-name handle))))))
    

(define (unload-module! module #!optional free?)
  (let ((free? (if (default-object? free?) #t free?)))
    (without-interrupts
     (lambda ()
       (if (and free? (number? (module/handle module)))
	   (free-library (module/handle module)))
       (set-module/handle! module 'unloaded)
       (set-cell-contents! (module/finalization-info module) #f)
       (protection-list/for-each-info
	(lambda (entry)
	  (set-module-entry/address! entry #f))
	(module/entries module))))))

(define (load-module! module)
  (case (module/handle module)
    ((unloaded bogus)
     (let ((handle (load-library (module/load-name module))))
       (if (= handle 0)
	   (set-module/handle! module 'bogus)
	   (without-interrupts
	    (lambda ()
	      (set-module/handle! module handle)
	      (set-cell-contents! (module/finalization-info module) handle))))))
    (else
     unspecific)))

(define (reload-module! module)
  (unload-module! module)
  (load-module! module))


(define (mark-modules-as-unloaded!)
  (protection-list/for-each
   (lambda (module) (unload-module! module #f))    
   *modules*))

;;
;;-------------------------------------
;;

(define (make-entries-list)
  (make-protection-list identity-procedure))
;;

(define-structure
  (module-entry
    (conc-name module-entry/)
    (constructor %make-module-entry))
  module	
  name		;; a string
  address      	;; the address from GetProcAddress, or #f
  proc   	;; a weak pair of the foreign function (a scheme procedure)
)


(define ((entry-without-procedure entry) . ?)
  ?
  ((access error ()) "Called dll entry without procedure:" entry))

(define-integrable (module-entry/machine-address entry)
  (or (module-entry/address entry)
      (module-entry/attempt-linkage entry)))

(define (module-entry/attempt-linkage entry)
  (let* ((module  (module-entry/module entry)))
    (if (eq? (module/handle module) 'unloaded)
	(load-module! module))
    (let ((address (module-entry/address entry)))
      (if address
	  address
	  (case (module/handle module)
	    ((unloaded bogus)
	     (module-entry/error/bad-module entry))
	    (else
	     (let ((address (get-proc-address (module/handle module)
					      (module-entry/name entry))))
	       (if address
		   (begin
		     (set-module-entry/address! entry address)
		     address)
		   (module-entry/error/bad-entry entry)))))))))


(define (module-entry/error/bad-entry entry)
  ((access error ())
   "Cant find"
   entry 'for (module-entry/name entry)
   'in (module-entry/module entry)))
    
(define (module-entry/error/bad-module entry)
  ((access error ())
   "Cant load"
   (module-entry/module entry)
   'for 'procedure (module-entry/name entry)))
    
(define (make-module-entry module name)
  (let* ((handle       (module/handle module))
	 (address      (if (int:integer? handle)
			   (get-proc-address handle name)
			   #f))
	 (entry        (%make-module-entry module name address #f))
	 (weak-pair    (protection-list/add! (module/entries module) (entry-without-procedure entry) entry)))
    (set-module-entry/proc! entry weak-pair)
    entry))

(define (parameterize-with-module-entry procedure module name)
  (let* ((entry  (make-module-entry module name))
	 (proc   (procedure entry)))
    (weak-set-car! (module-entry/proc entry) proc)
    proc))
		 
;;
;;----------------------------------------------------------------------
;;

(define (when-microcode-supports-win32 thunk)
  ;; This is for wrapping event:after-restore procedures so that a windows
  ;; band will restore into a DOS only microcode.
  (lambda ()
    (if (implemented-primitive-procedure? (ucode-primitive nt:load-library 1))
	(thunk))))

(define (initialize-module-package!)
  (set! *modules*
    (make-protection-list
      ;;(lambda (handle)
      ;;  (and handle
      ;;     (integer? (cell-contents handle))
      ;;     (free-library (cell-contents handle))))
      (lambda (handle-cell) handle-cell)
      ))
  (add-event-receiver! event:after-restore
		       (when-microcode-supports-win32 mark-modules-as-unloaded!))
)
