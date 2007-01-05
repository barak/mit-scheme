#| -*-Scheme-*-

$Id: win32-registry.scm,v 1.2 2007/01/05 15:33:10 cph Exp $

Copyright 1999,2004 Massachusetts Institute of Technology

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

;;;; Win32 Registry Operations
;;; package: (runtime win32-registry)

(declare (usual-integrations))

(define (win32-registry/open-key name mode)
  (let ((key
	 (let loop ((name name))
	   (receive (prefix name) (split-registry-key-name name)
	     (if prefix
		 (let ((parent (loop prefix)))
		   (and parent
			(get-subkey parent name
				    (eq? 'CREATE-IF-NEEDED mode))))
		 (get-root-key name 'WIN32-REGISTRY/OPEN-KEY))))))
    (if (and (not key) (eq? 'MUST-EXIST mode))
	(error "Unable to open registry key:" name))
    key))

(define (split-registry-key-name name)
  (let ((slash (string-find-previous-char name #\\)))
    (if slash
	(values (string-head name slash)
		(string-tail name (fix:+ slash 1)))
	(values #f name))))

(define (get-root-key name procedure-name)
  (let loop ((keys open-keys))
    (if (not (pair? keys))
	(error:bad-range-argument name procedure-name))
    (if (string-ci=? name (registry-key-name (car keys)))
	(car keys)
	(loop (cdr keys)))))

(define (get-subkey parent name create?)
  (or (find-subkey parent name)
      (and create?
	   (let ((key (%make-registry-key parent name #f)))
	     (open-registry-handle win32-create-registry-key key)
	     (add-subkey! parent name key)
	     key))))

(define (win32-registry/add-subkey parent name)
  (guarantee-registry-key parent 'WIN32-REGISTRY/ADD-SUBKEY)
  (get-subkey parent name #t))

(define (win32-registry/delete-subkey parent name)
  (guarantee-registry-key parent 'WIN32-REGISTRY/DELETE-SUBKEY)
  (win32-delete-registry-key (guarantee-handle parent) name)
  (delete-subkey! parent name))

(define (win32-registry/key-name key)
  (guarantee-registry-key key 'WIN32-REGISTRY/KEY-NAME)
  (registry-key-name key))

(define (win32-registry/key-full-name key)
  (guarantee-registry-key key 'WIN32-REGISTRY/KEY-FULL-NAME)
  (if (registry-key-parent key)
      (string-append (win32-registry/key-name (registry-key-parent key))
		     "\\"
		     (registry-key-name key))
      (registry-key-name key)))

(define (win32-registry/key-parent key)
  (guarantee-registry-key key 'WIN32-REGISTRY/KEY-PARENT)
  (registry-key-parent key))

(define (win32-registry/subkeys key)
  (guarantee-registry-key key 'WIN32-REGISTRY/SUBKEYS)
  (guarantee-subkeys key)
  (map (lambda (k.n) (guarantee-subkey key k.n))
       (registry-key-subkeys key)))

(define (win32-registry/subkey key name)
  (guarantee-registry-key key 'WIN32-REGISTRY/SUBKEY)
  (find-subkey key name))

(define (win32-registry/value-names key)
  (guarantee-registry-key key 'WIN32-REGISTRY/VALUE-NAMES)
  (guarantee-values key)
  (map registry-value-name (registry-key-values key)))

(define (win32-registry/get-value key name)
  (guarantee-registry-key key 'WIN32-REGISTRY/GET-VALUE)
  (let ((data (win32-query-registry-value (guarantee-handle key) name)))
    (if data
	(values (number->value-type (car data)) (cdr data))
	(values #f #f))))

(define (win32-registry/set-value key name type data)
  (guarantee-registry-key key 'WIN32-REGISTRY/SET-VALUE)
  (win32-set-registry-value (guarantee-handle key) name
			    (value-type->number type) data)
  (add-value! key name type))

(define (win32-registry/delete-value key name)
  (guarantee-registry-key key 'WIN32-REGISTRY/DELETE-VALUE)
  (win32-delete-registry-value (guarantee-handle key) name)
  (delete-value! key name))

(define (win32/expand-environment-strings string)
  (let ((result (make-string (win32-expand-environment-strings string ""))))
    (win32-expand-environment-strings string result)
    (let ((nul (string-find-next-char result #\nul)))
      (if nul
	  (string-head result nul)
	  result))))

;;;; Data Structures

(define-structure (registry-key
		   (constructor %make-registry-key (parent name handle))
		   (predicate win32-registry/key?)
		   (print-procedure
		    (standard-unparser-method 'REGISTRY-KEY
		      (lambda (key port)
			(write-char #\space port)
			(write (registry-key-name key) port)))))
  (name #f read-only #t)
  (parent #f read-only #t)
  (handle #f)
  (subkeys 'UNKNOWN)
  (values 'UNKNOWN))

(define (guarantee-registry-key object procedure)
  (if (not (win32-registry/key? object))
      (error:wrong-type-argument object "registry key" procedure)))

(define (guarantee-handle key)
  (if (eq? 'DELETED (registry-key-handle key))
      (error "Registry key has been deleted:" key))
  (or (registry-key-handle key)
      (begin
	(open-registry-handle win32-open-registry-key key)
	(or (registry-key-handle key)
	    (error "Registry handle no longer exists:" key)))))

(define-structure (registry-value
		   (print-procedure
		    (standard-unparser-method 'REGISTRY-VALUE
		      (lambda (key port)
			(write-char #\space port)
			(write (registry-value-name key) port)))))
  (name #f read-only #t)
  (type #f))

;;;; Subkey Manipulation

(define (find-subkey parent name)
  (guarantee-subkeys parent)
  (let loop ((subkeys (registry-key-subkeys parent)))
    (if (pair? subkeys)
	(if (string-ci=? name (%weak-cdr (car subkeys)))
	    (guarantee-subkey parent (car subkeys))
	    (loop (cdr subkeys)))
	#f)))

(define (guarantee-subkeys key)
  (if (eq? 'UNKNOWN (registry-key-subkeys key))
      (set-registry-key-subkeys! key
				 (map (lambda (key)
					(%weak-cons key
						    (registry-key-name key)))
				      (generate-subkeys key)))))

(define (generate-subkeys key)
  (enumerate key
	     win32-enumerate-registry-key
	     1
	     (lambda (v) v)
	     (lambda (name v) v (%make-registry-key key name #f))))

(define (guarantee-subkey parent k.n)
  (or (%weak-car k.n)
      (let ((key (%make-registry-key parent (%weak-cdr k.n) #f)))
	(%weak-set-car! k.n key)
	key)))

(define (add-subkey! parent name key)
  (if (not (eq? 'UNKNOWN (registry-key-subkeys parent)))
      (let loop ((subkeys (registry-key-subkeys parent)))
	(if (pair? subkeys)
	    (if (not (string-ci=? name (%weak-cdr (car subkeys))))
		(loop (cdr subkeys)))
	    (set-registry-key-subkeys!
	     parent
	     (cons (%weak-cons key name) (registry-key-subkeys parent)))))))

(define (delete-subkey! parent name)
  (if (not (eq? 'UNKNOWN (registry-key-subkeys parent)))
      (let loop ((subkeys (registry-key-subkeys parent)) (prev #f))
	(if (pair? subkeys)
	    (if (string-ci=? name (%weak-cdr (car subkeys)))
		(without-interrupts
		 (lambda ()
		   (let ((key (%weak-car (car subkeys))))
		     (if key
			 (begin
			   (close-registry-handle key)
			   (set-registry-key-handle! key 'DELETED))))
		   (if prev
		       (set-cdr! prev (cdr subkeys))
		       (set-registry-key-subkeys! parent (cdr subkeys)))))
		(loop (cdr subkeys) subkeys))))))

;;;; Value Manipulation

(define (guarantee-values key)
  (if (eq? 'UNKNOWN (registry-key-values key))
      (set-registry-key-values! key (generate-values key))))

(define (generate-values key)
  (enumerate key
	     (lambda (h i b) (win32-enumerate-registry-value h i b #f))
	     3
	     (lambda (v) (vector-ref v 0))
	     (lambda (name v)
	       (make-registry-value name
				    (number->value-type (vector-ref v 1))))))

(define (find-value key name)
  (guarantee-values key)
  (let loop ((vs (registry-key-values key)))
    (if (pair? vs)
	(if (string-ci=? name (registry-value-name (car vs)))
	    (car vs)
	    (loop (cdr vs)))
	#f)))

(define (add-value! key name type)
  (if (not (eq? 'UNKNOWN (registry-key-values key)))
      (let loop ((vs (registry-key-values key)))
	(if (pair? vs)
	    (if (string-ci=? name (registry-value-name (car vs)))
		(set-registry-value-type! (car vs) type)
		(loop (cdr vs)))
	    (set-registry-key-values!
	     key
	     (cons (make-registry-value name type)
		   (registry-key-values key)))))))

(define (delete-value! key name)
  (if (not (eq? 'UNKNOWN (registry-key-values key)))
      (let loop ((vs (registry-key-values key)) (prev #f))
	(if (pair? vs)
	    (if (string-ci=? name (registry-value-name (car vs)))
		(if prev
		    (set-cdr! prev (cdr vs))
		    (set-registry-key-values! key (cdr vs)))
		(loop (cdr vs) vs))))))

;;;; Low-level Handle Tracking

(define (open-registry-handle procedure key)
  (let ((p (system-pair-cons (ucode-type weak-cons) #f #f)))
    (dynamic-wind
     (lambda () unspecific)
     (lambda ()
       (let ((v
	      (procedure (guarantee-handle (registry-key-parent key))
			 (registry-key-name key)
			 p)))
	 (if (%weak-cdr p)
	     (without-interrupts
	      (lambda ()
		(set-registry-key-handle! key (%weak-cdr p))
		(set-cdr! open-handles-list
			  (cons p (cdr open-handles-list)))
		(%weak-set-car! p key))))
	 v))
     (lambda ()
       (if (and (%weak-cdr p) (not (%weak-car p)))
	   (close-registry-handle key))))))

(define (close-registry-handle key)
  (let loop ((l1 open-handles-list) (l2 (cdr open-handles-list)))
    (if (pair? l2)
	(if (eq? key (%weak-car (car l2)))
	    (without-interrupts
	     (lambda ()
	       (win32-close-registry-key (%weak-cdr (car l2)))
	       (set-registry-key-handle! key #f)
	       (set-cdr! l1 (cdr l2))))
	    (loop l2 (cdr l2))))))

(define open-keys)
(define open-handles-list)

(define (initialize-package!)
  (set! open-keys
	(map (lambda (n.h)
	       (%make-registry-key #f (car n.h) (cdr n.h)))
	     (win32-predefined-registry-keys)))
  (set! open-handles-list (list 'OPEN-HANDLES-LIST))
  (add-gc-daemon! close-lost-open-keys-daemon))

(define (close-lost-open-keys-daemon)
  (let loop ((l1 open-handles-list) (l2 (cdr open-handles-list)))
    (if (pair? l2)
	(if (%weak-car (car l2))
	    (loop l2 (cdr l2))
	    (begin
	      (win32-close-registry-key (%weak-cdr (car l2)))
	      (set-cdr! l1 (cdr l2))
	      (loop l1 (cdr l1)))))))

;;;; Microcode Interface

(define-primitives
  (win32-predefined-registry-keys 0)
  (win32-open-registry-key 3)
  (win32-create-registry-key 3)
  (win32-close-registry-key 1)
  (win32-set-registry-value 4)
  (win32-delete-registry-value 2)
  (win32-delete-registry-key 2)
  (win32-enumerate-registry-key 3)
  (win32-query-info-registry-key 1)
  (win32-enumerate-registry-value 4)
  (win32-query-info-registry-value 2)
  (win32-query-registry-value 2)
  (win32-expand-environment-strings 2))

(define-structure (registry-key-info (type vector)
				     (conc-name registry-key-info/))
  (n-subkeys #f read-only #t)
  (max-subkey-name-length #f read-only #t)
  (n-values #f read-only #t)
  (max-value-name-name #f read-only #t)
  (max-value-length #f read-only #t))

;;; Value types:
(define value-types
  '#((REG_NONE)				; No value type
     (REG_SZ)				; Unicode null-terminated string
     (REG_EXPAND_SZ)			; Unicode null-terminated
					; string (with environment
					; variable references)
     (REG_BINARY)			; Free form binary
     (REG_DWORD REG_DWORD_LITTLE_ENDIAN) ; 32-bit number
     (REG_DWORD_BIG_ENDIAN)		; 32-bit number
     (REG_LINK)				; Symbolic Link (unicode)
     (REG_MULTI_SZ)			; Multiple Unicode strings
     (REG_RESOURCE_LIST)		; Resource list in the resource map
     (REG_FULL_RESOURCE_DESCRIPTOR)	; Resource list in the
					; hardware description
     (REG_RESOURCE_REQUIREMENTS_LIST)
     ))

(define (number->value-type n)
  (if (and (exact-nonnegative-integer? n)
	   (< n (vector-length value-types)))
      (car (vector-ref value-types n))
      n))

(define (value-type->number type)
  (cond ((symbol? type)
	 (let ((n (vector-length value-types)))
	   (let loop ((i 0))
	     (if (fix:= i n)
		 (error:bad-range-argument type #f))
	     (if (memq type (vector-ref value-types i))
		 i
		 (loop (fix:+ i 1))))))
	((and (exact-nonnegative-integer? type)
	      (< type #x100000000))
	 type)
	(else
	 (error:wrong-type-argument type "registry value type" #f))))

;;;; Utilities

(define (burst-string string delimiter)
  (let ((end (string-length string)))
    (let loop ((start 0) (result '()))
      (let ((index (substring-find-next-char string start end delimiter)))
	(if index
	    (loop (fix:+ index 1)
		  (cons (substring string start index) result))
	    (list->vector
	     (reverse! (cons (substring string start end) result))))))))

(define-integrable (%weak-cons a d)
  (system-pair-cons (ucode-type weak-cons) a d))

(define-integrable (%weak-car p) (system-pair-car p))
(define-integrable (%weak-set-car! p a) (system-pair-set-car! p a))
(define-integrable (%weak-cdr p) (system-pair-cdr p))
(define-integrable (%weak-set-cdr! p d) (system-pair-set-cdr! p d))

(define (enumerate key enumerator length-index get-length make-result)
  (let* ((handle (guarantee-handle key))
	 (buffer-length
	  (vector-ref (win32-query-info-registry-key handle) length-index))
	 (buffer (make-string buffer-length)))
    (let loop ((index 0) (vs '()))
      (let ((v (enumerator handle index buffer)))
	(if v
	    (loop (fix:+ index 1)
		  (cons (make-result (string-head buffer (get-length v)) v)
			vs))
	    vs)))))