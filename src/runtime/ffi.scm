#| -*-Scheme-*-

Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011 Matthew Birkholz

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

;;;; Aliens and Alien Functions
;;; package: (runtime ffi)

(declare (usual-integrations))


;;; Aliens

(define-structure (alien (constructor %make-alien)
			 (conc-name %alien/)
			 (copier copy-alien)
			 (predicate alien?))
  ;; Two fixnums.
  (high-bits 0) (low-bits 0)
  ;; A symbol or list.
  ctype)

;; Breaking a word in two produces high and low fixnums.  If they are
;; two digits representing a larger number, then RADIX is their base.
;; For a 32 bit word, (radix) is #x10000.
;;
;; This substitutes a constant when there is a compiler, per its
;; target.  Else this is a reference to %radix.
(define-syntax radix
  (er-macro-transformer
   (lambda (form rename compare)
     (declare (ignore rename compare))
     (if (not (null? (cdr form)))
	 (syntax-error "No sub-forms allowed:" form))
     (cond ((get-subsystem-version "LIAR/i386") #x10000)
	   ((get-subsystem-version "LIAR/x86-64") #x100000000)
	   (else
	    '%RADIX)))))

;; This is only needed when the target machine's word size is unknown
;; (e.g. when compiling to C, or when there is no compiler).
(define %radix)

(set-record-type-unparser-method! rtd:alien
  (standard-unparser-method
   'alien
   (lambda (alien port)
     (write-char #\space port)
     (write (%alien/ctype alien) port)
     (write-string " 0x" port)
     (write-string (alien/address-string alien) port))))

(define-integrable alien/ctype %alien/ctype)

(define-integrable set-alien/ctype! set-%alien/ctype!)

(declare (integrate-operator c-cast))
(define (c-cast alien ctype)
  (set-%alien/ctype! alien ctype)
  alien)

(define (alien/address-string alien)
  ;; Returns a string, e.g. "081adc60".
  (let ((high (%alien/high-bits alien))
	(low (%alien/low-bits alien))
	(hex (lambda (n)
	       (string-pad-left (number->string n 16)
				(if (fix:= (radix) #x10000) 4 8)
				#\0))))
    (string-append (hex high) (hex low))))

(define (make-alien #!optional ctype)
  (let ((ctype (if (default-object? ctype) #f ctype)))
    (%make-alien 0 0 ctype)))

(declare (integrate-operator alien/address))
(define (alien/address alien)
  (+ (* (%alien/high-bits alien) (radix))
     (%alien/low-bits alien)))

(define (%set-alien/address! alien address)
  (let ((qr (integer-divide address (radix))))
    (set-%alien/high-bits! alien (integer-divide-quotient qr))
    (set-%alien/low-bits! alien (integer-divide-remainder qr))))

(declare (integrate-operator copy-alien-address!))
(define (copy-alien-address! alien source)
  (if (not (eq? alien source))
      (begin
	(set-%alien/high-bits! alien (%alien/high-bits source))
	(set-%alien/low-bits! alien (%alien/low-bits source)))))

(declare (integrate-operator alien-null?))
(define (alien-null? alien)
  (and (fix:zero? (%alien/high-bits alien))
       (fix:zero? (%alien/low-bits alien))))

(declare (integrate-operator alien-null!))
(define (alien-null! alien)
  (set-%alien/high-bits! alien 0)
  (set-%alien/low-bits! alien 0))

(define null-alien (make-alien '|void|))

(declare (integrate-operator alien=?))
(define (alien=? alien1 alien2)
  (and (fix:= (%alien/high-bits alien1) (%alien/high-bits alien2))
       (fix:= (%alien/low-bits alien1) (%alien/low-bits alien2))))

(define (alien-hash alien modulus)
  ;; Appropriate for hash table construction (as is alien=?).
  (remainder (fix:xor (%alien/high-bits alien)
		      (%alien/low-bits alien)) modulus))

(define (alien-byte-increment alien offset #!optional ctype)
  ;; Returns a new alien - a copy of ALIEN - whose address is OFFSET
  ;; bytes from ALIEN's.  If CTYPE is specified, the type slot of the
  ;; new alien is set.
  (let ((new (copy-alien alien)))
    (alien-byte-increment! new offset)
    (if (not (default-object? ctype))
	(set-%alien/ctype! new ctype))
    new))

(define (alien-byte-increment! alien increment #!optional ctype)
  ;; This procedure returns ALIEN after modifying it to have an
  ;; address INCREMENT bytes away from its previous address.  If CTYPE
  ;; is specified, the type slot of ALIEN is set.
  (let ((quotient.remainder (fix:divide increment (radix))))
    (let ((new-high (fix:+ (%alien/high-bits alien)
			   (integer-divide-quotient quotient.remainder)))
	  (new-low (fix:+ (%alien/low-bits alien)
			  (integer-divide-remainder quotient.remainder))))
      (cond ((fix:negative? new-high)
	     (error:bad-range-argument increment 'alien-byte-increment!))
	    ((fix:negative? new-low)
	     (if (fix:zero? new-high)
		 (error:bad-range-argument increment 'alien-byte-increment!)
		 (begin
		   (set-%alien/low-bits! alien (fix:+ new-low (radix)))
		   (set-%alien/high-bits! alien (fix:-1+ new-high)))))
	    ((fix:>= new-low (radix))
	     (set-%alien/low-bits! alien (fix:- new-low (radix)))
	     (set-%alien/high-bits! alien (fix:1+ new-high)))
	    (else
	     (set-%alien/low-bits! alien new-low)
	     (set-%alien/high-bits! alien new-high)))))
  (if (not (default-object? ctype))
      (set-%alien/ctype! alien ctype))
  alien)

(declare (integrate-operator guarantee-alien))
(define (guarantee-alien object operator)
  (if (not (alien? object))
      (error:not-alien object operator)))

(define (error:not-alien object operator)
  (call-with-current-continuation
   (lambda (continuation)
     (with-restart
      'USE-VALUE			;name
      "Continue with an alien."		;reporter
      continuation			;effector
      (lambda ()			;interactor
	(values
	 (prompt-for-evaluated-expression
	  "New alien (an expression to be evaluated)")))
      (lambda ()			;thunk
	(error:wrong-type-argument object "an alien" operator))))))


;;; Alien Functions

(define-structure (alien-function
		   (constructor %make-alien-function)
		   (conc-name %alien-function/)
		   (predicate alien-function?)
		   ;; To be fasdump/loadable.
		   (type vector) (named 'alien-function)
		   (print-procedure
		    (standard-unparser-method 'ALIEN-FUNCTION
		     (lambda (alienf port)
		       (write-char #\space port)
		       (write-string (%alien-function/name alienf)
				     port)))))

  ;; C function entry address as two fixnums.
  high-bits low-bits

  ;; String: name of trampoline.  (Starts with "Scm_".)
  name

  ;; String: name of shim.  (WithOUT "-shim.so" on the end.)
  library

  ;; Caseful symbol or list, e.g. (* |GtkWidget|).
  return-type

  ;; Alist of parameter names * types, e.g. ((widget (* |GtkWidget|))...)
  parameters

  ;; Filename from which the EXTERN declaration was read.
  filename

  ;; Band ID
  band-id)

(declare (integrate-operator guarantee-alien-function))
(define (guarantee-alien-function object operator)
  (if (not (alien-function? object))
      (error:not-alien-function object operator)))

(define (error:not-alien-function object operator)
  (error:wrong-type-argument object "an alien function" operator))

(define (make-alien-function name library return-type params filename)
  (%make-alien-function 0 0 (string-append "Scm_" name)
			library return-type params filename #f))

(define-integrable alien-function/return-type %alien-function/return-type)

(define-integrable alien-function/parameters %alien-function/parameters)

(define-integrable alien-function/filename %alien-function/filename)

(define-integrable (alien-function/name alienf)
  (string-tail (%alien-function/name alienf) 4)) 

(define (%set-alien-function/address! alienf address)
  (let ((qr (integer-divide address (radix))))
    (set-%alien-function/high-bits! alienf (integer-divide-quotient qr))
    (set-%alien-function/low-bits! alienf (integer-divide-remainder qr))))

(define band-id)

(define (reset-alien-functions!)
  (set! band-id (list (get-universal-time))))

(define (alien-function-cache! afunc)
  (if (eq? band-id (%alien-function/band-id afunc))
      unspecific
      (let* ((library (%alien-function/library afunc))
	     (name (%alien-function/name afunc))
	     (pathname (merge-pathnames
			(pathname-new-type (string-append library "-shim") "so")
			(system-library-directory-pathname)))
	     (handle (or (find-dld-handle
			  (lambda (h)
			    (pathname=? pathname (dld-handle-pathname h))))
			 (dld-load-file pathname)))
	     (address (dld-lookup-symbol handle name)))
	(if address
	    (%set-alien-function/address! afunc address)
	    (error:bad-range-argument afunc 'alien-function-cache!))
	(set-%alien-function/band-id! afunc band-id))))

(define-integrable (c-peek-cstring alien)
  ((ucode-primitive c-peek-cstring 2) alien 0))

(define-integrable (c-peek-cstring! alien)
  ((ucode-primitive c-peek-cstring! 2) alien 0))

(define-integrable (c-peek-cstringp alien)
  ((ucode-primitive c-peek-cstringp 2) alien 0))

(define-integrable (c-peek-cstringp! alien)
  ((ucode-primitive c-peek-cstringp! 2) alien 0))

(define-integrable (c-peek-bytes alien offset count buffer start)
  ((ucode-primitive c-peek-bytes 5) alien offset count buffer start))

(define-integrable (c-poke-pointer dest alien)
  ;; Sets the pointer at the alien DEST to point to the ALIEN.
  ((ucode-primitive c-poke-pointer 3) dest 0 alien))

(define (c-poke-pointer! dest alien)
  ;; Like c-poke-pointer, but increments DEST by a pointer width.
  ((ucode-primitive c-poke-pointer! 3) dest 0 alien))

(define (c-poke-string alien string)
  ;; Copy STRING to the bytes at the ALIEN address.
  (guarantee-string string 'C-POKE-STRING)
  ((ucode-primitive c-poke-string 3) alien 0 string))

(define (c-poke-string! alien string)
  ;; Like c-poke-string, but increments ALIEN by the null-terminated
  ;; STRING length.
  (guarantee-string string 'C-POKE-STRING)
  ((ucode-primitive c-poke-string! 3) alien 0 string))

(define-integrable (c-poke-bytes alien offset count buffer start)
  ((ucode-primitive c-poke-bytes 5) alien offset count buffer start))

(define (c-enum-name value enum-name constants)
  enum-name
  (let loop ((consts constants))
    (if (null? consts)
	(error:bad-range-argument value 'c-enum-name)
	(let ((name.value (car consts)))
	  (if (= value (cdr name.value))
	      (car name.value)
	      (loop (cdr consts)))))))

(define (call-alien alien-function . args)
  (guarantee-alien-function alien-function 'call-alien)
  (alien-function-cache! alien-function)
  (for-each
   (lambda (arg)
     (if (alien-function? arg)
	 (alien-function-cache! arg)))
   args)
  (without-interrupts
   (lambda ()
     (call-alien* alien-function args))))

(define (call-alien* alien-function args)
  (let ((old-top calloutback-stack))
    (%if-tracing
     (outf-error ";"(tindent)"=> "alien-function" "args"\n")
     (set! calloutback-stack (cons (cons* alien-function args) old-top)))
    (let ((value (apply (ucode-primitive c-call -1) alien-function args)))
      (%if-tracing
       (%assert (eq? old-top (cdr calloutback-stack))
		"call-alien: freak stack "calloutback-stack"\n")
       (set! calloutback-stack old-top)
       (outf-error ";"(tindent)"<= "value"\n"))
      value)))


;;; Malloc/Free

;; Weak alist of: ( malloc alien X copy for c-free )...
(define malloced-aliens '())

(define (free-malloced-aliens)
  (let loop ((aliens malloced-aliens)
	     (prev #f))
    (if (pair? aliens)
	(if (weak-pair/car? (car aliens))
	    (loop (cdr aliens) aliens)
	    (let ((copy (weak-cdr (car aliens)))
		  (next (cdr aliens)))
	      (if prev
		  (set-cdr! prev next)
		  (set! malloced-aliens next))
	      (if (not (alien-null? copy))
		  (begin
		    ((ucode-primitive c-free 1) copy)
		    (alien-null! copy)))
	      (loop next prev))))))

(define (reset-malloced-aliens!)
  (let loop ((aliens malloced-aliens))
    (if (pair? aliens)
	(let ((alien (weak-car (car aliens)))
	      (copy (weak-cdr (car aliens))))
	  (if alien (alien-null! alien))
	  (alien-null! copy)
	  (loop (cdr aliens)))))
  (set! malloced-aliens '()))

(define (make-alien-to-free ctype init)
  ;; Register BEFORE initializing (allocating).
  (let ((alien (make-alien ctype)))
    (let ((copy (make-alien ctype)))
      (let ((entry (weak-cons alien copy)))
	(without-interrupts
	 (lambda ()
	   (set! malloced-aliens (cons entry malloced-aliens)))))
      (init copy)
      ;; Even an abort here will not leak a byte.
      (copy-alien-address! alien copy))
    alien))

(define (malloc size ctype)
  (make-alien-to-free ctype
		      (lambda (alien)
			((ucode-primitive c-malloc 2) alien size))))

(define (free alien)
  (if (not (alien? alien))
      (warn "Cannot free a non-alien:" alien)
      (let ((weak (weak-assq alien malloced-aliens)))
	(if (not weak)
	    (warn "Cannot free an alien that was not malloced:" alien)
	    (let ((copy (weak-cdr weak)))
	      (without-interrupts
	       (lambda ()
		 (if (not (alien-null? alien))
		     (begin
		       (alien-null! alien)
		       ((ucode-primitive c-free 1) copy)
		       (alien-null! copy))))))))))

(define (weak-assq obj alist)
  (let loop ((alist alist))
    (if (null? alist) #f
	(let* ((entry (car alist))
	       (key (weak-car entry)))
	  (if (eq? obj key) entry
	      (loop (cdr alist)))))))


;;; Callback support

(define registered-callbacks)
(define first-free-id)

(define (reset-callbacks!)
  (set! registered-callbacks (make-vector 100 #f))
  (set! first-free-id 1))

(define (register-c-callback procedure)
  (if (not (procedure? procedure))
      (error:wrong-type-argument procedure "a procedure" 'register-c-callback))
  (without-interrupts
   (lambda ()
     (let ((id first-free-id))
       (set! first-free-id (next-free-id (1+ id)))
       (vector-set! registered-callbacks id procedure)
       id))))

(define (next-free-id id)
  (let ((len (vector-length registered-callbacks)))
    (let next-id ((id id))
      (cond ((= id len)
	     (set! registered-callbacks
		   (vector-grow registered-callbacks (* 2 len)))
	     (next-free-id id))
	    ((not (vector-ref registered-callbacks id)) id)
	    ;; When not recycling ids, the above is always true.
	    ;; There is no need for the next-id loop.
	    (else (next-id (1+ id)))))))

(define (de-register-c-callback id)
  (vector-set! registered-callbacks id #f)
  ;; Uncomment to recycle ids.
  ;;(if (< id first-free-id)
  ;;    (set! first-free-id id))
  )

(define (normalize-aliens! args)
  ;; Any vectors among ARGS are assumed to be freshly-consed aliens
  ;; without their record-type.  Fix them.
  (let ((tag (record-type-dispatch-tag rtd:alien)))
    (let loop ((args args))
      (if (null? args)
	  unspecific
	  (let ((arg (car args)))
	    (if (%record? arg) (%record-set! arg 0 tag))
	    (loop (cdr args)))))))

(define (callback-handler id args)
  ;; Installed in the fixed-objects-vector, this procedure is called
  ;; by a callback trampoline.  The callout should have already masked
  ;; all but the GC interrupts.

  (if (not (< id (vector-length registered-callbacks)))
      (error:bad-range-argument id 'apply-callback))
  (let ((procedure (vector-ref registered-callbacks id)))
    (if (not procedure)
	(error:bad-range-argument id 'apply-callback))
    (normalize-aliens! args)
    (let ((old-top calloutback-stack))
      (%if-tracing
       (outf-error ";"(tindent)"=>> "procedure" "args"\n")
       (set! calloutback-stack (cons (cons procedure args) old-top)))
      (let ((value (apply-callback-proc procedure args)))
	(%if-tracing
	 (%assert (and (pair? calloutback-stack)
		       (eq? old-top (cdr calloutback-stack)))
		  "callback-handler: freak stack "calloutback-stack"\n")
	 (set! calloutback-stack old-top)
	 (outf-error ";"(tindent)"<<= "value"\n"))
	value))))

(define (apply-callback-proc procedure args)
  (call-with-current-continuation
   (lambda (return)
     (with-restart
      'USE-VALUE			;name
      "Return a value from the callback." ;reporter
      return				;effector
      (lambda ()			;interactor
	(values (prompt-for-evaluated-expression
		 "Value to return from callback")))
      (lambda ()			;thunk
	(let ((done? #f))
	  (if (not done?)
	      (begin
		(set! done? #t)
		(apply procedure args))
	      (let loop ()
		(error "Cannot return from a callback more than once.")
		(loop)))))))))

;;; For callback debugging:

(define (outf-error . objects)
  ((ucode-primitive outf-error 1)
   (apply string-append
	  (map (lambda (o) (if (string? o) o (write-to-string o)))
	       objects))))

(define (registered-callback-count)
  (let* ((vector registered-callbacks)
	 (end (vector-length vector)))
    (let loop ((i 0)(count 0))
      (if (fix:< i end)
	  (loop (fix:1+ i)
		(if (vector-ref vector i)
		    (fix:1+ count)
		    count))
	  (cons count end)))))

(define (initialize-callbacks!)
  (vector-set! (get-fixed-objects-vector) #x41 callback-handler))


(define calloutback-stack '())

(define %trace? #f)

(define (reset-package!)
  (reset-alien-functions!)
  (reset-malloced-aliens!)
  (reset-callbacks!)
  (set! %radix (if (fix:fixnum? #x100000000) #x100000000 #x10000))
  (set! %trace? #f)
  (set! calloutback-stack '()))

(define (initialize-package!)
  (reset-package!)
  (initialize-callbacks!)
  (add-event-receiver! event:after-restore reset-package!)
  (add-gc-daemon! free-malloced-aliens)
  unspecific)

(define-syntax %if-tracing
  (syntax-rules ()
    ((_ . BODY)
     (if %trace? ((lambda () . BODY))))))

(define-syntax %assert
  (syntax-rules ()
    ((_ TEST . MSG)
     (if (not TEST) (error "Failed assert:" . MSG)))))

(define-syntax %trace
  (syntax-rules ()
    ((_ . MSG)
     (if %trace? ((lambda () (outf-error . MSG)))))))

(define (tindent)
  (make-string (* 2 (length calloutback-stack)) #\space))