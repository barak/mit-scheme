#| -*-Scheme-*-

$Id: win_ffi.scm,v 1.3 1993/12/01 03:08:03 adams Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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

;;;; Foreign function interface
;;; package: (win32)

(declare (usual-integrations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Windows foreign function interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define get-handle        (make-primitive-procedure 'get-handle))
(define send-message      (make-primitive-procedure 'nt:send-message))
(define get-module-handle (make-primitive-procedure 'nt:get-module-handle))
(define load-library      (make-primitive-procedure 'nt:load-library))
(define free-library      (make-primitive-procedure 'nt:free-library))
(define get-proc-address  (make-primitive-procedure 'nt:get-proc-address))
(define def-window-proc   (make-primitive-procedure 'win:def-window-proc))
(define register-class    (make-primitive-procedure 'register-class))

(define (win:register-class name style hIcon hCursor background menu-name)
  (register-class style (get-handle 2) 4 4 (get-handle 0)
     hIcon hCursor background menu-name name))


;(define %call-foreign-function (make-primitive-procedure 'call-ff))
(define-integrable %call-foreign-function (ucode-primitive call-ff))

;(define int-result (lambda (result) result))
;(define bool-result (lambda (result) (not (= result 0))))
;(define void-result (lambda (result) result unspecific))
;(define nullable-pointer-result (lambda (result) (if (= result 0) #f result)))
;(define handle-result int-result)
;(define hwnd-result handle-result)

;(define any-arg (lambda (arg) arg))
;(define int-arg (lambda (arg) arg))
;(define bool-arg (lambda (arg) (if arg 1 0)))
;(define nullable-pointer-arg (lambda (arg) (or arg 0)))
;(define string-arg
;  (lambda (arg)
;    (if (or (eq? arg #f) (string? arg))
;        arg
;	((access error ())
;          "Type error on foreign function argument: Not string" arg))))
;(define-integrable handle-arg int-arg)
;(define-integrable hwnd-arg handle-arg)

;(define-integrable hdc-result handle-result)
;(define-integrable hdc-arg handle-arg)


(define (windows-procedure-argument-type-check-error type arg)
  ((access error ()) "Bad argument type for foreign procedure: " type 'value: arg))


(define-macro (call-case n)
#|  Generate
;      (lambda (module-entry)
;	(let ((arg1-type (list-ref arg-types 0))
;	      (arg2-type (list-ref arg-types 1)))
;	  (lambda (arg1 arg2)
;	    (result-type (%call-foreign-function
;	                   (module-entry/machine-address module-entry)
;			   (arg1-type arg1)
;			   (arg2-type arg2)))))))
|#
  (define (map-index f i n)
    (if (<= i n)
	(cons (f i) (map-index f (1+ i) n))
	'()))
  (define (->string thing)
    (cond  ((string? thing)  thing)
	   ((symbol? thing)  (symbol-name thing))
	   ((number? thing)  (number->string thing))))
  (define (concat . things)
    (string->symbol (apply string-append (map ->string things))))

  (let* ((arg-names  (map-index (lambda (i) (concat "arg" i)) 1 n))
 	 (type-names (map-index (lambda (i) (concat "arg" i "-type")) 1 n))
	 (indexes    (map-index identity-procedure 1 n))
	 (type-binds (map (lambda (type-name index) 
			    `(,type-name (list-ref arg-types ,(- index 1))))
			  type-names indexes))
	 (conversions (map list type-names arg-names)))

    `(lambda (module-entry)
       (let ,type-binds
	   (lambda ,arg-names
	     (result-type (%call-foreign-function
			   (module-entry/machine-address module-entry)
			   . ,conversions)))))))


(define (make-windows-procedure lib name result-type . arg-types)
  (let* ((arg-count (length arg-types))
	 (procedure
	  (case arg-count
	    (0  (call-case 0))
	    (1  (call-case 1))
	    (2  (call-case 2))
	    (3  (call-case 3))
	    (4  (call-case 4))
	    (5  (call-case 5))
	    (6  (call-case 6))
	    (7  (call-case 7))
	    (8  (call-case 8))
	    (9  (call-case 9))
	    (10  (call-case 10))
	    (11  (call-case 11))
	    (12  (call-case 12))
	    (13  (call-case 13))
	    (14  (call-case 14))
	    (15  (call-case 15))
	    (else
	     (lambda args
	       (if (= (length args) arg-count)
		   (result-type
		    (apply %call-foreign-function
			   (module-entry/machine-address module-entry)
			   (map (lambda (f x) (f x)) arg-types args)))
		   ((access error ())
                    "Wrong arg count for foreign function" 
		    name
		    (length args)
		    (list 'requires arg-count))))))))
    (parameterize-with-module-entry procedure lib name)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Windows function registry
;;
;;  CREATE-SCHEME-WINDOW calls create-window-ex with all but the scheme
;;  procedure argument.  The window class has to be declared with the C
;;  wndproc that calls GENERAL-SCHEME-WNDPROC.  This C wndproc is available
;;  as (GET-HANDLE 3)
;;
;;  GENERAL-SCHEME-WNDPROC is invoked as the window procedure of every
;;  scheme window.  It used the hwnd parameter to find the window-specific
;;  version of the wndproc.  There is a minor complication: the first time
;;  that we know what the window handle is happens during the call to
;;  GENERAL-SCHEME-WNDPROC, so we can only associate the handle with 
;;  the window procedure at that time
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(load-option 'hash-table)

(define make-integer-hash-table
        (hash-table/constructor  modulo
				 int:=
				 cons
				 true
				 car
				 cdr
				 set-cdr!))

(define (initialize-wndproc-registry)
  (set! wndproc-registry (make-integer-hash-table)))


(define newproc #f)

(define (general-scheme-wndproc hwnd message wparam lparam)
  (let ((wndproc (hash-table/get wndproc-registry hwnd #f)))
    (if wndproc
        (wndproc hwnd message wparam lparam)
        (let ((theproc newproc))
	  (set! newproc #f)
	  (if (eq? theproc #f)
	      (begin
		(display "\nNo wndproc for ") (display hwnd) (display "!\n")
		(set! newproc default-scheme-wndproc)))
	  (hash-table/put! wndproc-registry hwnd theproc)
	  (theproc hwnd message wparam lparam)))))

(define (create-scheme-window ex-style class name style x y w h
                               parent menu inst param proc)
  (set! newproc proc)
  (create-window-ex ex-style class name style x y w h
	            parent menu inst param))


;;
;; How do we delete wndprocs from the table?  It is not clear what is the very
;; last windows message received by a window. 
;; As a temporary measure we check to see if the windows still exist every GC

(define (wndproc-registry-cleaner)
  (hash-table/for-each wndproc-registry
    (lambda (hwnd wndproc)
      wndproc
      (if (not (is-window? hwnd))
        (hash-table/remove! wndproc-registry hwnd)))))

;; Use DEFAULT-SCHEME-WNDPROC rather than DEF-WINDOW-PROC so that we can hook in
;; behaviour for scheme windows

(define default-scheme-wndproc def-window-proc)

;; SUBCLASS-WINDOW! hwnd
;;    (lambda (prev) (lambda (hwnd msg wp lp) ... (prev hwnd msg wp lp)))
(define (subclass-window! hwnd subclass-behaviour)
  (let* ((scheme-wndproc (get-handle 3))
         (C-proc    (get-window-long hwnd GWL_WNDPROC))
         (scheme?   (= C-proc scheme-wndproc))
	 (old-proc  (if scheme?
	                (or (hash-table/get wndproc-registry hwnd #f)
			    default-scheme-wndproc)
			(lambda (hw m w l)
			  (%call-foreign-function c-proc hw m w l)))))
    (set-window-long hwnd GWL_WNDPROC scheme-wndproc)
    (hash-table/put! wndproc-registry hwnd (subclass-behaviour old-proc))
    unspecific))
	                

(define wndproc-registry)
(define message-polling-thread)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Message polling thread.
;;  Using a thread gives the window procedures a well-defined dynamic context
;;  rather than using the interrupt handlers dynamic context
;;
;;  (START-MESSAGE-POLLING-THREAD) sets thing going.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-message-polling-loop)
  (define msg (make-string 40))
  (define (message-polling-loop)
    (if (peek-message msg 0 0 0 1 #|PM_REMOVE|#)
        (begin
	  (translate-message msg)
	  (without-interrupts (lambda()(dispatch-message msg)))
	  (message-polling-loop))))
  message-polling-loop)

(define (start-message-polling-thread)
  (define default-handler-interrupt-mask 8)
  (define clear-interrupts! (make-primitive-procedure 'clear-interrupts!))
  (define (ignoring-interrupt-handler interrupt-code interrupt-mask)
    interrupt-code interrupt-mask
    (clear-interrupts! default-handler-interrupt-mask))
  (define message-polling-loop (make-message-polling-loop))
  (define (thunk)
    (define (loop)
;;      (disallow-preempt-current-thread)
      (message-polling-loop)
      (yield-current-thread)
      (loop))
    (loop))

  ;; install dummy handler and 
  (without-interrupts
    (lambda ()
      (let  ((system-interrupt-vector
	       (vector-ref (get-fixed-objects-vector)
		           (fixed-objects-vector-slot 'SYSTEM-INTERRUPT-VECTOR))))
         (vector-set! system-interrupt-vector 3 ignoring-interrupt-handler))))

  (set! message-polling-thread (create-thread #f thunk)))
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Windows common types
;;

(define-windows-type unchecked
  #f #f #f #f)

(define-windows-type bool
  boolean?
  (lambda (it) (if it 1 0))
  (lambda (it) (if (eq? it 0) #f #t))
  #f)

(define-windows-type int
  int:integer?
  #f
  #f
  #f)

(define-similar-windows-type uint int)
(define-similar-windows-type long int)
(define-similar-windows-type ulong int)
(define-similar-windows-type short int)
(define-similar-windows-type ushort int)
(define-similar-windows-type dword int)
(define-similar-windows-type word int)
(define-similar-windows-type byte int)

(define-similar-windows-type colorref int)

(define-windows-type string
  string?)

(define-windows-type char*
  (lambda (thing) (or (eq? thing #f) (string? #f))))

(define-windows-type handle
  (lambda (x) (or (eq? x #f) (int:integer? x)))
  #f
  #f
  #f)

(define-similar-windows-type hbitmap handle)
(define-similar-windows-type hbrush handle)
(define-similar-windows-type hcursor handle)
(define-similar-windows-type hdc handle)
(define-similar-windows-type hgdiobj handle)
(define-similar-windows-type hicon handle)
(define-similar-windows-type hinstance handle)
(define-similar-windows-type hmenu handle)
(define-similar-windows-type hpalette handle)
(define-similar-windows-type hpen handle)
(define-similar-windows-type hrgn handle)
(define-similar-windows-type hwnd handle)

(define-windows-type resource-id  ;; string or int
  (lambda (x) (or (string? x) (int:integer? x)))
  #f
  #f
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kernel32.dll)
(define shell32.dll)
(define user32.dll)
(define gdi32.dll)

;;(define get-last-error
;;  (make-windows-procedure kernel32.dll "GetLastError" int-result))
;;    
;;(define set-last-error
;;  (make-windows-procedure kernel32.dll "SetLastError" void-result int-arg))
    
(define close-window)
(define create-round-rect-rgn)
(define create-window-ex)
(define dispatch-message)
(define get-window-long)       
(define get-window-text)
(define is-iconic?)
(define is-window?)
(define message-beep)
(define message-box)
(define message-box-ex)
(define peek-message)
(define pt-in-region)      
(define set-window-long)
(define set-window-text)
(define translate-message)
(define unregister-class)      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-windows-procedures!)
  (set! kernel32.dll (find-module "kernel32"))
  (set! shell32.dll  (find-module "shell32"))
  (set! user32.dll (find-module "user32.dll"))
  (set! gdi32.dll  (find-module "gdi32.dll"))

  (set! close-window
    (windows-procedure (close-window (hwnd hwnd))
      bool user32.dll "CloseWindow"))

  (set! set-window-text
    (windows-procedure (set-window-text (hwnd hwnd) (text string))
      bool user32.dll "SetWindowTextA"))

  (set! get-window-text
    (windows-procedure
	(get-window-text (hwnd hwnd) (text string) (maxlength int))
      int user32.dll "GetWindowTextA"))

  (set! message-beep
    (windows-procedure (message-beep (sound-type int))
      bool user32.dll "MessageBeep"))

  (set! is-iconic?
    (windows-procedure (is-iconic? (hwnd hwnd)) bool user32.dll "IsIconic"))

  (set! is-window?
    (windows-procedure (is-window? (hwnd hwnd)) bool user32.dll "IsWindow"))

  (set! create-round-rect-rgn
    (windows-procedure
	(create-round-rect-rgn (left int) (top int) (right int) (bottom int)
			       (widthellipse int)(heightellipse int))
      hrgn gdi32.dll "CreateRoundRectRgn"))

  (set! pt-in-region
    (windows-procedure (pt-in-region (hrgn hrgn) (x int) (y int))
      bool gdi32.dll "PtInRegion"))

  (set! create-window-ex
    (windows-procedure
	(create-window-ex (ex-style dword)
			  (class-name string)
			  (window-name string)
			  (style dword)
			  (x int)
			  (y int)
			  (width int)
			  (height int)
			  (parent hwnd)
			  (menu hmenu)
			  (instance hinstance)
			  (param unchecked))
      hwnd user32.dll "CreateWindowExA"))

  (set! unregister-class
    (windows-procedure (unregister-class (name string) (instance hinstance))
      bool user32.dll "UnregisterClassA"))
      
  (set! get-window-long
    (windows-procedure (get-window-long (hwnd hwnd) (index int))
      long user32.dll "GetWindowLongA"))
       
  (set! set-window-long
    (windows-procedure (set-window-long (hwnd hwnd) (index int) (value long))
      long user32.dll "SetWindowLongA"))
       
  (set! message-box
    (windows-procedure
	(message-box (owner hwnd) (text string ) (title string) (style int))
      int user32.dll "MessageBoxA"))

  (set! message-box-ex
    (windows-procedure
	(message-box (owner hwnd) (text string ) (title string)
		     (style int) (language word))
      int user32.dll "MessageBoxExA"))

  (set! peek-message
    (windows-procedure
	(peek-message (msg unchecked) (hwnd hwnd)
		      (filter-min int) (filter-max int) (remove-flag int))
      bool user32.dll "PeekMessageA"))

  (set! translate-message
    (windows-procedure (translate-message (msg unchecked))
      bool user32.dll "TranslateMessage"))

  (set! dispatch-message
    (windows-procedure (dispatch-message (msg unchecked))
      long user32.dll "DispatchMessageA"))
  )


(define (install-general-scheme-wndproc!)
  ((make-primitive-procedure 'set-general-scheme-wndproc)
    general-scheme-wndproc))

    
(define (initialize-package!)
  ;; Install GENERAL-SCHEME-WNDPROC
  ;; (initialize-general-scheme-wndproc!)
  (purify general-scheme-wndproc)
  (flush-purification-queue!)
  (install-general-scheme-wndproc!)
  (add-event-receiver!
    event:after-restore
    (when-microcode-supports-win32 install-general-scheme-wndproc!))
  (create-windows-procedures!)

  (initialize-wndproc-registry)
  (add-gc-daemon! wndproc-registry-cleaner)
  ;(start-message-polling-thread)
)
