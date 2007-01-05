#| -*-Scheme-*-

$Id: win_ffi.scm,v 1.15 2007/01/05 15:33:10 cph Exp $

Copyright 1993,1994,1998,2001,2002,2003 Massachusetts Institute of Technology
Copyright 2004 Massachusetts Institute of Technology

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


(define-integrable %call-foreign-function
  (ucode-primitive call-ff -1))

(define (windows-procedure-argument-type-check-error type arg)
  ((access error system-global-environment)
   "Bad argument type for foreign procedure: "  type  'value:  arg))


(define-syntax call-case
  (sc-macro-transformer
   (lambda (form environment)
     (let ((n (cadr form)))
       (let* ((indexes
	       (let loop ((i 1))
		 (if (<= i n)
		     (cons i (loop (+ i 1)))
		     '())))
	      (arg-names
	       (map (lambda (i)
		      (intern (string-append "arg" (number->string i))))
		    indexes))
	      (type-names
	       (map (lambda (n) (symbol-append n '-TYPE))
		    arg-names)))
	 `(LAMBDA (MODULE-ENTRY)
	    (LET ,(map (lambda (type-name index) 
			 `(,type-name
			   (LIST-REF ,(close-syntax 'ARG-TYPES environment)
				     ,(- index 1))))
		       type-names
		       indexes)
		(LAMBDA ,arg-names
		  (,(close-syntax 'RESULT-TYPE environment)
		   (%CALL-FOREIGN-FUNCTION
		    (MODULE-ENTRY/MACHINE-ADDRESS MODULE-ENTRY)
		    ,@(map list type-names arg-names)))))))))))

(define (make-windows-procedure lib name result-type . arg-types)
  (let* ((arg-count (length arg-types))
	 (procedure
	  (case arg-count
	    ((0) (call-case 0))
	    ((1) (call-case 1))
	    ((2) (call-case 2))
	    ((3) (call-case 3))
	    ((4) (call-case 4))
	    ((5) (call-case 5))
	    ((6) (call-case 6))
	    ((7) (call-case 7))
	    ((8) (call-case 8))
	    ((9) (call-case 9))
	    ((10) (call-case 10))
	    ((11) (call-case 11))
	    ((12) (call-case 12))
	    ((13) (call-case 13))
	    ((14) (call-case 14))
	    ((15) (call-case 15))
	    (else
	     (lambda (module-entry)
	       (lambda args
		 (if (= (length args) arg-count)
		     (result-type
		      (apply %call-foreign-function
			     (module-entry/machine-address module-entry)
			     (map (lambda (f x) (f x)) arg-types args)))
		     ((access error system-global-environment)
		      "Wrong arg count for foreign function" 
		      name
		      (length args)
		      (list 'requires arg-count)))))))))
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
;;  the window procedure at that time.  Further, we do not know what first
;;  or last message is -- Various places in the Win32 API Reference imply
;;  the first is WM_CREATE or WM_NCCREATE but I have seen the sequence
;;  [WM_GETMINMAXINFO, WM_NCCALCSIZE, WM_NCCREATE, WM_CREATE].  Similarly,
;;  WM_NCDESTROY seems to be sent after WM_DESTROY, but who knows what happens
;;  in other cases?  Ugh.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-integer-hash-table
  (strong-hash-table/constructor modulo int:= #f))

(define (initialize-wndproc-registry)
  (set! wndproc-registry (make-integer-hash-table)))


(define general-scheme-wndproc)
(define create-scheme-window)

(let ((newproc #f)
      (mask    0))

  (define (the-general-scheme-wndproc hwnd message wparam lparam)
    (cond (newproc
	   => (lambda (theproc)
		(set! newproc #F)
		(hash-table/put! wndproc-registry hwnd theproc)
		(set-interrupt-enables! mask)
		(theproc hwnd message wparam lparam)))
	  ((hash-table/get wndproc-registry hwnd #f)
	   => (lambda (wndproc)
		(wndproc hwnd message wparam lparam)))
	  (else
	   ((access warn system-global-environment)
	    "No wndproc for window "  hwnd  'general-scheme-wndproc)
	   (default-scheme-wndproc hwnd message wparam lparam))))

  (define (the-create-scheme-window ex-style class name style x y w h
				    parent menu inst param proc)
    (set! mask (set-interrupt-enables! interrupt-mask/gc-ok))
    (set! newproc proc)
    (create-window-ex ex-style class name style x y w h
		      parent menu inst param))

  (set! general-scheme-wndproc the-general-scheme-wndproc)
  (set! create-scheme-window   the-create-scheme-window)
  unspecific)


;; How do we delete wndprocs from the table?  It is not clear what is the very
;; last windows message received by a window. 
;;
;; As a temporary measure we check to see if the windows still exist every GC

(define (wndproc-registry-cleaner)
  (hash-table/for-each wndproc-registry
    (lambda (hwnd wndproc)
      wndproc                         ; ignored
      (if (not (is-window? hwnd))
	  (hash-table/remove! wndproc-registry hwnd)))))

;; Applications should use DEFAULT-SCHEME-WNDPROC rather than DEF-WINDOW-PROC
;; so that we can hook in behaviour for all scheme windows.

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
  (lambda (thing) (or (eq? thing #f) (string? thing))))

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
(define sleep)
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

  (set! sleep
    (windows-procedure (sleep (msec int))
      bool kernel32.dll "Sleep"))

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