; -*- Scheme -*-
;;;;; C external interfaces to Tk procedures not associated with
;;;;; a particular widget.
;;;; $Id: tk-mit.scm,v 1.4 1997/10/02 19:19:44 adams Exp $

;;;; This is the lowest level Scheme interface to general TK/TCL data
;;;; structures.  Primitives are defined in tk-mit.c and tk.c

(define-primitives
  (%tclGlobalEval 2)
  (%tkCompletelyHandlesEvent? 1)
  (%tkCreateTopLevelWindow 3)
  (%tkDoEvents 0)
  (%tkDrainCallBacks 2)
  (%tkGenerateSchemeEvent 2)
  (%tkInit 2)
  (%tkInvokeCommand -1)
  (%tkKillApplication 1)
  (%tkManageGeometry 2)
  (%tkMapWindow 1)
  (%tkMoveResizeWindow 5)
  (%tkMoveWindow 3)
  (%tkNextWakeup 0)
  (%tkResizeWindow 3)
  (%tkUnmapWindow 1)
  (%tkWidget.tkwin 1)
  (%tkWinDisplay 1)
  (%tkWinReqHeight 1)
  (%tkWinReqWidth 1)
  (%tkWinHeight 1)
  (%tkWinIsMapped? 1)
  (%tkWinName 1)
  (%tkWinPathName 1)
  (%tkWinWidth 1)
  (%tkWinWindow 1)
  (%tkWinX 1)
  (%tkWinY 1)
)

;;;; Support code

(define tk-gen-name
  (let ((count 0))
    (lambda (name)
      (set! count (+ 1 count))
      (string-append name (number->string count)))))

;;;; Entry points in alphabetical order

(define (get-interval-to-tk-wakeup)
  (%tkNextWakeup))


;; A not-so-precise number->string that is faster and more than
;; sufficient for our purposes.  Note that the output always has a
;; leading digit to prevent tk from thinking that .7 is a name and
;; not a number.

(define (swat:number->string x)

  (define (digits x n tail)
    (define (next* ch x*)
      (cons ch (digits x* (fix:- n 1) tail)))
    (define-integrable (next ch delta)
      (next* ch (flo:* (flo:- x delta) 10.0)))
    (cond ((< n 0)  tail)
	  ((flo:< x 1.e-10)  tail)
	  ((flo:< x 1.0) (next #\0 0.0))
	  ((flo:< x 2.0) (next #\1 1.0))
	  ((flo:< x 3.0) (next #\2 2.0))
	  ((flo:< x 4.0) (next #\3 3.0))
	  ((flo:< x 5.0) (next #\4 4.0))
	  ((flo:< x 6.0) (next #\5 5.0))
	  ((flo:< x 7.0) (next #\6 6.0))
	  ((flo:< x 8.0) (next #\7 7.0))
	  ((flo:< x 9.0) (next #\8 8.0))
	  (else          (next #\9 9.0))))
	
  (define (format-exponent e)
    (define (format-integer n tail)
      (define (+digit k) (cons (ascii->char (fix:+ k 48)) tail))
      (if (fix:< n 10)
	  (+digit n)
	  (let ((front (fix:quotient n 10))
		(back  (fix:remainder n 10)))
	    (format-integer front (+digit back)))))
    (cond ((fix:= e 0) '())
	  ((fix:< e 0)
	   (cons* #\e #\- (format-integer (fix:- 0 e) '())))
	  (else
	   (cons* #\e (format-integer e '())))))

  (define (scale x e)
    (cond ((flo:< x 1.0e-30) '(#\0 #\. #\0))
	  ((flo:< x 1.0)     (scale (flo:* x 1000.0) (- e 3)))
	  ((flo:< x 10.0)
	   (let* ((tail  (format-exponent e))
		  (ds (digits x 8 tail)))
	     (if (eq? (cdr ds) tail)
		 (cons* (car ds) #\. #\0 (cdr ds))
		 (cons* (car ds) #\. (cdr ds)))))
	  (else          (scale (flo:* x 0.1) (+ e 1)))))

  (if (flo:flonum? x)
      (list->string
       (if (flo:< x 0.0)
	   (cons #\- (scale (flo:- 0.0 x) 0))
	   (scale x 0)))
      (number->string x 10)))

(define (stringify-for-tk arg)
  (define (->string arg)
    (cond ((string? arg)      arg)
	  ((number? arg)      (swat:number->string arg))
	  ((symbol? arg)      (symbol-name arg))
	  ((TK-variable? arg) (TK-variable.tk-name arg))
	  ((pair? arg)        (apply string-append (map stringify-for-tk arg)))
	  ((procedure? arg)   (->string (arg)))
	  (else (error "tcl-global-eval: Unknown argument type" arg))))

  (string-append "{" (->string arg) "} "))

(define (tk-op thunk)
  (let ((result (thunk)))
    (kick-uitk-thread)
    result))
   
(define (tcl-global-eval application command-name args)
  (tk-op
   (lambda ()
     (%tclGlobalEval
      (application->TKMainWindow application)
      (apply string-append (map stringify-for-tk (cons command-name args)))))))


;;;turn off all floating errors around TK processing
;;;Note that we don't need a dynamic wind because
;;;%tkCompletelyHandlesEvent? always completes.  If the argument is
;;;bad it returns a 0.

(define (tk-completely-handles-event? os-event)
  (let ((old-mask (set-floating-error-mask! 0)))
    (let ((result (%tkCompletelyHandlesEvent? os-event)))
      (set-floating-error-mask! old-mask)
      (if (eqv? result 0)
	  (error "bad argument to tk-completely-handles-event?" os-event)
	  result))))

(define (tk-create-top-level-window main-window callbackhash)
  (tk-op
   (lambda ()
     (%tkCreateTopLevelWindow main-window
			      (tk-gen-name "top-level-window")
			      callbackhash))))

(define (tk-doevents)
  ;; Turn off floating errors
  (let ((old-mask (set-floating-error-mask! 0)))
    ;; Do all pending Tk events, which should only be do-when-idles
    (%tkDoEvents)
    (set-floating-error-mask! old-mask))
  (do-tk-callbacks))

(define (tk-generate-Scheme-event event-mask unwrapped-tk-window)
  ;; Cause TK to signal us that Scheme wants to know about these kinds
  ;; of events on this window.
  (%tkGenerateSchemeEvent event-mask unwrapped-tk-window))

(define (tk-init xdisplay)
  ;; Set up an initial environment with a Tcl interpreter
  (tk-op
   (lambda ()
     (%tkInit (->xdisplay xdisplay)
	      (tk-gen-name
	       (string-append "main-window-for-display-"
			      (number->string (->xdisplay xdisplay))))))))

(define (tk-invoke-command command-name main-window arg-strings)
  (define commands
    `((After . 0)
      (Bind . 1)
      (Destroy . 2)
      (Focus . 3)
      (Grab . 4)
      (Option . 5)
      (Pack . 6)
      (Place . 7)
      (Selection . 8)
      (Tk . 9)
      (Tkwait . 10)
      (Update . 11)
      (Winfo . 12)
      (Wm . 13)))
  (tk-op 
   (lambda ()
     (apply %tkInvokeCommand (cdr (assq command-name commands))
	    main-window
	    arg-strings))))

(define (tk-kill-application main-window)
  ;; main-window is an integer, not wrapped
  (%tkKillApplication main-window))

(define (tk-manage-geometry widget manager-procedure)
  ;; Arrange for manager-procedure to be called with no arguments
  ;; whenever TK requests geometry operations on widget.
  (tk-op
   (lambda ()
     (%tkManageGeometry (tk-widget.tkwin widget)
			(and manager-procedure
			     (hash manager-procedure
				   *our-hash-table*))))))

(define (tk-map-window tkwin)
  (tk-op (lambda () (%tkmapwindow tkwin))))

(define (tk-move-resize-widget widget screen-area)
  (tk-op
   (lambda ()
     (%tkMoveResizeWindow (tk-widget.tkwin widget)
			  (Point.X (UITKRectangle.Offset screen-area))
			  (Point.Y (UITKRectangle.Offset screen-area))
			  (UITKRectangle.Width screen-area)
			  (UITKRectangle.Height screen-area)))))

(define (TK-Unmap-Window tkwin)
  (tk-op (lambda () (%tkUnmapWindow tkwin))))

(define (tk-widget.tkwin widget)
  (%tkWidget.tkwin (->widget widget)))

(define (tkwin.display tkwin)
  (%tkWinDisplay tkwin))

(define (tkwin.req-height tkwin)
  (%tkWinReqHeight tkwin))

(define (tkwin.req-width tkwin)
  (%tkWinReqWidth tkwin))

(define (tkwin.height tkwin)
  (%tkWinHeight tkwin))

(define (tkwin.IsMapped? tkwin)
  (%tkWinIsMapped? tkwin))

(define (tkwin.width tkwin)
  (%tkWinWidth tkwin))

(define (tkwin.window tkwin)
  ;; Deliberately don't do a wrap-window. Instead, allow a higher
  ;; level to do it, since the server maintains the window hierarchy
  ;; and effectively keeps pointers for us.
  (%tkWinWindow tkwin))

(define (tkwin.name tkwin)
  (%tkWinName tkwin))

(define (tkwin.pathname tkwin)
  (%tkWinPathName tkwin))

(define (tkwin.x tkwin)
  (%tkWinX tkwin))

(define (tkwin.y tkwin)
  (%tkWinY tkwin))

;;;; TK Callback handling

(define (do-tk-callbacks-from-string string)
  ;; The string has the following format:
  ;;  <char. count>
  ;;    <nchars>chars
  ;;    <nchars>chars
  ;;  ...
  ;;  where <char. count> is the number of characters in the object ID
  ;;  and its associated string arguments.  The "<" and ">" are NOT
  ;;  meta-characters; they are used for separating the entries and
  ;;  error detection.
  (define (split-string-by-number string receiver)
    ;; Expects a character count in angle brackets.  Calls receiver
    ;; with the counted string and the rest, or #F/#F if the string is
    ;; empty.
    (cond
     ((string-null? string) (receiver #F #F))
     ((not (char=? (string-ref string 0) #\<))
      (error "Split-String-By-Number: Badly formed entry"
	     string))
     (else
      (let ((break-at (string-find-next-char string #\>)))
	(if (not break-at)
	    (error "Split-String-By-Number: entry not terminated"
		   string)
	    (let ((count (string->number (substring string 1 break-at)))
		  (after-count (+ break-at 1))
		  (slength (string-length string)))
	      (cond
	       ((not count)
		(error "Split-String-By-Number: non-numeric count" string))
	       ((> (+ after-count count) slength)
		(error "Split-String-By-Number: count too big" string))
	       (else
		(let ((end (+ after-count count)))
		  (receiver (substring string after-count end)
			    (substring string end slength)))))))))))
  (define (parse-entry string receiver)
    ;; Entry starts with a character count in angle brackets
    ;; Receiver is called with an object, a vector of strings, and the
    ;; remaining string.
    (split-string-by-number string
     (lambda (entry after-entry)
       (let loop ((rest entry)
		  (strings '()))
	 (split-string-by-number rest
	  (lambda (this-string rest-of-strings)
	    (if this-string
		(loop rest-of-strings
		      (cons this-string strings))
		(let ((all-strings (reverse strings)))
		  (if (null? all-strings)
		      (error "Parse-Entry: no entries" string))
		  (let* ((Object-Name (car all-strings))
			 (Object-ID (string->number object-name)))
		    (if (not object-id)
			(error "Parse-Entry: non-number object ID"
			       string object-name))
		    ;; Note that the object associated with object-id
		    ;; may have been GCed away!
		    (receiver (object-unhash object-id *our-hash-table*)
			      (cdr all-strings)
			      after-entry))))))))))
  (if string
      (let callback-loop ((string string))
	(if (string-null? string)
	    'done
	    (parse-entry string
			 (lambda (callback list-of-string-args rest-of-string)
			   ;; "callback" will be #F if it GC'ed away
			   (if callback
			       (our-with-thread-mutex-locked
				'do-tk-callback
				*event-processing-mutex*
				(lambda ()
				  (apply-callback callback list-of-string-args))))
			   (callback-loop rest-of-string))))))
  'OK)

(define (apply-callback callback arglist)
  (cond ((ignore-errors
	  (lambda () (apply callback arglist)))
	 => (lambda (result)
	      (if (condition? result)
		  (let ((port  (notification-output-port)))
		    (newline port)
		    (write-string ";Error in callback " port)
		    (display callback port)
		    (newline port)
		    (write-string ";" port)
		    (write-condition-report result port)
		    (newline port)
		    (write-string ";To debug, type (debug #@" port)
		    (write (hash result) port)
		    (write-string ")" port)
		    (newline port)))))))


(define *event-processing-mutex* (make-thread-mutex))

(define do-tk-callbacks
  (let ((nchars 0)
	(string (make-string 0)))
    (lambda ()
      (let ((nchars-ready (%tkDrainCallBacks nchars string)))
	(if nchars-ready
	    (if
	     (positive? nchars-ready)
	     (begin
	       (set! nchars nchars-ready)
	       (set! string (make-string nchars-ready))
	       (do-tk-callbacks))
	     'OK)
	    (do-tk-callbacks-from-string string))))))

