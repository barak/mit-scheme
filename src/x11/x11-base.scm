#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of an x11 plugin for MIT/GNU Scheme.

This plugin is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

This plugin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this plugin; if not, write to the Free Software Foundation,
Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

|#

;;;; X11 interface
;;; package: (x11 base)
;;;
;;; These were once primitives created by x11base.c in umodule prx11.

(C-include "x11")

(define (x-visual-deallocate xvisual)
  (guarantee-xvisual xvisual 'x-visual-deallocate)
  (C-call "x_visual_deallocate" xvisual)
  (alien-null! xvisual))

;;; Initialize/Close Displays

(define (x-close-display xd)
  (guarantee-xdisplay xd 'x-close-display)
  (C-call "x_close_display" xd))

(define (x-close-all-displays)
  (C-call "x_close_all_displays"))

;;; Window Manager Properties

(define (x-window-set-input-hint window hint?)
  (guarantee-xwindow window 'x-window-set-input-hint)
  (if (not (zero? (C-call "x_window_set_input_hint" window (if hint? 1 0))))
      (error "XAllocWMHints failed.")))

(define (->bytes string)
  ;; NOT null terminated
  (if (and (or (bytevector? string)
	       (and (ustring? string)
		    (fix:= 1 (ustring-cp-size string))))
	   (let ((end (string-length string)))
	     (every-loop (lambda (cp) (fix:< cp #x80))
			 cp1-ref string 0 end)))
      string
      (string->iso8859-1 string)))

(define-integrable (every-loop proc ref string start end)
  (let loop ((i start))
    (if (fix:< i end)
	(and (proc (ref string i))
	     (loop (fix:+ i 1)))
	#t)))

(define (->cstring string)
  (cond ((and (integer? string) (zero? string))
	 0)
	((bytevector? string)
	 (if (let ((end (bytevector-length string)))
	       (let loop ((i 0))
		 (if (fix:< i end)
		     (or (fix:zero? (bytevector-u8-ref string i))
			 (loop (fix:1+ i)))
		     #f)))
	     string
	     (error "C string not null terminated:" string)))
	((string? string)
	 ;; String->iso8859-1 would be incorrect; it does not null terminate.
	 (let* ((end (string-length string))
		(result (make-bytevector (fix:1+ end))))
	   (do ((i 0 (fix:1+ i)))
	       ((not (fix:< i end))
		(bytevector-u8-set! result i #x00))
	     (bytevector-u8-set! result i (char->integer
					   (string-ref string i))))
	   result))
	(else
	 (error:wrong-type-argument string "a string or 0" '->cstring))))

(define (x-window-set-name window name)
  (guarantee-xwindow window 'x-window-set-name)
  (if (not (zero? (C-call "x_window_set_name" window (->cstring name))))
      (error "XStringListToTextProperty failed.")))

(define (x-window-set-icon-name window name)
  (guarantee-xwindow window 'x-window-set-icon-name)
  (if (not (zero? (C-call "x_window_set_icon_name" window (->cstring name))))
      (error "XStringListToTextProperty failed.")))

;;; Open/Close

(define (x-open-display display-name)
  (let ((alien (make-alien '(struct |xdisplay|))))
    (C-call "x_open_display" alien (if (eq? #f display-name)
				       0
				       (->cstring display-name)))
    (if (alien-null? alien)
	(error "Could not open display:" display-name)
	alien)))

(define (x-display-get-size display screen)
  (guarantee-xdisplay display 'x-display-get-size)
  (let ((results (malloc (* 2 (c-sizeof "int")) 'int)))
    (c-call "x_display_get_size" display screen results)
    (let ((width (c-> results "int"))
	  (height (c-> (c-array-loc results "int" 1) "int")))
      (free results)
      (cons width height))))

(define (x-close-window xw)
  (guarantee-xwindow xw 'x-close-window)
  (C-call "x_close_window" xw))

(define (x-set-default-font display font-name)
  (guarantee-xdisplay display 'x-set-default-font)
  (if (not (zero? (c-call "x_set_default_font" display (->cstring font-name))))
      (error "Could not load font:" font-name)))

;;; Event Processing

(define (x-display-descriptor display)
  (guarantee-xdisplay display 'x-display-descriptor)
  (C-call "x_display_descriptor" display))

(define (x-max-request-size display)
  (guarantee-xdisplay display 'x-max-request-size)
  (c-call "x_max_request_size" display))

(define (x-display-process-events display how)
  (declare (ignore how))
  (guarantee-xdisplay display 'x-display-process-events)
  (let ((event (malloc (C-sizeof "XEvent") '|XEvent|))
	(xw (malloc (C-sizeof "* struct xwindow") '(* (struct |xwindow|)))))
    (let* ((done-p (C-call "x_display_process_events"
			   display event xw))
	   (window (C-> xw "* struct xwindow" (make-alien '(struct |xwindow|))))
	   (obj (if (= done-p 1)
		    #f
		    (or (make-event-object window event)
			#t))))
      (free xw)
      (free event)
      obj)))

(define (event-type xtype window xevent)
  (cond
   ((eq? xtype (C-enum "KeyPress")) event-type:key-press)
   ((eq? xtype (C-enum "ButtonPress")) event-type:button-down)
   ((eq? xtype (C-enum "ButtonRelease")) event-type:button-up)
   ((eq? xtype (C-enum "MotionNotify")) event-type:motion)
   ((eq? xtype (C-enum "ConfigureNotify")) event-type:configure)
   ((eq? xtype (C-enum "Expose")) event-type:expose)
   ((eq? xtype (C-enum "GraphicsExpose")) event-type:expose)
   ((eq? xtype (C-enum "ClientMessage"))
    (cond ((not (zero? (C-call "x_event_delete_window_p"
			       window xevent)))
	   event-type:delete-window)
	  ((not (zero? (C-call "x_event_take_focus_p"
			       window xevent)))
	   event-type:take-focus)
	  (else
	   (warn "Unexpected ClientMessage.")
	   #f)))
   ((eq? xtype (C-enum "VisibilityNotify")) event-type:visibility)
   ((eq? xtype (C-enum "SelectionClear")) event-type:selection-clear)
   ((eq? xtype (C-enum "SelectionNotify")) event-type:selection-notify)
   ((eq? xtype (C-enum "SelectionRequest")) event-type:selection-request)
   ((eq? xtype (C-enum "PropertyNotify")) event-type:property-notify)
   ((eq? xtype (C-enum "EnterNotify")) event-type:enter)
   ((eq? xtype (C-enum "LeaveNotify")) event-type:leave)
   ((eq? xtype (C-enum "FocusIn")) event-type:focus-in)
   ((eq? xtype (C-enum "FocusOut")) event-type:focus-out)
   ((eq? xtype (C-enum "MapNotify")) event-type:map)
   ((eq? xtype (C-enum "UnmapNotify")) event-type:unmap)
   (else (warn "Unexpected XEvent.") #f)))

(define (event-type-name scmtype)
  (let ((sym (C-enum "ScmEventType" scmtype)))
    (if (not sym)
	"<unknown>"
	(symbol->string sym))))

(define (make-event-object window xevent)
  (let* ((xtype (C-> xevent "XEvent type"))
	 (scmtype (event-type xtype window xevent)))

    (define (event . slots)
      (apply vector scmtype window slots))

    (and
     (not (eq? #f scmtype))
     (or (alien-null? window)
	 (not (zero? (bitwise-and (bit scmtype)
				  (C-call "x_window_event_mask" window)))))
     (cond

      ((eq? scmtype event-type:key-press)		; xtype = KeyPress
       (key-event window xevent event-type:key-press))

      ((eq? scmtype event-type:button-down)		; xtype = ButtonPress
       (button-event window xevent event-type:button-down))

      ((eq? scmtype event-type:button-up)		; xtype = ButtonRelease
       (button-event window xevent event-type:button-up))

      ((eq? scmtype event-type:motion)			; xtype = MotionNotify
       (event (C-> xevent "XMotionEvent x")
	      (C-> xevent "XMotionEvent y")
	      (x-key-button-mask-to-scheme
	       (C-> xevent "XMotionEvent state"))))

      ((eq? scmtype event-type:configure)		; xtype = ConfigureNotif
       (event (C-> xevent "XConfigureEvent width")
	      (C-> xevent "XConfigureEvent height")))

      ((eq? scmtype event-type:expose)
       (if (eq? xtype (C-enum "GraphicsExpose"))
	   (event (C-> xevent "XGraphicsExposeEvent x")	; xtype = GraphicsExpose
		  (C-> xevent "XGraphicsExposeEvent y")
		  (C-> xevent "XGraphicsExposeEvent width")
		  (C-> xevent "XGraphicsExposeEvent height")
		  1)
	   (event (C-> xevent "XExposeEvent x")		; xtype = Expose
		  (C-> xevent "XExposeEvent y")
		  (C-> xevent "XExposeEvent width")
		  (C-> xevent "XExposeEvent height")
		  0)))

      ((eq? scmtype event-type:delete-window)		; xtype = ClientMessage
       (event))

      ((eq? scmtype event-type:take-focus)		; xtype = ClientMessage
       (event (C-call "x_event_take_focus_time" window xevent)))

      ((eq? scmtype event-type:visibility)		; xtype = VisibilityNoti
       (event
	(let ((state (C-> xevent "XVisibilityEvent state")))
	  (cond ((eq? state (C-enum "VisibilityUnobscured")) 0)
		((eq? state (C-enum "VisibilityPartiallyObscured")) 1)
		((eq? state (C-enum "VisibilityFullyObscured")) 2)
		(else 3)))))

      ((eq? scmtype event-type:selection-clear)		; xtype = SelectionClear
       (event (C-> xevent "XSelectionClearEvent selection")
	      (C-> xevent "XSelectionClearEvent time")))

      ((eq? scmtype event-type:selection-notify)	; xtype = SelectionNotif
       (event (C-> xevent "XSelectionEvent requestor")
	      (C-> xevent "XSelectionEvent selection")
	      (C-> xevent "XSelectionEvent target")
	      (C-> xevent "XSelectionEvent property")
	      (C-> xevent "XSelectionEvent time")))

      ((eq? scmtype event-type:selection-request)	; xtype = SelectionReque
       (event (C-> xevent "XSelectionRequestEvent requestor")
	      (C-> xevent "XSelectionRequestEvent selection")
	      (C-> xevent "XSelectionRequestEvent target")
	      (C-> xevent "XSelectionRequestEvent property")
	      (C-> xevent "XSelectionRequestEvent time")))

      ((eq? scmtype event-type:property-notify)		; xtype = PropertyNotify
       (event
	;; Must be an alien Window because this window
	;; might not have a corresponding XW object.
	(C-> xevent "XPropertyEvent window")
	(C-> xevent "XPropertyEvent atom")
	(C-> xevent "XPropertyEvent time")
	(C-> xevent "XPropertyEvent state")))

      ((or (eq? scmtype event-type:enter)		; xtype = EnterNotify
	   (eq? scmtype event-type:leave)		; xtype = LeaveNotify
	   (eq? scmtype event-type:focus-in)		; xtype = FocusIn
	   (eq? scmtype event-type:focus-out)		; xtype = FocusOut
	   (eq? scmtype event-type:map)			; xtype = MapNotify
	   (eq? scmtype event-type:unmap))		; xtype = UnmapNotify
       (event))

      (else
       (warn "Mistranslated XEvent type.")
       #f)))))

(define (key-event window event type)
  (define-integrable buffer-size 80)
  (let ((buffer (malloc buffer-size 'char))
	(keysym-buffer (malloc (C-sizeof "KeySym") '|KeySym|)))
    ;; Make ShiftLock modifier not affect keys with other modifiers.
    (let ((state (C-> event "XKeyEvent state")))
      (if (and (not (zero? (bitwise-and state key-event-state-mask)))
	       (not (zero? (bitwise-and state (C-enum "LockMask")))))
	  (C->= event "XKeyEvent state" (bitwise-nand state
						      (C-enum "LockMask"))))
      (let* ((nbytes (C-call "x_lookup_string"
			     event buffer buffer-size keysym-buffer))
	     (keysym (C-> keysym-buffer "KeySym"))
	     (event (and (not (= keysym (C-enum "NoSymbol")))
			 (not (= (C-enum "True")
				 (C-call "IsModifierKey" keysym)))
			 (vector type
				 window
				 ;; If the BackSpace keysym is received, and
				 ;; XLookupString has translated it into ASCII
				 ;; backspace, substitute ASCII DEL instead.
				 (cond ((and (= keysym (C-enum "XK_BackSpace"))
					     (= nbytes 1)
					     (= (C-> buffer "char")
						(char->integer #\backspace)))
					"\177")
				       ((> nbytes 0)
					(let ((bv (make-bytevector nbytes)))
					  (c-peek-bytes buffer 0 nbytes bv 0)
					  (iso8859-1->string bv)))
				       (else ""))
				 ;; Create Scheme bucky bits (kept independent
				 ;; of the character).  X has already
				 ;; controlified, so Scheme may choose to
				 ;; ignore the control bucky bit.
				 (C-call "x_modifier_mask_to_bucky_bits"
					 state window)
				 keysym
				 (C-> event "XKeyEvent time")))))
	(free keysym-buffer)
	(free buffer)
	event))))

(define key-event-state-mask
  (+ (C-enum "ShiftMask")
     (C-enum "ControlMask")
     (C-enum "Mod1Mask") (C-enum "Mod2Mask") (C-enum "Mod3Mask")
     (C-enum "Mod4Mask") (C-enum "Mod5Mask")))

(define (button-event window event type)
  (vector type
	  window
	  (C-> event "XButtonEvent x")
	  (C-> event "XButtonEvent y")
	  (let ((button (C-> event "XButtonEvent button"))
		(state (C-> event "XButtonEvent state")))
	    (if (and (<= 1 button) (<= button 256))
		(+ (-1+ button)
		   (* 256 (C-call "x_modifier_mask_to_bucky_bits"
				  state window)))
		#f))
	  (C-> event "XButtonEvent time")))

(define (x-key-button-mask-to-scheme xstate)
  ;; I'm not sure why we have a function for this.
  (+ (if (eq? xstate (C-enum "ControlMask")) #x0001 0)
     (if (eq? xstate (C-enum "Mod1Mask"))    #x0002 0)
     (if (eq? xstate (C-enum "Mod2Mask"))    #x0004 0)
     (if (eq? xstate (C-enum "Mod3Mask"))    #x0008 0)
     (if (eq? xstate (C-enum "ShiftMask"))   #x0010 0)
     (if (eq? xstate (C-enum "LockMask"))    #x0020 0)
     (if (eq? xstate (C-enum "Mod4Mask"))    #x0040 0)
     (if (eq? xstate (C-enum "Mod5Mask"))    #x0080 0)
     (if (eq? xstate (C-enum "Button1Mask")) #x0100 0)
     (if (eq? xstate (C-enum "Button2Mask")) #x0200 0)
     (if (eq? xstate (C-enum "Button3Mask")) #x0400 0)
     (if (eq? xstate (C-enum "Button4Mask")) #x0800 0)
     (if (eq? xstate (C-enum "Button5Mask")) #x1000 0)))

(define (x-select-input display window event-mask)
  (guarantee-xdisplay display 'x-select-input)
  (guarantee-Window window 'x-select-input)
  (c-call "x_select_input" display window event-mask))

(define (x-window-event-mask window)
  (guarantee-xwindow window 'x-window-event-mask)
  (C-call "x_window_event_mask" window))

(define (x-window-set-event-mask window mask)
  (guarantee-xwindow window 'x-window-set-event-mask)
  (if (zero? (C-call "x_window_set_event_mask" window mask))
      (error "Bad mask:" mask)))

(define (x-window-or-event-mask window event-mask)
  (guarantee-xwindow window 'x-window-or-event-mask)
  (if (>= event-mask (c-enum "event_type_supremum"))
      (error:bad-range-argument event-mask 'x-window-andc-event-mask))
  (c-call "x_window_or_event_mask" window event-mask))

(define (x-window-andc-event-mask window event-mask)
  (guarantee-xwindow window 'x-window-andc-event-mask)
  (if (>= event-mask (c-enum "event_type_supremum"))
      (error:bad-range-argument event-mask 'x-window-andc-event-mask))
  (c-call "x_window_andc_event_mask" window event-mask))

(define event-type:button-down (C-enum "event_type_button_down"))
(define event-type:button-up (C-enum "event_type_button_up"))
(define event-type:configure (C-enum "event_type_configure"))
(define event-type:enter (C-enum "event_type_enter"))
(define event-type:focus-in (C-enum "event_type_focus_in"))
(define event-type:focus-out (C-enum "event_type_focus_out"))
(define event-type:key-press (C-enum "event_type_key_press"))
(define event-type:leave (C-enum "event_type_leave"))
(define event-type:motion (C-enum "event_type_motion"))
(define event-type:expose (C-enum "event_type_expose"))
(define event-type:delete-window (C-enum "event_type_delete_window"))
(define event-type:map (C-enum "event_type_map"))
(define event-type:unmap (C-enum "event_type_unmap"))
(define event-type:take-focus (C-enum "event_type_take_focus"))
(define event-type:visibility (C-enum "event_type_visibility"))
(define event-type:selection-clear (C-enum "event_type_selection_clear"))
(define event-type:selection-notify (C-enum "event_type_selection_notify"))
(define event-type:selection-request (C-enum "event_type_selection_request"))
(define event-type:property-notify (C-enum "event_type_property_notify"))
(define number-of-event-types (C-enum "event_type_supremum"))

;;; Miscellaneous

(define (x-window-display window)
  (guarantee-xwindow window 'x-window-display)
  (c-call "x_window_display" (make-alien '(struct xdisplay)) window))

(define (x-window-x-size window)
  (guarantee-xwindow window 'x-window-x-size)
  (C-call "x_window_x_size" window))

(define (x-window-y-size window)
  (guarantee-xwindow window 'x-window-y-size)
  (C-call "x_window_y_size" window))

(define (x-window-beep window)
  (guarantee-xwindow window 'x-window-beep)
  (C-call "x_window_beep" window))

(define (x-window-clear window)
  (guarantee-xwindow window 'x-window-clear)
  (C-call "x_window_clear" window))

(define (x-display-flush xd)
  (guarantee-xdisplay xd 'x-display-flush)
  (C-call "x_display_flush" xd))

(define (x-window-flush window)
  (guarantee-xwindow window 'x-window-flush)
  (C-call "x_window_flush" window))

(define (x-display-sync display discard?)
  (guarantee-xdisplay display 'x-display-sync)
  (c-call "x_display_sync" display (if discard? 1 0)))

(define (x-display-get-default display resource-name class-name)
  (guarantee-xdisplay display 'x-display-get-default)
  (let ((alien (C-call "x_display_get_default" (make-alien 'char) display
		       (->cstring resource-name) (->cstring class-name))))
    (and (not (alien-null? alien))
	 (c-peek-cstring alien))))

(define (x-window-query-pointer window)
  (guarantee-xwindow window 'x-window-query-pointer)
  (let ((result (malloc (* 5 (c-sizeof "int")) 'int)))
    (if (zero? (C-call "x_window_query_pointer" window result))
	(error "XQueryPointer failed:" window))
    (let ((v (make-vector 5))
	  (scan (copy-alien result)))
      (vector-set! v 0 (C-> scan "int"))
      (alien-byte-increment! scan (C-sizeof "int"))
      (vector-set! v 1 (C-> scan "int"))
      (alien-byte-increment! scan (C-sizeof "int"))
      (vector-set! v 2 (C-> scan "int"))
      (alien-byte-increment! scan (C-sizeof "int"))
      (vector-set! v 3 (C-> scan "int"))
      (alien-byte-increment! scan (C-sizeof "int"))
      (vector-set! v 4 (map-key-state (C-> scan "int")))
      (free result)
      v)))

(define map-key-state
  (let ((translations (list (cons (C-enum "ControlMask") #x0001)
			    (cons (C-enum "Mod1Mask")    #x0002)
			    (cons (C-enum "Mod2Mask")    #x0004)
			    (cons (C-enum "Mod3Mask")    #x0008)
			    (cons (C-enum "ShiftMask")   #x0010)
			    (cons (C-enum "LockMask")    #x0020)
			    (cons (C-enum "Mod4Mask")    #x0040)
			    (cons (C-enum "Mod5Mask")    #x0080)
			    (cons (C-enum "Button1Mask") #x0100)
			    (cons (C-enum "Button2Mask") #x0200)
			    (cons (C-enum "Button3Mask") #x0400)
			    (cons (C-enum "Button4Mask") #x0800)
			    (cons (C-enum "Button5Mask") #x1000))))
    (named-lambda (map-key-state state)
      (reduce bitwise-ior 0
	      (map (lambda (from.to)
		     (if (zero? (bitwise-and state (car from.to)))
			 0
			 (cdr from.to)))
		   translations)))))

(define (x-window-id window)
  (guarantee-xwindow window 'x-window-id)
  (C-call "x_window_id" window))

;;; Appearance Control Functions

(define (x-window-set-foreground-color window color)
  (guarantee-xwindow window 'x-window-set-foreground-color)
  (cond ((string? color)
	 (C-call "x_window_set_foreground_color_name" window (->cstring color)))
	((integer? color)
	 (C-call "x_window_set_foreground_color_pixel" window color))
	(else
	 (error:wrong-type-argument color "an X color (string or integer)"
				    'x-window-set-border-color))))

(define (x-window-set-background-color window color)
  (guarantee-xwindow window 'x-window-set-background-color)
  (cond ((string? color)
	 (C-call "x_window_set_background_color_name" window (->cstring color)))
	((integer? color)
	 (C-call "x_window_set_background_color_pixel" window color))
	(else
	 (error:wrong-type-argument color "an X color (string or integer)"
				    'x-window-set-background-color))))

(define (x-window-set-border-color window color)
  (guarantee-xwindow window 'x-window-set-border-color)
  (cond ((string? color)
	 (C-call "x_window_set_border_color_name" window (->cstring color)))
	((integer? color)
	 (C-call "x_window_set_border_color_pixel" window color))
	(else
	 (error:wrong-type-argument color "an X color (string or integer)"
				    'x-window-set-border-color))))

(define (x-window-set-cursor-color window color)
  (guarantee-xwindow window 'x-window-set-cursor-color)
  (cond ((string? color)
	 (C-call "x_window_set_cursor_color_name" window (->cstring color)))
	((integer? color)
	 (C-call "x_window_set_cursor_color_pixel" window color))
	(else
	 (error:wrong-type-argument color "an X color (string or integer)"
				    'x-window-set-border-color))))

(define (x-window-set-mouse-color window color)
  (guarantee-xwindow window 'x-window-set-mouse-color)
  (cond ((string? color)
	 (C-call "x_window_set_mouse_color_name" window (->cstring color)))
	((integer? color)
	 (C-call "x_window_set_mouse_color_pixel" window color))
	(else
	 (error:wrong-type-argument color "an X color (string or integer)"
				    'x-window-set-border-color))))

(define (x-window-set-mouse-shape window shape)
  (guarantee-xwindow window 'x-window-set-mouse-shape)
  (if (zero? (C-call "x_window_set_mouse_shape" window shape))
      (error "Bad shape:" shape)))

(define (x-window-set-font window font)
  (guarantee-xwindow window 'x-window-set-font)
  (guarantee string? font 'x-window-set-font)
  (not (zero? (C-call "x_window_set_font" window (->cstring font)))))

(define (x-window-set-border-width window width)
  (guarantee-xwindow window 'x-window-set-border-width)
  (C-call "x_window_set_border_width" window width))

(define (x-window-set-internal-border-width window width)
  (guarantee-xwindow window 'x-window-set-internal-border-width)
  (C-call "x_window_set_internal_border_width" window width))

;;; WM Communication

(define (x-window-set-input-focus window time)
  (guarantee-xwindow window 'x-window-set-input-focus)
  (if (not (zero? (c-call "x_window_set_input_focus" window time)))
      (error:bad-range-argument window 'x-window-set-input-focus)))

;;; WM Control

(define (x-window-map window)
  (guarantee-xwindow window 'x-window-map)
  (C-call "x_window_map" window))

(define (x-window-iconify window)
  (guarantee-xwindow window 'x-window-iconify)
  (C-call "x_window_iconify" window))

(define (x-window-withdraw window)
  (guarantee-xwindow window 'x-window-withdraw)
  (C-call "x_window_withdraw" window))

(define (x-window-set-size window width height)
  (guarantee-xwindow window 'x-window-set-size)
  (C-call "x_window_set_size" window width height))

(define (x-window-raise window)
  (guarantee-xwindow window 'x-window-raise)
  (C-call "x_window_raise" window))

(define (x-window-lower window)
  (guarantee-xwindow window 'x-window-lower)
  (C-call "x_window_lower" window))

(define (x-window-get-size window)
  (guarantee-xwindow window 'x-window-get-size)
  (let ((dimensions (malloc (* 2 (c-sizeof "int")) 'int)))
    (c-call "x_window_get_size" window dimensions)
    (let ((width (c-> dimensions "int"))
	  (height (c-> (c-array-loc dimensions "int" 1) "int")))
      (free dimensions)
      (cons width height))))

(define (x-window-get-position window)
  (guarantee-xwindow window 'x-window-get-position)
  (let ((coords (malloc (* 2 (c-sizeof "int")) 'int)))
    (c-call "x_window_get_position" window coords)
    (let ((x (c-> coords "int"))
	  (y (c-> (c-array-loc coords "int" 1) "int")))
      (free coords)
      (cons x y))))

(define (x-window-set-position window x y)
  (guarantee-xwindow window 'x-window-set-position)
  (C-call "x_window_set_position" window x y))

;;; Font Structure

(define (x-font-structure display name/id)

  (define (font-struct-cleanup! copy)
    (if (not (alien-null? copy))
	(begin
	  (C-call "x_free_font" display copy)
	  (alien-null! copy))))

  (guarantee-xdisplay display 'x-font-structure)
  (let ((font-struct (make-alien '(struct |XFontStruct|))))
    (cond ((string? name/id)
	   (let ((name (->cstring name/id)))
	     (add-alien-cleanup!
	      font-struct
	      (named-lambda (font-struct-init-by-name! copy)
		(C-call "x_font_structure_by_name" copy display name))
	      font-struct-cleanup!)))
	  ((integer? name/id)
	   (add-alien-cleanup!
	    font-struct
	    (named-lambda (font-struct-init-by-id! copy)
	      (C-call "x_font_structure_by_id" copy display name/id))
	    font-struct-cleanup!))
	  (else
	   (error:wrong-type-argument name/id "a string or integer"
				      'x-font-structure)))
    (if (alien-null? font-struct)
	(error "Could not load font:" name/id display))
    (let ((vector (copy-x-font-struct name/id font-struct)))
      (cleanup-alien! font-struct)
      vector)))

(define (copy-x-font-struct font-name font)
  (if (alien-null? font)
      #f
      ;; Handle only 8-bit fonts because of laziness.
      (if (or (not (zero? (C-> font "XFontStruct min_byte1")))
	      (not (zero? (C-> font "XFontStruct max_byte1"))))
	  #f
	  (let ((result (make-vector 10))
		(per-char (C-> font "XFontStruct per_char")))
	    (if (zero? per-char)
		(vector-set! result 6 #f)
		(let* ((start-index (C-> font "XFontStruct min_char_or_byte2"))
		       (length (- (C-> font "XFontStruct max_char_or_byte2")
				  start-index -1))
		       (character-vector (make-vector length)))
		  (let loop ((index 0))
		    (if (< index length)
			(begin
			  (vector-set! character-vector index
				       (copy-x-char-struct
					(alien-byte-increment
					 per-char
					 (* index (C-sizeof "XCharStruct")))))
			  (loop (1+ index)))))
		  (vector-set! result 6 start-index)
		  (vector-set! result 7 character-vector)))
	    (vector-set! result 0 font-name)
	    (vector-set! result 1 (C-> font "XFontStruct direction"))
	    (vector-set! result 2
			 (not (zero? (C-> font "XFontStruct all_chars_exist"))))
	    (vector-set! result 3 (C-> font "XFontStruct default_char"))
	    (vector-set! result 4 (copy-x-char-struct
				   (alien-byte-increment
				    font (C-offset "XFontStruct min_bounds"))))
	    (vector-set! result 5 (copy-x-char-struct
				   (alien-byte-increment
				    font (C-offset "XFontStruct max_bounds"))))
	    (vector-set! result 8 (C-> font "XFontStruct ascent"))
	    (vector-set! result 9 (C-> font "XFontStruct descent"))
	    result))))

(define (copy-x-char-struct char-struct)
  (let ((lbearing (C-> char-struct "XCharStruct lbearing"))
	(rbearing (C-> char-struct "XCharStruct rbearing"))
	(width (C-> char-struct "XCharStruct width"))
	(ascent (C-> char-struct "XCharStruct ascent"))
	(descent (C-> char-struct "XCharStruct descent")))
    (if (and (zero? lbearing) (zero? rbearing)
	     (zero? width) (zero? ascent) (zero? descent))
	#f
	(vector lbearing rbearing width ascent descent))))

(define (x-free-font display alien)
  (declare (ignore display))
  (cleanup-alien! alien))

(define (x-list-fonts display pattern limit)
  ;; LIMIT is an exact non-negative integer or #F for no limit.
  ;; Returns #F or a vector of at least one string.
  (guarantee-xdisplay display 'x-list-fonts)
  (and limit
       (guarantee exact-positive-integer? limit 'x-list-fonts))
  (let ((actual-count-return (malloc (c-sizeof "int") 'int))
	(pattern-bytes (->bytes pattern)))

    (define (cleanup-names! copy)
      (if (not (alien-null? copy))
	  (begin
	    (c-call "XFreeFontNames" copy)
	    (alien-null! copy))))

    (define (init-names! copy)
      (c-call "x_list_fonts" copy
	      display pattern-bytes (or limit 1000000) actual-count-return))

    (let ((names (make-alien '(* char))))
      (add-alien-cleanup! names cleanup-names! init-names!)
      (if (alien-null? names)
	  (begin
	    (cleanup-alien! names)
	    (free actual-count-return)
	    #f)
	  (let ((actual-count (c-> actual-count-return "int"))
		(scan (copy-alien names)))
	    (let ((result (make-vector actual-count)))
	      (let loop ((i 0))
		(if (< i actual-count)
		    (begin
		      (vector-set! result i (c-peek-cstringp! scan))
		      (loop (1+ i)))))
	      (cleanup-alien! names)
	      (free actual-count-return)
	      result))))))

;;; Atoms

(define (x-intern-atom display name soft?)
  (guarantee-xdisplay display 'x-intern-atom)
  (c-call "x_intern_atom" display (->cstring name) (if soft? 1 0)))

(define (x-get-atom-name display atom)

  (define (cleanup-name-return! copy)
    (if (not (alien-null? copy))
	(let ((cstr (c-> copy "* char")))
	  (if (not (alien-null? cstr))
	      (begin
		(c-call "XFree" cstr)
		(alien-null! cstr)))
	  ((ucode-primitive c-free 1) copy)
	  (alien-null! copy))))

  (define (init-name-return! copy)
    ((ucode-primitive c-malloc 2) copy (c-sizeof "* char")))

  (guarantee-xdisplay display 'x-get-atom-name)
  (let ((name-return (make-alien '(* char))))
    (add-alien-cleanup! name-return cleanup-name-return! init-name-return!)
    (let ((code (c-call "x_get_atom_name" display atom name-return)))
      (if (zero? code)
	  (let ((name (c-peek-cstringp name-return)))
	    (cleanup-alien! name-return)
	    name)
	  (error "XGetAtomName failed:" code)))))

;;; Window Properties

(define (x-get-window-property display window property long-offset
			       long-length delete? req-type)
  (guarantee-xdisplay display 'x-get-window-property)
  (guarantee-Window window 'x-get-window-property)
  (guarantee-Atom property 'x-get-window-property)
  (let ((actual-type-return (malloc (c-sizeof "Atom") '|Atom|))
	(actual-format-return (malloc (c-sizeof "int") 'int))
	(nitems-return (malloc (c-sizeof "ulong") 'ulong))
	(bytes-after-return (malloc (c-sizeof "ulong") 'ulong)))

    (define (cleanup-data-return! copy)
      (if (not (alien-null? copy))
	  (let ((cstr (c-> copy "* char")))
	    (if (not (alien-null? cstr))
		(begin
		  (c-call "XFree" cstr)
		  (alien-null! cstr)))
	    ((ucode-primitive c-free 1) copy)
	    (alien-null! copy))))

    (define (init-data-return! copy)
      ((ucode-primitive c-malloc 2) copy (c-sizeof "* char"))
      (c->= copy "* char" 0))

    (let ((data-return (make-alien '(* char))))
      (add-alien-cleanup! data-return cleanup-data-return! init-data-return!)
      (let ((code (c-call "x_get_window_property" display window property
			   long-offset long-length (if delete? 1 0) req-type
			   actual-type-return actual-format-return
			   nitems-return bytes-after-return data-return)))
	(if (not (zero? code))
	    (error "XGetWindowProperty failed."))
	(let ((actual-type (c-> actual-type-return "Atom"))
	      (actual-format (c-> actual-format-return "int")))
	  (let ((result
		 (vector actual-type
			 actual-format
			 (c-> bytes-after-return "ulong")
			 (cond ((and (not (= req-type
					     (c-enum "AnyPropertyType")))
				     (not (= req-type actual-type)))
				#f)
			       ((= 32 actual-format)
				(char-ptr-to-prop-data-32
				 (c-> data-return "* char")
				 (c-> nitems-return "ulong")))
			       ((= 16 actual-format)
				(char-ptr-to-prop-data-16
				 (c-> data-return "* char")
				 (c-> nitems-return "ulong")))
			       ((= 8 actual-format)
				(char-ptr-to-prop-data-8
				 (c-> data-return "* char")
				 (c-> nitems-return "ulong")))
			       (else
				(error "Unexpected format:" actual-format))))))
	    (cleanup-alien! data-return)
	    (free actual-type-return)
	    (free actual-format-return)
	    (free nitems-return)
	    (free bytes-after-return)
	    result))))))

(define (char-ptr-to-prop-data-32 data length)
  (let ((scan (copy-alien data))
	(result (make-vector length)))
    (let loop ((index 0))
      (if (< index length)
	  (begin
	    (vector-set! result index (c-> scan "CARD32"))
	    (alien-byte-increment! scan (c-sizeof "long"))
	    (loop (1+ index)))))
    result))

(define (char-ptr-to-prop-data-16 data length)
  (let ((scan (copy-alien data))
	(result (make-vector length)))
    (let loop ((index 0))
      (if (< index length)
	  (begin
	    (vector-set! result index (c-> scan "CARD16"))
	    (alien-byte-increment! scan (c-sizeof "CARD16"))
	    (loop (1+ index)))))
    result))

(define (char-ptr-to-prop-data-8 data length)
  (let ((string ((ucode-primitive string-allocate 1) length)))
    (if (> length 0)
	(c-peek-bytes data 0 length string 0))
    string))

(define (x-change-property display window property type format mode data)
  (guarantee-xdisplay display 'x-change-property)
  (guarantee-Window window 'x-change-property)
  (guarantee-Atom property 'x-change-property)
  (guarantee-Atom type 'x-change-property)
  (guarantee integer? format 'x-change-property)
  (guarantee integer? mode 'x-change-property)
  (let* ((bytes.length
	  (case format
	    ((8)
	     (guarantee string? data 'x-change-property)
	     (prop-data-8->bytes.length data))
	    ((16)
	     (guarantee vector? data 'x-change-property)
	     (prop-data-16->bytes.length data))
	    ((32)
	     (guarantee vector? data 'x-change-property)
	     (prop-data-32->bytes.length data))
	    (else
	     (error:bad-range-argument format 'x-change-property))))
	 (code
	  (c-call "x_change_property" display window property type format mode
		  (car bytes.length) (cdr bytes.length))))
    (free (car bytes.length))
    (if (not (zero? code))
	(error "XChangeProperty failed:" property))
    code))

(define (prop-data-32->bytes.length vector)
  (let* ((nitems (vector-length vector))
	 (length (* 4 nitems))
	 (bytes (malloc length 'uchar))
	 (scan (copy-alien bytes)))
    (let loop ((index 0))
      (if (< index nitems)
	  (let ((n (vector-ref vector index)))
	    (guarantee integer? n 'prop-data-32->bytes.length)
	    (c->= scan "CARD32" n)
	    (alien-byte-increment scan (c-sizeof "CARD32"))
	    (loop (1+ index)))))
    (cons bytes length)))

(define (prop-data-16->bytes.length vector)
  (let* ((nitems (vector-length vector))
	 (length (* (c-sizeof "CARD16") nitems))
	 (bytes (malloc length 'uchar))
	 (scan (copy-alien bytes)))
    (let loop ((index 0))
      (if (< index nitems)
	  (let ((n (vector-ref vector index)))
	    (guarantee integer? n 'prop-data-16->bytes.length)
	    (if (or (< n 0) (<= 65536 n))
		(error:bad-range-argument vector 'prop-data-16->bytes.length))
	    (c->= scan "CARD16" n)
	    (alien-byte-increment scan (c-sizeof "CARD16"))
	    (loop (1+ index)))))
    (cons bytes length)))

(define (prop-data-8->bytes.length string)
  (let* ((bytevector (->bytes string))
	 (length (bytevector-length bytevector))
	 (bytes (malloc length 'uchar)))
    (c-poke-bytes bytes 0 length bytevector 0)
    (cons bytes length)))

(define (x-delete-property display window property)
  (guarantee-xdisplay display 'x-delete-property)
  (guarantee-Window window 'x-delete-property)
  (c-call "x_delete_property" display window property))

;;; Selections

(define (x-set-selection-owner display selection owner time)
  (guarantee-xdisplay display 'x-set-selection-owner)
  (guarantee-Atom selection 'x-set-selection-owner)
  (guarantee-Window owner 'x-set-selection-owner)
  (guarantee-Time time 'x-set-selection-owner)
  (c-call "x_set_selection_owner" display selection owner time))

(define (x-get-selection-owner display selection)
  (guarantee-xdisplay display 'x-get-selection-owner)
  (c-call "x_get_selection_owner" display selection))

(define (x-convert-selection display selection target property requestor time)
  (guarantee-xdisplay display 'x-convert-selection)
  (guarantee-Atom selection 'x-convert-selection)
  (guarantee-Atom target 'x-convert-selection)
  (guarantee-Atom property 'x-convert-selection)
  (guarantee-Window requestor 'x-convert-selection)
  (guarantee-Time time 'x-convert-selection)
  (c-call "x_convert_selection"
	  display selection target property requestor time))

(define (x-send-selection-notify display requestor
				 selection target property time)
  (guarantee-xdisplay display 'x-send-selection-notify)
  (guarantee-Window requestor 'x-send-selection-notify) ;a Window
  (guarantee-Atom selection 'x-send-selection-notify) ;an Atom
  (guarantee-Atom target 'x-send-selection-notify)	 ;an Atom
  (guarantee-Atom property 'x-send-selection-notify)	 ;an Atom
  (guarantee-Time time 'x-send-selection-notify)	 ;a Time
  (c-call "x_send_selection_notify"
	  display requestor selection target property time))

;;; Guarantors

(define-integrable (guarantee-Atom object operator)
  (guarantee exact-nonnegative-integer? object operator))

(define-integrable (guarantee-Window object operator)
  (guarantee exact-positive-integer? object operator))

(define-integrable (guarantee-Time object operator)
  (guarantee exact-nonnegative-integer? object operator))

(define-integrable (guarantee-xvisual object operator)
  (if (not (and (alien? object)
		(equal? '(struct |xvisual|) (alien/ctype object))))
      (error:wrong-type-argument object "an xvisual alien" operator)
      object))

(define-integrable (guarantee-xdisplay object operator)
  (if (not (and (alien? object)
		(equal? '(struct |xdisplay|) (alien/ctype object))))
      (error:wrong-type-argument object "an xdisplay alien" operator)))

(define-integrable (guarantee-xwindow object operator)
  (if (not (and (alien? object)
		(equal? '(struct |xwindow|) (alien/ctype object))))
      (error:wrong-type-argument object "an xwindow alien" operator)))

;;;; Cleanups

;;; This weak list ensures that allocated memory is freed.  It
;;; associates an alien with a cleanup procedure.  If the alien is
;;; garbage collected, the procedure is applied to a copy of the
;;; alien.  The cleanup procedure should apply the correct library
;;; function, e.g. XFree or XFreeFont.  When the alien is to be freed,
;;; alien-cleanup! should be used to do the cleanup.

(define cleanups)
(define cleanups-mutex)

(define (reset-x11-cleanups!)
  (set! cleanups-mutex (make-thread-mutex))
  (set! cleanups '()))

(define (initialize-package!)
  (reset-x11-cleanups!)
  (add-gc-daemon! cleanup-x11!)
  (add-event-receiver! event:after-restore reset-x11-cleanups!))

(define (add-alien-cleanup! alien cleanup! init!)
  (let* ((copy (copy-alien alien))
	 (entry (weak-cons alien (make-cleanup copy cleanup!))))
    (with-thread-mutex-lock cleanups-mutex
      (lambda ()
	(set! cleanups (cons entry cleanups))))
    (init! copy)
    (copy-alien-address! alien copy)
    alien))

(define (make-cleanup copy cleanup!)
  (named-lambda (cleanup-thunk)
    (cleanup! copy)))

(define (cleanup-alien! alien)
  (with-thread-mutex-lock cleanups-mutex
    (lambda ()
      (let ((entry (weak-assq alien cleanups)))
	(if (not entry)
	    (warn "Could not cleanup:" alien)
	    (begin
	      ((weak-cdr entry))
	      (set! cleanups (delq! entry cleanups))))))))

(define (cleanup-x11!)
  (with-thread-mutex-try-lock cleanups-mutex
    (lambda ()
      (let loop ((entries cleanups)
		 (prev #f))
	(if (pair? entries)
	    (let ((entry (car entries))
		  (next (cdr entries)))
	      (if (weak-pair/car? entry)
		  (loop next entries)
		  (begin
		    ((weak-cdr entry))
		    (loop next prev)))))))
    (lambda ()
      unspecific)))

(define (weak-assq obj alist)
  (let loop ((alist alist))
    (if (null? alist) #f
	(let* ((entry (car alist))
	       (key (weak-car entry)))
	  (if (eq? obj key) entry
	      (loop (cdr alist)))))))

(initialize-package!)