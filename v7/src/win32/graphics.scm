#| -*-Scheme-*-

$Id: graphics.scm,v 1.20 2003/02/14 18:28:35 cph Exp $

Copyright (c) 1993-1999, 2002 Massachusetts Institute of Technology

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Scheme Graphics Operations
;;; package: (win32 scheme-graphics)

(declare (usual-integrations))

(define-structure (win32-device (conc-name win32-device/)
                                 (constructor %make-win32-device))

  width
  height
  bitmap
  hwnd
  hdc

  cursor-x
  cursor-y

  x-left y-bottom x-right y-top ;
  x-scale y-scale
    
  fg-color  bg-color
  pen-valid?
  line-width line-style

  invalid?
  ;; iv-left iv-top iv-right iv-bottom   ; invalidated region

  palette
)

(define (make-win32-device width height palette)
  (%make-win32-device
    width height
    #f #f #f

    0 0 ; cursor
    -1.0 -1.0 1.0 1.0
    (/ (- width 1) 2.0)  ; xscale
    (/ (- height 1) -2.0) ; yscale
    #x000000 #xffffff
    #f
    0 0

    #f
    ;;0 0 0 0
    palette
))


(define-integrable (win32-xform-x window x)
  (round->exact (* (- x (win32-device/x-left window))
                   (win32-device/x-scale window))))

(define-integrable (win32-xform-y window y)
  (round->exact (* (- y (win32-device/y-top window))
                   (win32-device/y-scale window))))



(define (graphics-error . complaint)
  (apply (access error system-global-environment) complaint))


(define (make-scheme-graphics-wndproc window)

  (define ps (make-paintstruct))

  (lambda (hwnd msg wparam lparam)

    (define (default) (default-scheme-wndproc hwnd msg wparam lparam))

    (define (update-palette)
      (let* ((palette (win32-device/palette window))
	     (hdc     (get-dc hwnd))
	     (oldpal  (select-palette hdc palette #f))
	     (ncolors-changed (realize-palette hdc))
	     (redisplay  (> ncolors-changed 0)))
	(if redisplay
	    (begin
	      (invalidate-rect hwnd #f #f)
	      (update-window hwnd)
	      ))
	(select-palette hdc oldpal #f)
	(release-dc hwnd hdc)
	redisplay))

    (cond
     ((= msg WM_CREATE)
      (let* ((palette    (win32-device/palette window))
	     (hdc        (get-dc hwnd))
	     (bitmap-dc)
	     (bitmap))
	
	(if palette
	    (select-palette hdc palette #f))
	(set! bitmap-dc  (create-compatible-dc hdc))
	(if palette
	    (select-palette bitmap-dc palette #f))
	(set! bitmap   
	      (create-compatible-bitmap hdc (win32-device/width window)
					(win32-device/height window)))
	(if palette
	    (realize-palette hdc))
	
	(set-win32-device/bitmap! window bitmap)
	(set-win32-device/hwnd! window hwnd)
	(set-win32-device/hdc! window bitmap-dc)
	(set-bk-color bitmap-dc (win32-device/bg-color window))
	(set-bk-mode bitmap-dc TRANSPARENT)
	(set-text-align bitmap-dc (+ TA_BASELINE TA_LEFT TA_NOUPDATECP))
	(select-object bitmap-dc bitmap)
	(if palette
	    (realize-palette bitmap-dc))
	;;(delete-dc bitmap-dc)
	(release-dc hwnd hdc)
	(win32-device/clear window)
	0))

     ((= msg WM_CLOSE)
      ;; Ignore WM_CLOSE.  This is a workaround for a nasty bug in
      ;; Windows NT 4.0.  The bug is reproduced by making graphics
      ;; windows and closing them with the close button; the result is
      ;; the BSOD.
      0)
     
     ((= msg WM_DESTROY)
      (let ((bitmap-dc (win32-device/hdc window)))
	(if (not (eqv? 0 bitmap-dc))
	    (begin
	      (delete-dc bitmap-dc)
	      (set-win32-device/hdc! window #f))))
      (let ((bitmap (win32-device/bitmap window)))
	(if bitmap
	    (begin
	      (delete-object bitmap)
	      (set-win32-device/bitmap! window #f))))
      (let ((palette (win32-device/palette window)))
	(if palette
	    (begin
	      (delete-object palette)
	      (set-win32-device/palette! window #f))))
      (set-win32-device/hwnd! window #f)
      0)

     ((= msg WM_PAINT)
      ;; flush here so that uncovering parts of a buffered window causes
      ;; other visible buffered parts to be updated
      (win32-device/flush window #T)
      (let*  ((hdc      (begin-paint hwnd ps))
	      (palette  (win32-device/palette window))
	      )
	(if palette
	    (begin
	      (select-palette hdc palette #f)
	      (realize-palette hdc)
	      (realize-palette (win32-device/hdc window))
	    ))
	(bit-blt hdc 0 0 
		 (win32-device/width window) (win32-device/height window)
		 (win32-device/hdc window) 0 0 SRCCOPY)
	(end-paint hwnd ps))
      0)

     ((= msg WM_PALETTECHANGED)
      (if (and (win32-device/palette window) (not (= wparam hwnd)))
	  (let* ((hdc (get-dc hwnd)))
	    (realize-palette hdc)
	    (invalidate-rect hwnd #f #f)
	    (release-dc hwnd hdc)
	    ))
       0)
	    
     ((and (= msg WM_PALETTEISCHANGING) (win32-device/palette window))
      (default))
	    
     ((and (= msg WM_QUERYNEWPALETTE) (win32-device/palette window))
      (update-palette))

     ((and (= msg WM_ACTIVATE)
	   (not (= wparam 0)) (win32-device/palette window))
      (update-palette)
      0)

     ((and (= msg WM_SETFOCUS) (win32-device/palette window))
      (update-palette)
      0)

     ((= msg WM_SIZE)
      (if (is-iconic? hwnd)
	  0
	  (let*  ((new-w    (loword lparam))
		  (new-h    (hiword lparam))
		  (old-w    (win32-device/width window))
		  (old-h    (win32-device/height window)))
	    (if (and (= new-w old-w) (= new-h old-h))
		0
		(let*  ((palette    (win32-device/palette window))
			(hdc        (get-dc hwnd))
			(bitmap-dc  (create-compatible-dc hdc))
			(bitmap     (create-compatible-bitmap hdc new-w new-h))
			(old-bm     (win32-device/bitmap window))
			(old-bm-dc  (win32-device/hdc window)))
		  (select-object bitmap-dc bitmap)
		  (if palette
		      (begin
			(select-palette bitmap-dc palette #f)
			(realize-palette bitmap-dc)))
		  (set-bk-color bitmap-dc (win32-device/bg-color window))
		  (set-bk-mode  bitmap-dc TRANSPARENT)
		  (set-text-align bitmap-dc
				  (+ TA_BASELINE TA_LEFT TA_NOUPDATECP))
		  (set-stretch-blt-mode bitmap-dc COLORONCOLOR #|HALFTONE|#)
		  (stretch-blt bitmap-dc 0 0 new-w new-h
			       old-bm-dc 0 0 old-w old-h
			       SRCCOPY)
		  (set-win32-device/bitmap! window bitmap)
		  (set-win32-device/hdc!    window bitmap-dc)
		  (set-win32-device/width!  window new-w)
		  (set-win32-device/height! window new-h)
		  (set-win32-device/pen-valid?! window #f)
		  (set-rop2 bitmap-dc (get-rop2 old-bm-dc))
		  (win32-device/rescale window)
		  (delete-dc old-bm-dc)
		  (delete-object old-bm)
		  (release-dc hwnd hdc)
		  (invalidate-rect hwnd #f #f)
		  0)))))

    ((= msg WM_NCLBUTTONDOWN)
      (win32-device/flush window #F)
      (default))

    (else
      (default)))))
	

(define (make-standard-palette)
  (define pal (make-string (+ 4 (* 4 256))))
  (define i 0)
  (define (alloc r g b f)
    (let ((base (fix:+ 4 (fix:* i 4))))
      (vector-8b-set! pal base       r)
      (vector-8b-set! pal (+ base 1) g)
      (vector-8b-set! pal (+ base 2) b)
      (vector-8b-set! pal (+ base 3) f)
      (set! i (1+ i))))

  ;; RGB intensities scaled to look good.  Notice that 128 is in the list
  (define cv #(0 88 128 170 212 255))
  (alloc 0 0 0 0)		; Black is matched
  (alloc 255 255 255 0)		; White is matched
  ; Grays are not matched: this ensures order of colors in palette is
  ; always the same when the graphics window has the focus
  (do ((grey 48 (+ grey 8)))
      ((>= grey 255))
    (alloc grey grey grey PC_NOCOLLAPSE))
  (do ((r 0 (1+ r)))
      ((> r 5))
    (do ((g 0 (1+ g)))
	((> g 5))
      (do ((b 0 (1+ b)))
	  ((> b 5))
	(if (not (= r g b))
	    (alloc (vector-ref cv r) (vector-ref cv g) (vector-ref cv b)
		   PC_NOCOLLAPSE)))))
  (vector-8b-set! pal 0 #x00)
  (vector-8b-set! pal 1 #x03)
  (vector-8b-set! pal 2 (fix:and i 255))
  (vector-8b-set! pal 3 (fix:lsh i -8))
  (create-palette pal)
)

(define (make-grayscale-palette)
  (define pal (make-string (+ 4 (* 4 256))))
  (define i 0)
  (define (alloc r g b f)
    (let ((base (fix:+ 4 (fix:* i 4))))
      (vector-8b-set! pal base           r)
      (vector-8b-set! pal (fix:+ base 1) g)
      (vector-8b-set! pal (fix:+ base 2) b)
      (vector-8b-set! pal (fix:+ base 3) f)
      (set! i (1+ i))))

  (alloc 0 0 0 0)		; Black is matched
  ; Grays are not matched: this ensures order of colors in palette is always
  ; the same when the graphics window has the focus, which is important in
  ; maintaining consistency between the screen image and backing bitmap.
  (do ((grey 24 (fix:+ grey 1)))
      ((> grey 254))
    (alloc grey grey grey PC_NOCOLLAPSE))
  (alloc 255 255 255 0)		; White is matched
  (vector-8b-set! pal 0 #x00)
  (vector-8b-set! pal 1 #x03)
  (vector-8b-set! pal 2 (fix:and i 255))
  (vector-8b-set! pal 3 (fix:lsh i -8))
  (create-palette pal)
)


(define (make-grayscale-128-palette)
  (define pal (make-string (+ 4 (* 4 256))))
  (define i 0)
  (define (alloc r g b f)
    (let ((base (fix:+ 4 (fix:* i 4))))
      (vector-8b-set! pal base           r)
      (vector-8b-set! pal (fix:+ base 1) g)
      (vector-8b-set! pal (fix:+ base 2) b)
      (vector-8b-set! pal (fix:+ base 3) f)
      (set! i (1+ i))))

  (alloc 0 0 0 0)		; Black is matched
  ; Grays are not matched: this ensures order of colors in palette is always
  ; the same when the graphcis window has the focus.
  (do ((grey 2 (fix:+ grey 2)))
      ((> grey 254))
    (alloc grey grey grey PC_NOCOLLAPSE))
  (alloc 255 255 255 0)		; White is matched
  (vector-8b-set! pal 0 #x00)
  (vector-8b-set! pal 1 #x03)
  (vector-8b-set! pal 2 (fix:and i 255))
  (vector-8b-set! pal 3 (fix:lsh i -8))
  (create-palette pal)
)

(define (external-palette? object)
  (and (vector? object)
       (let ((l (vector-length object)))
	 (and (<= 2 l 256)
	      (let loop ((i 0))
		(or (= i l)
		    (and (let ((elt (vector-ref object i)))
			   (and (exact-nonnegative-integer? elt)
				(< elt #x100000000)))
			 (loop (+ i 1)))))))))

(define (convert-palette external)
  (let ((s (make-string (+ 4 (* 4 256))))
	(n-entries (vector-length external)))
    (vector-8b-set! s 0 #x00)
    (vector-8b-set! s 1 #x03)
    (vector-8b-set! s 2 (fix:and #xFF n-entries))
    (vector-8b-set! s 3 (fix:and #xFF (fix:lsh n-entries -8)))
    (do ((i 0 (fix:+ i 1))
	 (j 4 (fix:+ j 4)))
	((fix:= i n-entries))
      (let ((elt (vector-ref external i)))
	(let ((rgb (remainder elt #x1000000))
	      (bits (quotient elt #x1000000)))
	  (vector-8b-set! s j (fix:and #xFF elt))
	  (vector-8b-set! s (fix:+ j 1) (fix:and #xFF (fix:lsh elt -8)))
	  (vector-8b-set! s (fix:+ j 2) (fix:and #xFF (fix:lsh elt -16)))
	  (vector-8b-set! s (fix:+ j 3)
			  (if (or (fix:= 0 rgb) (fix:= #xFFFFFF rgb))
			      0
			      bits)))))
    (create-palette s)))

(define (client-width->window-width w)
  (+ w (* 2 (get-system-metrics SM_CXFRAME))))

(define (client-height->window-height h)
  (+ h (- (get-system-metrics SM_CYCAPTION)
          (get-system-metrics SM_CYBORDER))
       (* 2 (get-system-metrics SM_CYFRAME))))


(define device-protection-list)

(define (win32-graphics/open descriptor->device 
			     #!optional width height palette)
  (let* ((width   (if (default-object? width)  512 width))
         (height  (if (default-object? height) 512 height))
	 (palette
	  (cond ((default-object? palette) (make-standard-palette))
		((eq? palette 'GRAYSCALE) (make-grayscale-palette))
		((eq? palette 'GRAYSCALE-128) (make-grayscale-128-palette))
		((eq? palette 'STANDARD) (make-standard-palette))
		((eq? palette 'SYSTEM) #f)
		((external-palette? palette) (convert-palette palette))
		(else #f)))
         (descriptor (make-win32-device width height palette))
	 (wndproc    (make-scheme-graphics-wndproc descriptor))
         (w
	   (create-scheme-window
	     (+)
	     "SCHEME-GRAPHICS"
	     "Scheme Graphics"
             (+ WS_OVERLAPPED WS_CAPTION WS_THICKFRAME
                WS_SYSMENU WS_MINIMIZEBOX)
	     CW_USEDEFAULT CW_USEDEFAULT
             (client-width->window-width width)
             (client-height->window-height height)
	     0 0 (get-handle 0) 0
             wndproc)))
    (show-window w SW_SHOWNOACTIVATE)
    (let ((device (descriptor->device descriptor)))
      (protection-list/add! device-protection-list device descriptor)
      device)))

(define (win32-device/select-pen window)
  (let* ((hdc      (win32-device/hdc window))
         (new-pen  (create-pen (win32-device/line-style window)
	                       (win32-device/line-width window)
	 		       (win32-device/fg-color window)))
	 (old-pen  (select-object hdc new-pen)))
    (delete-object old-pen)
    (set-win32-device/pen-valid?! window #t)))
  
(define-integrable (win32-device/validate-pen window)
  (if (not (win32-device/pen-valid? window))
      (win32-device/select-pen window)))


(define (win32-graphics/device-coordinate-limits device)
  (define window (graphics-device/descriptor device))
  (values 0
          (- (win32-device/height window) 1)
	  (- (win32-device/width window) 1)
	  0))

(define (win32-graphics/coordinate-limits device)
  (define window (graphics-device/descriptor device))
  (values (win32-device/x-left   window)
          (win32-device/y-bottom window)
          (win32-device/x-right  window)
          (win32-device/y-top    window)))

(define (win32-graphics/set-coordinate-limits device
                x-left y-bottom x-right y-top)
  (let ((window (graphics-device/descriptor device)))
    (win32-graphics/reset-clip-rectangle device)
    (set-win32-device/x-left!    window  x-left)
    (set-win32-device/y-bottom!  window  y-bottom)
    (set-win32-device/x-right!   window  x-right)
    (set-win32-device/y-top!     window  y-top)
    (win32-device/rescale window)
    unspecific))

(define (win32-device/rescale window)
  (set-win32-device/x-scale!   window
    (/ (- (win32-device/width window) 1)
       (- (win32-device/x-right window) (win32-device/x-left window))))
  (set-win32-device/y-scale!   window
    (/ (- (win32-device/height window) 1)
      (- (win32-device/y-bottom window) (win32-device/y-top window)))))
    
(define (win32-translate-drawing-mode mode)
  (case mode				;X11 function names:
    (( 0) R2_BLACK)			;GXclear
    (( 1) R2_MASKPEN)			;GXand
    (( 2) R2_MASKPENNOT)		;GXandReverse
    (( 3) R2_COPYPEN)			;GXcopy
    (( 4) R2_MASKNOTPEN)		;GXandInverted
    (( 5) R2_NOP)			;GXnoop
    (( 6) R2_XORPEN)			;GXxor
    (( 7) R2_MERGEPEN)			;GXor
    (( 8) R2_NOTMERGEPEN)		;GXnor
    (( 9) R2_NOTXORPEN)			;GXequiv
    ((10) R2_NOT)			;GXinvert
    ((11) R2_MERGEPENNOT)		;GXorReverse
    ((12) R2_NOTCOPYPEN)		;GXcopyInverted
    ((13) R2_MERGENOTPEN)		;GXorInverted
    ((14) R2_NOTMASKPEN)		;GXnand
    ((15) R2_WHITE)			;GXset
    (else (error:bad-range-argument mode 'WIN32-TRANSLATE-DRAWING-MODE))))

(define (win32-graphics/set-drawing-mode device mode)
  (let* ((window (graphics-device/descriptor device))
         (hdc    (win32-device/hdc window))
         (rop2   (win32-translate-drawing-mode mode)))
    (set-rop2 hdc rop2)))

(define (win32-graphics/set-line-style device style)
  (let ((window (graphics-device/descriptor device)))
    (set-win32-device/line-style! window style)
    (set-win32-device/pen-valid?! window #f)
    unspecific))

(define (win32-graphics/set-line-width device width)
  (let ((window (graphics-device/descriptor device)))
    (set-win32-device/line-width! window width)
    (set-win32-device/pen-valid?! window #f)
    unspecific))

(define-integrable (win32-device/invalidate! window)
  (set-win32-device/invalid?!  window #t))

(define (win32-device/flush window immediate-update?)
  (if (win32-device/invalid? window)
      (let ((hwnd   (win32-device/hwnd window)))
	(set-win32-device/invalid?! window #f)
	(if hwnd
	    (begin
	      (invalidate-rect hwnd #f #f)
	      (if immediate-update?
		  (update-window hwnd))
	      unspecific)
	    (graphics-error
	     "Attempt to use deleted Scheme Graphics window" window)))))

(define (win32-graphics/flush device)
  ;; If we are in buffered mode, GRAPHICS-FLUSH was called explicitly
  ;; by the programmer.  If we redraw synchronously, then we get most
  ;; of the benefits of double-buffering.
  (win32-device/flush (graphics-device/descriptor device)
		      (graphics-device/buffer? device)))


(define (win32-device/clear window)
  (win32-device/validate-pen window)
  (let* ((hdc (win32-device/hdc window))
         (w   (win32-device/width window))
         (h   (win32-device/height window))
	 (penwidth (+ 1 (win32-device/line-width window)))
	 (rgb (win32-device/bg-color window)))
    (define newbrush (create-solid-brush rgb))
    (define oldbrush (select-object hdc newbrush))
    (rectangle hdc (- penwidth) (- penwidth) (+ w penwidth) (+ h penwidth))
    (select-object hdc oldbrush)
    (delete-object newbrush)
    (win32-device/invalidate! window)
    unspecific))

(define (win32-graphics/clear device)
  (win32-device/clear (graphics-device/descriptor device)))

(define (win32-graphics/draw-line device x1 y1 x2 y2)
  (let* ((window (graphics-device/descriptor device))
         (hdc    (win32-device/hdc window))
         (x1t    (win32-xform-x window x1))
         (y1t    (win32-xform-y window y1))
         (x2t    (win32-xform-x window x2))
         (y2t    (win32-xform-y window y2)))
    ;(realize-palette hdc)
    (win32-device/validate-pen window)
    (move-to-ex hdc x1t y1t #f)
    (line-to hdc x2t y2t)
    (line-to hdc (+ 1 x2t) y2t)		;; draws end point
    (win32-device/invalidate! window)
    unspecific))

(define (win32-graphics/draw-point device x y)
  ;; use line-to to get ROP2 effects
  (let* ((window (graphics-device/descriptor device))
         (hdc    (win32-device/hdc window))
	 ;(color  (win32-device/fg-color window))
         (xt     (win32-xform-x window x))
         (yt     (win32-xform-y window y)))
    (win32-device/validate-pen window)
    (move-to-ex hdc xt yt #f)
    (line-to hdc (+ 1 xt) yt)
;    (set-pixel hdc xt yt color)
    (win32-device/invalidate! window)
    unspecific))

(define (win32-graphics/draw-text device x y text)
  (let* ((window (graphics-device/descriptor device))
         (hdc    (win32-device/hdc window))
         (xt     (win32-xform-x window x))
         (yt     (win32-xform-y window y)))
    (win32-device/validate-pen window)
    (text-out hdc xt yt text (string-length text))
    (win32-device/invalidate! window)
    unspecific))
  

(define (win32-graphics/move-cursor device x y)
  (let ((window (graphics-device/descriptor device)))
    (set-win32-device/cursor-x! window x)
    (set-win32-device/cursor-y! window y)
    unspecific))

(define (win32-graphics/drag-cursor device x y)
  ;; use line-to to get ROP2 effects
  (let ((window (graphics-device/descriptor device)))
    (win32-graphics/draw-line device
      (win32-device/cursor-x window) (win32-device/cursor-y window) x y)
    (set-win32-device/cursor-x! window x)
    (set-win32-device/cursor-y! window y)
    unspecific))


(define (win32-graphics/draw-ellipse device left top right bottom)
  (let* ((window (graphics-device/descriptor device))
         (hdc    (win32-device/hdc window))
         (x1t    (win32-xform-x window left))
         (y1t    (win32-xform-y window top))
         (x2t    (win32-xform-x window right))
         (y2t    (win32-xform-y window bottom)))
    (win32-device/validate-pen window)
    (arc hdc x1t y1t x2t y2t x1t y1t x1t y1t)
    (win32-device/invalidate! window)
    unspecific))


(define (make-C-point-vector window vec)
  (let* ((n  (vector-length vec))
         (s  (make-string (* 4 n))))
    (define (loop i)
      (if (fix:< i n)
	(begin
	  (int32-offset-set! s (fix:* i 4)
	    (win32-xform-x window (vector-ref vec i)))
	  (int32-offset-set! s (fix:* (+ 1 i) 4)
	    (win32-xform-y window (vector-ref vec (+ 1 i))))
	  (loop (fix:+ i 2)))))
    (loop 0)
    s))


(define (win32-graphics/fill-polygon device point-vector)
  (let* ((window        (graphics-device/descriptor device))
         (hdc		(win32-device/hdc window))
         (old-width     (win32-device/line-width window))
	 (pt-vec        (make-C-point-vector  window point-vector)))

    (dynamic-wind
      (lambda ()
	(win32-graphics/set-line-width device 0))
      (lambda ()
	(let* ((newbrush (create-solid-brush (win32-device/fg-color window)))
	       (oldbrush (select-object hdc newbrush)))
          (polygon hdc pt-vec (fix:lsh (vector-length point-vector) -1))
	  ;(polyline hdc pt-vec (fix:lsh (vector-length point-vector) -1))
	  (select-object hdc oldbrush)
	  (delete-object newbrush)))
      (lambda ()
	(win32-graphics/set-line-width device old-width)))
    (win32-device/invalidate! window)))


(define (win32-graphics/copy-area device x-left y-top width height
              destination-x-left destination-y-top)
  (let* ((window (graphics-device/descriptor device))
         (hdc    (win32-device/hdc window))
         (x1t    (win32-xform-x window x-left))
         (y1t    (win32-xform-y window y-top))
	 (x2t    (win32-xform-x window (+ x-left width)))
	 (y2t    (win32-xform-y window (+ y-top height)))
	 (widtht (- x2t x1t))
	 (heightt (- y2t y1t))
         (xdt    (win32-xform-x window destination-x-left))
         (ydt    (win32-xform-y window destination-y-top))
	 (x0) (y0) (x1) (y1) (w) (h)
	 )
    (if (< widtht 0)
      (begin
	(set! x0 x2t)
	(set! x1 (+ xdt widtht))
	(set! w  (- widtht)))
      (begin
	(set! x0 x1t)
	(set! x1 xdt)
	(set! w widtht)))
    (if (< heightt 0)
      (begin
	(set! y0 y2t)
	(set! y1 (+ ydt heightt))
	(set! h  (- heightt)))
      (begin
	(set! y0 y1t)
	(set! y1 ydt)
	(set! h heightt)))

    (bit-blt hdc x1 y1 w h hdc x0 y0 SRCCOPY)
	  
    (win32-device/invalidate! window)
    unspecific))

(define (win32-graphics/open? device)
  (if (win32-device/hwnd (graphics-device/descriptor device)) #t #f))

(define (win32-graphics/close device)
  (close-descriptor (graphics-device/descriptor device)))

(define (close-descriptor descriptor)
  (if (and descriptor (win32-device/hwnd descriptor))
      (destroy-window (win32-device/hwnd descriptor))))

(define (win32-graphics/set-clip-rectangle device
           x-left y-bottom x-right y-top)
  (define window (graphics-device/descriptor device))
  (let* ((hdc (win32-device/hdc window))
         (x1t  (win32-xform-x window x-left))
         (y1t  (win32-xform-y window y-bottom))
         (x2t  (win32-xform-x window x-right))
         (y2t  (win32-xform-y window y-top))
	 (rgn  (create-rect-rgn x1t y2t x2t y1t)))
    (select-clip-rgn hdc rgn)
    unspecific))

(define (win32-graphics/reset-clip-rectangle device)
  (define window (graphics-device/descriptor device))
  (let* ((hdc (win32-device/hdc window)))
    (select-clip-rgn hdc 0)
    unspecific))

;;
;;  Colors
;;
;; WIN32/FIND-COLOR returns a BGR encoded integer.
;; ->COLOR returns a PALETTERGB encoded color.  All color uses internal to this
;;  file should use ->COLOR.

(define color-table)

(define (win32/define-color name spec)
  (set! color-table (cons (cons name (win32/find-color spec)) color-table))
  unspecific)

(define (win32/find-color spec)
  (define (rgb r g b)
    (+ r (* g 256) (* b 65536)))
  (define (rgb-hex spec width)
    (let* ((pos1 (fix:+ 1 width))
	   (pos2 (fix:+ pos1 width))
	   (pos3 (fix:+ pos2 width)))
      (rgb (string->number (substring spec    1 pos1) 16)
	   (string->number (substring spec pos1 pos2) 16)
	   (string->number (substring spec pos2 pos3) 16))))
  (define (dim? x)
    (and (exact-nonnegative-integer? x)
	 (<= x #x100)))
  (cond ((and (exact-nonnegative-integer? spec) (<= spec #x1000000))
	 spec)
        ((and (vector? spec)
	      (= 3 (vector-length spec))
	      (dim? (vector-ref spec 0))
	      (dim? (vector-ref spec 1))
	      (dim? (vector-ref spec 2)))
	 (rgb (vector-ref spec 0) (vector-ref spec 1) (vector-ref spec 2)))
        ((and (list? spec)
	      (= 3 (length spec))
	      (for-all? spec dim?))
	 (rgb (list-ref spec 0) (list-ref spec 1) (list-ref spec 2)))
	((and (string? spec)
	      (= 7 (string-length spec))
	      (char=? (string-ref spec 0) #\#))
	 (rgb-hex spec 2))
	((string? spec)
	 (let  ((pair
		 (list-search-positive color-table
		   (lambda (pair) (string-ci=? (car pair) spec)))))
	   (if pair
	       (cdr pair)
	       (graphics-error "Unknown color name:" spec))))
	(else
	 (graphics-error "Illegal color" spec))))

(define (win32-graphics/define-color device name spec)
  device
  (win32/define-color name spec))

(define (win32-graphics/find-color device spec)
  device
  (->color spec))

(define (->color spec)
  (let ((rgb (win32/find-color spec)))
    (if (< rgb #x02000000)
	(+ rgb #x02000000)		; force palette RGB
	spec)))

(define initial-color-definitions
  `(("red"          255   0   0)
    ("green"          0 255   0)
    ("blue"           0   0 255)
    ("cyan"           0 255 255)
    ("magenta"      255   0 255)
    ("yellow"       255 255   0)
    ("black"          0   0   0)
    ("dark gray"     63  63  63)
    ("dark grey"     63  63  63)
    ("grey"         127 127 127)
    ("gray"         127 127 127)
    ("light gray"   191 191 191)
    ("light grey"   191 191 191)
    ("white"        255 255 255)
    ("purple"	    127   0 127)
    ("dark green"     0 127   0)
    ("orange"       255 135   0)
    ("pink"         255 181 197)
    ("brown"        127  63   0)))

    
(define (win32-graphics/set-foreground-color device color)
  (let* ((window (graphics-device/descriptor device))
         (hdc    (win32-device/hdc window))
	 (rgb    (->color color)))
    (set-win32-device/fg-color!  window rgb)
    (set-win32-device/pen-valid?! window #f)
    (set-text-color hdc rgb))
  unspecific)

(define (win32-graphics/set-background-color device color)
  (define window (graphics-device/descriptor device))
  (define rgb (->color color))
  (set-win32-device/bg-color! window rgb)
  unspecific)

(define (win32-graphics/available?)
  (implemented-primitive-procedure?
   (ucode-primitive set-general-scheme-wndproc 1)))

(define (win32-graphics/image-depth device)
  device
  8)

;;----------------------------------------------------------------------------

(define (win32-graphics/move-window device x y)
  (let* ((window (graphics-device/descriptor device))
         (hwnd   (win32-device/hwnd window)))
    (set-window-pos hwnd 0 x y 0 0
		    (+ SWP_NOZORDER SWP_NOSIZE SWP_NOACTIVATE))))
      
(define (win32-graphics/resize-window device w h)
  (let* ((window (graphics-device/descriptor device))
         (hwnd   (win32-device/hwnd window)))
    (set-window-pos hwnd 0 0 0
      (client-width->window-width w)
      (client-height->window-height h)
      (+ SWP_NOZORDER SWP_NOMOVE SWP_NOACTIVATE))))
      
(define (win32-graphics/set-font device font)
  (let* ((window (graphics-device/descriptor device))
         (hdc    (win32-device/hdc window)))
    (select-object hdc font)
    unspecific))

(define (win32-graphics/set-window-name device name)
  (let* ((window (graphics-device/descriptor device))
         (hwnd   (win32-device/hwnd window)))
    (set-window-text hwnd name)
    unspecific))
;;-----------------------------------------------------------------------------
;;

(define dib-image-type)

(define (dib-image/destroy image)
  (let* ((dib    (image/descriptor image)))
    (delete-dib dib)))

(define (dib-image/create device width height)
  (let* ((window  (graphics-device/descriptor device))
	 (palette (win32-device/palette window))
	 (dib     (create-dib width height BI_RGB 8 palette)))
    dib))

(define (dib-image/width image)   (dib-width  (image/descriptor image)))
(define (dib-image/height image)  (dib-height (image/descriptor image)))

(define (dib-image/draw device x y image)
  (let*  ((dib    (image/descriptor image))
	  (window (graphics-device/descriptor device))
	  (hdc    (win32-device/hdc window))
	  (w      (dib-width dib))
	  (h      (dib-height dib)))
    (if (and (= x 0) (= y 0)
	     (= w (win32-device/width window))
	     (= h (win32-device/height window)))
	(let ((bm (bitmap-from-dib dib (win32-device/palette window))))
	  ;(display ";Special full window image/draw\n")
	  (set-win32-device/bitmap! window bm)
	  (delete-object (select-object hdc bm)))
;;	(dib-blt hdc x y w h dib 0 0 SRCCOPY))
	(dib-blt hdc (win32-xform-x window x) (win32-xform-y window y) w h
		 dib 0 0 SRCCOPY))
    (win32-device/invalidate! window)))

(define (dib-image/draw-subimage device x y image im-x im-y w h)
  (let*  ((dib    (image/descriptor image))
	  (window (graphics-device/descriptor device))
	  (hdc    (win32-device/hdc window)))
    (dib-blt hdc (win32-xform-x window x) (win32-xform-y window y) w h
	     dib im-x im-y SRCCOPY)
    (win32-device/invalidate! window)))

(define (dib-image/fill-from-byte-vector image byte-vector)
  (dib-set-pixels-unaligned (image/descriptor image) byte-vector))

(define (win32-graphics/create-image device width height)
  (image/create device width height))

;; Should the following be in the runtime?
;; (or we can just pass image/draw to make-graphics-device-type
;(define (win32-graphics/draw-image device x y image)
;  (image/draw device x y image))

;(define (win32-graphics/draw-subimage device x y image im-x im-y w h)
;  (image/draw-subimage device x y image im-x im-y w h))

;;-----------------------------------------------------------------------------
;;
(define (bitmap-namestring filename)
  (->namestring (merge-pathnames filename (pathname-new-type filename "bmp"))))

(define (win32-graphics/save-bitmap device filename)
  (let* ((true-filename (bitmap-namestring filename))
	 (window        (graphics-device/descriptor device))
	 (ddb           (win32-device/bitmap window))
	 (palette       (win32-device/palette window))
	 (dib           (dib-from-bitmap ddb BI_RGB 0 palette))
	 (success?      (write-dib true-filename dib)))
    (if dib
	(delete-dib dib))
    (if (not success?)
	(graphics-error	 "Cannot save bitmap to" true-filename))
    unspecific))

(define (win32-graphics/load-bitmap device filename)
  (let* ((true-filename (bitmap-namestring filename))
	 (dib           (open-dib true-filename)))
    (if dib
	(let* ((width   (dib-width dib))
	       (height  (dib-height dib)))
	  (graphics-operation device 'resize-window width height)
	  (let* ((window  (graphics-device/descriptor device))
		 (hdc     (win32-device/hdc window)))
	    (dib-blt hdc 0 0 width height dib 0 0 SRCCOPY)
	    (win32-device/invalidate! window)
	    (delete-dib dib)
	    unspecific))
	(graphics-error "Cannot load bitmap from" true-filename))))

;;----------------------------------------------------------------------------
;;
;;

(define (register-graphics-window-class)
  (let* ((hInstance (get-handle 0))
         (hIcon     (load-icon hInstance "GRAPHICS_ICON")))
    (register-class (+) (get-handle 3) 0 0 hInstance
                    hIcon 32515 NULL_BRUSH 0 "SCHEME-GRAPHICS")))

(define (initialize-package!)
  (set! win32-graphics-device-type
	(make-graphics-device-type
	 'WIN32
	 `((available? ,win32-graphics/available?)
	   (open ,win32-graphics/open)
	   (open? ,win32-graphics/open?)
	   (clear ,win32-graphics/clear)
	   (close ,win32-graphics/close)
	   (coordinate-limits ,win32-graphics/coordinate-limits)
           (copy-area ,win32-graphics/copy-area)
	   (create-image ,win32-graphics/create-image)
	   (define-color ,win32-graphics/define-color)
	   (device-coordinate-limits ,win32-graphics/device-coordinate-limits)
	   (drag-cursor ,win32-graphics/drag-cursor)
	   (draw-image ,image/draw)
	   (draw-subimage ,image/draw-subimage)
	   (draw-line ,win32-graphics/draw-line)
	   (draw-point ,win32-graphics/draw-point)
	   (draw-text ,win32-graphics/draw-text)
	   (draw-ellipse ,win32-graphics/draw-ellipse)
	   (fill-polygon ,win32-graphics/fill-polygon)
	   (find-color ,win32-graphics/find-color)
	   (flush ,win32-graphics/flush)
	   (image-depth ,win32-graphics/image-depth)
	   (load-bitmap ,win32-graphics/load-bitmap)
	   (save-bitmap ,win32-graphics/save-bitmap)
	   (move-cursor ,win32-graphics/move-cursor)
	   (move-window ,win32-graphics/move-window)
	   (reset-clip-rectangle ,win32-graphics/reset-clip-rectangle)
	   (resize-window ,win32-graphics/resize-window)
	   (set-clip-rectangle ,win32-graphics/set-clip-rectangle)
	   (set-coordinate-limits ,win32-graphics/set-coordinate-limits)
	   (set-drawing-mode ,win32-graphics/set-drawing-mode)
	   (set-font ,win32-graphics/set-font)
	   (set-line-style ,win32-graphics/set-line-style)
	   (set-line-width ,win32-graphics/set-line-width)
	   (set-foreground-color ,win32-graphics/set-foreground-color)
	   (set-background-color ,win32-graphics/set-background-color)
	   (set-window-name ,win32-graphics/set-window-name))))

  (set! dib-image-type
	(make-image-type
	 `((create ,dib-image/create)
	   (destroy ,dib-image/destroy)
	   (width   ,dib-image/width)
	   (height  ,dib-image/height)
	   (draw    ,dib-image/draw)
	   (draw-subimage   ,dib-image/draw-subimage)
	   (fill-from-byte-vector ,dib-image/fill-from-byte-vector))))
  (1d-table/put! (graphics-type-properties win32-graphics-device-type)
		 'IMAGE-TYPE
		 dib-image-type)
  (set! color-table '())
  (for-each
    (lambda (pair) (win32/define-color (car pair) (cdr pair)))
    initial-color-definitions)
  (register-graphics-window-class)
  (add-event-receiver! event:after-restore
    (when-microcode-supports-win32 register-graphics-window-class))
  (set! device-protection-list (make-protection-list close-descriptor))
  (add-event-receiver! event:after-restore
    (lambda ()
      (set! device-protection-list (make-protection-list close-descriptor))
      unspecific))
  (add-event-receiver! event:before-exit
    (lambda ()
      (protection-list/for-each-info close-descriptor device-protection-list)))
  unspecific)

(define win32-graphics-device-type)