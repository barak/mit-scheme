#| -*-Scheme-*-

$Id: dib.scm,v 1.2 1993/11/10 21:41:48 adams Exp $

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Device-independent bitmaps (dibutils.dll)
;;; package: (win32 dib)

(define-structure
  (dib
   (constructor %make-dib))
  handle
)

;; DIBS are handles into non-scheme memory.  They are kept on a protection list
;; so that the memory can be freed if there is no longer a handle to the DIB.
;; Because DIBS can be huge, we also support explicit deallocation via DELETE-DIB.
;; The protection list descriptor is a CELL containing the handle.  It is shared with
;; the DIB structure so that explicit deallocation can signal that the dib is void.

(define dib-protection-list)

(define (make-dib handle)
  (let* ((cell (make-cell handle))
	 (dib  (%make-dib cell)))
    (add-to-protection-list! dib-protection-list dib cell)
    dib))


(define dib-result
  (lambda (handle)
    (if (= handle 0)
	#f
	(make-dib handle))))

(define dib-arg
  (lambda (dib)
    (if dib
	(cell-contents (dib-handle dib))
	0)))  

(define-windows-type dib
  (lambda (thing) (or (dib? thing) (eq? thing #f)))
  dib-arg
  dib-result)


(define (delete-dib dib)
  (let ((handle (cell-contents (dib-handle dib))))
    (set-cell-contents! (dib-handle dib) 0)
    (%delete-dib handle)))

(define (destroy-lost-dibs)
  (clean-lost-protected-objects
    dib-protection-list
    (lambda (cell) (%delete-dib (cell-contents cell)))))


(define dibutils.dll)
(define open-dib)
(define write-dib)
(define copy-bitmap)
(define create-dib)
(define crop-bitmap)
(define bitmap-from-dib)
(define dib-from-bitmap)
(define dib-blt)
(define %delete-dib)
(define dib-width)
(define dib-height)
(define dib-set-pixels-unaligned)

(define (initialize-package!)

  (set! dibutils.dll (find-module "DIBUTILS.DLL"))

  (set!  open-dib
    (windows-procedure (open-dib (filename string))
       dib dibutils.dll "OpenDIB"))

  (set!  write-dib
    (windows-procedure (write-dib (filename string) (dib dib))
		       bool dibutils.dll "WriteDIB"))


  (set!  bitmap-from-dib
    (windows-procedure (bitmap-from-dib (dib dib) (palette hpalette))
		       hbitmap dibutils.dll "BitmapFromDib"))

  (set!  dib-from-bitmap
    (windows-procedure
     (dib-from-bitmap (bitmap hbitmap) (style dword) (bits word) (palette hpalette))
     dib  dibutils.dll "DibFromBitmap"))

  (set!  dib-blt
    (windows-procedure
     (dib-blt (dest hdc) (x int) (y int) (w int) (height int) 
	      (src dib) (src-x int) (src-y int) (raster-op long))
     bool dibutils.dll "DibBlt"))

  (set!  %delete-dib
    (windows-procedure
      (%delete-dib (dib-handle handle)) bool dibutils.dll "DeleteDIB"))
    ;; int-arg is the handle, NOT dib-arg for a DIB record.

  (set!  dib-height
    (windows-procedure (dib-height (dib dib)) int dibutils.dll "DibHeight" expand))

  (set!  dib-width
    (windows-procedure (dib-width (dib dib)) int dibutils.dll "DibWidth" expand))

  (set!  copy-bitmap
    (windows-procedure (copy-bitmap (bm hbitmap))
      hbitmap dibutils.dll "CopyBitmap"))

  (set!  create-dib
    (windows-procedure
	(create-dib (width int) (height int)
		    (style int) (depth int) (palette hpalette))
      dib dibutils.dll "CreateDIB"))

  (set!  crop-bitmap
    (windows-procedure
	(crop-bitmap (bm hbitmap) (left int) (top int) (right int) (bottom int))
      hbitmap dibutils.dll "CropBitmap"))

  (set!  dib-set-pixels-unaligned
    (windows-procedure
	(dib-set-pixels-unaligned (dib dib) (pixels string))
      bool dibutils.dll "DIBSetPixelsUnaligned"))

  (set! dib-protection-list (make-protection-list))
  (add-gc-daemon! destroy-lost-dibs)
)





