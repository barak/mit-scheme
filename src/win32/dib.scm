#| -*-Scheme-*-

$Id: dib.scm,v 1.5 2001/12/23 17:21:00 cph Exp $

Copyright (c) 1993, 1999-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Device-independent bitmaps (dibutils.dll)
;;; package: (win32 dib)

(declare (usual-integrations))

(define-structure (dib (constructor %make-dib))
  handle)

;; DIBS are handles into non-scheme memory.  They are kept on a GC
;; finalizer so that the memory can be freed if there is no longer a
;; handle to the DIB.  Because DIBS can be huge, we also support
;; explicit deallocation via DELETE-DIB.  The GC finalizer descriptor
;; is a CELL containing the handle.  It is shared with the DIB
;; structure so that explicit deallocation can signal that the dib is
;; void.

(define dib-finalizer)

(define (make-dib handle)
  (let* ((cell (make-cell handle))
	 (dib (%make-dib cell)))
    (add-to-gc-finalizer! dib-finalizer dib cell)
    dib))

(define (dib-result handle)
  (if (= handle 0)
      #f
      (make-dib handle)))

(define (dib-arg dib)
  (if dib
      (cell-contents (dib-handle dib))
      0))

(define-windows-type dib
  (lambda (thing) (or (dib? thing) (eq? thing #f)))
  dib-arg
  dib-result)

(define (delete-dib dib)
  (let ((handle (cell-contents (dib-handle dib))))
    (set-cell-contents! (dib-handle dib) 0)
    (%delete-dib handle)))

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
  (set! dibutils.dll
	(find-module "DIBUTILS.DLL"))
  (set! open-dib
	(windows-procedure (open-dib (filename string))
			   dib dibutils.dll "OpenDIB"))
  (set! write-dib
	(windows-procedure (write-dib (filename string) (dib dib))
			   bool dibutils.dll "WriteDIB"))
  (set! bitmap-from-dib
	(windows-procedure (bitmap-from-dib (dib dib) (palette hpalette))
			   hbitmap dibutils.dll "BitmapFromDib"))
  (set! dib-from-bitmap
	(windows-procedure
	 (dib-from-bitmap (bitmap hbitmap) (style dword) (bits word)
			  (palette hpalette))
	 dib  dibutils.dll "DibFromBitmap"))
  (set! dib-blt
	(windows-procedure
	 (dib-blt (dest hdc) (x int) (y int) (w int) (height int) 
		  (src dib) (src-x int) (src-y int) (raster-op long))
	 bool dibutils.dll "DibBlt"))
  (set! %delete-dib
	(windows-procedure
	 (%delete-dib (dib-handle handle)) bool dibutils.dll "DeleteDIB"))
  ;; int-arg is the handle, NOT dib-arg for a DIB record.
  (set! dib-height
	(windows-procedure (dib-height (dib dib)) int dibutils.dll "DibHeight"
			   expand))
  (set! dib-width
	(windows-procedure (dib-width (dib dib)) int dibutils.dll "DibWidth"
			   expand))
  (set! copy-bitmap
	(windows-procedure (copy-bitmap (bm hbitmap))
			   hbitmap dibutils.dll "CopyBitmap"))
  (set! create-dib
	(windows-procedure
	 (create-dib (width int) (height int)
		     (style int) (depth int) (palette hpalette))
	 dib dibutils.dll "CreateDIB"))
  (set! crop-bitmap
	(windows-procedure
	 (crop-bitmap (bm hbitmap)
		      (left int) (top int) (right int) (bottom int))
	 hbitmap dibutils.dll "CropBitmap"))
  (set! dib-set-pixels-unaligned
	(windows-procedure
	 (dib-set-pixels-unaligned (dib dib) (pixels string))
	 bool dibutils.dll "DIBSetPixelsUnaligned"))
  (set! dib-finalizer
	(make-gc-finalizer (lambda (cell) (%delete-dib (cell-contents cell)))))
  unspecific)