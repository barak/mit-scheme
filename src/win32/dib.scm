#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Device-independent bitmaps (dibutils.dll)
;;; package: (win32 dib)

(declare (usual-integrations))

(define-structure (dib (constructor %make-dib))
  handle)

;; DIBS are handles into non-scheme memory.  They are kept on a GC
;; finalizer so that the memory can be freed if there is no longer a
;; handle to the DIB.  Because DIBS can be huge, we also support
;; explicit deallocation via DELETE-DIB.

(define dib-finalizer)

(define (make-dib handle)
  (add-to-gc-finalizer! dib-finalizer (%make-dib handle)))

(define (dib-result handle)
  (if (= handle 0)
      #f
      (make-dib handle)))

(define (dib-arg dib)
  (if dib
      (dib-handle dib)
      0))

(define-windows-type dib
  (lambda (thing) (or (dib? thing) (eq? thing #f)))
  dib-arg
  dib-result)

(define (delete-dib dib)
  (remove-from-gc-finalizer! dib-finalizer dib))

(define dibutils.dll)
(define open-dib)
(define write-dib)
(define copy-bitmap)
(define create-dib)
(define crop-bitmap)
(define bitmap-from-dib)
(define dib-from-bitmap)
(define dib-blt)
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
	(make-gc-finalizer (windows-procedure (%delete-dib (dib-handle handle))
					      bool dibutils.dll "DeleteDIB")
			   dib?
			   dib-handle
			   set-dib-handle!))
  unspecific)