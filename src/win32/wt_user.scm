#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

(declare (usual-integrations))

;;
;; common win32 types

(declare (usual-integrations))


(define-integrable int32-offset-ref   
          (ucode-primitive int32-offset-ref 2))
(define-integrable int32-offset-set!  
          (ucode-primitive int32-offset-set! 3))
(define-integrable uint32-offset-ref  
          (ucode-primitive uint32-offset-ref 2))
(define-integrable uint32-offset-set! 
          (ucode-primitive uint32-offset-set! 3))

(define-integrable byte-offset-ref  vector-8b-ref)	    
(define-integrable byte-offset-set! vector-8b-set!)	    

(define-integrable (loword dword)  (modulo dword 65536))
(define-integrable (hiword dword)  (integer-floor dword 65536))

(define-integrable (int->bool i) (not (= i 0)))
(define-integrable (bool->int b) (if b 1 0))

;;typedef struct tagRECT {    /* rc */
;;    LONG left;
;;    LONG top;
;;    LONG right;
;;    LONG bottom;
;;} RECT;

(define-structure (rect (conc-name rect/)
                        (constructor %make-rect))
  mem)

(define-integrable (rect/left r)   (int32-offset-ref (rect/mem r) 0))
(define-integrable (rect/top r)    (int32-offset-ref (rect/mem r) 4))
(define-integrable (rect/right r)  (int32-offset-ref (rect/mem r) 8))
(define-integrable (rect/bottom r) (int32-offset-ref (rect/mem r) 12))

(define-integrable (set-rect/left! r v)   (int32-offset-set! (rect/mem r) 0 v))
(define-integrable (set-rect/top! r v)    (int32-offset-set! (rect/mem r) 4 v))
(define-integrable (set-rect/right! r v)  (int32-offset-set! (rect/mem r) 8 v))
(define-integrable (set-rect/bottom! r v) (int32-offset-set! (rect/mem r) 12 v))

(define (make-rect left top right bottom)
  (define r (%make-rect (make-string 16)))
  (set-rect/left!   r left)
  (set-rect/top!    r top)
  (set-rect/right!  r right)
  (set-rect/bottom! r bottom)
  r)

(define-windows-type RECT
  (lambda (thing) (or (eq? thing #f) (rect? thing)))
  (lambda (thing) (and thing (rect/mem thing)))
  #f
  #f)

;;typedef struct tagPAINTSTRUCT { /* ps */
;;    HDC  hdc;
;;    BOOL fErase;
;;    RECT rcPaint;
;;    BOOL fRestore;
;;    BOOL fIncUpdate;
;;    BYTE rgbReserved[32];
;;} PAINTSTRUCT;


(define-structure (paintstruct (conc-name paintstruct/)
                        (constructor %make-paintstruct))
  mem)

(define-integrable (paintstruct/hdc r)
  (uint32-offset-ref (paintstruct/mem r) 0))
(define-integrable (paintstruct/f-erase r)
  (int->bool (int32-offset-ref (paintstruct/mem r) 4)))
(define-integrable (paintstruct/rc-paint/left r)
  (int32-offset-ref (paintstruct/mem r) 8))
(define-integrable (paintstruct/rc-paint/top r)
  (int32-offset-ref (paintstruct/mem r) 12))
(define-integrable (paintstruct/rc-paint/right r)
  (int32-offset-ref (paintstruct/mem r) 16))
(define-integrable (paintstruct/rc-paint/bottom r)
  (int32-offset-ref (paintstruct/mem r) 20))
(define-integrable (paintstruct/f-restore r)
  (int->bool (byte-offset-ref (paintstruct/mem r) 24)))
(define-integrable (paintstruct/f-inc-update r)
  (int->bool (byte-offset-ref (paintstruct/mem r) 28)))

(define-integrable (set-paintstruct/hdc! r v)
  (uint32-offset-set! (paintstruct/mem r) 0 v))
(define-integrable (set-paintstruct/f-erase! r v)
  (byte-offset-set! (paintstruct/mem r) 4 (bool->int v)))
(define-integrable (set-paintstruct/rc-paint/left! r v)
  (int32-offset-set! (paintstruct/mem r) 8 v))
(define-integrable (set-paintstruct/rc-paint/top! r v)
  (int32-offset-set! (paintstruct/mem r) 12 v))
(define-integrable (set-paintstruct/rc-paint/right! r v)
  (int32-offset-set! (paintstruct/mem r) 16 v))
(define-integrable (set-paintstruct/rc-paint/bottom! r v)
  (int32-offset-set! (paintstruct/mem r) 20 v))
(define-integrable (set-paintstruct/f-restore! r v)
  (byte-offset-set! (paintstruct/mem r) 24 (bool->int v)))
(define-integrable (set-paintstruct/f-inc-update! r v)
  (byte-offset-set! (paintstruct/mem r) 28 (bool->int v)))

(define (make-paintstruct)
  (define r (%make-paintstruct (make-string 64)))
  r)

(define-windows-type paintstruct
  paintstruct?
  paintstruct/mem
  #f
  #f)

(define (pp-paintstruct r)
  (define (pp-field name accessor)
    (display "(")
    (display name)
    (display " ")
    (display (accessor r))
    (display ")")
    (newline))
  (pp r)
  (pp-field 'hdc paintstruct/hdc)
  (pp-field 'f-erase paintstruct/f-erase)
  (pp-field 'rc-paint/left paintstruct/rc-paint/left)
  (pp-field 'rc-paint/top paintstruct/rc-paint/top)
  (pp-field 'rc-paint/right paintstruct/rc-paint/right)
  (pp-field 'rc-paint/bottom paintstruct/rc-paint/bottom)
  (pp-field 'f-restore paintstruct/f-restore)
  (pp-field 'f-inc-update paintstruct/f-inc-update)
)

