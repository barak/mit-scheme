#| -*-Scheme-*-

$Id: wt_user.scm,v 1.3 1993/12/01 03:08:03 adams Exp $

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
    (newline)(display "(") (display name) (display " ") (display (accessor r)) (display ")") )
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

