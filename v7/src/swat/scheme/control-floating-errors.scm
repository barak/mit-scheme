;;; -*-Scheme-*-

#|
	(set-floating-error-mask! <fixnum>)

	sets the floating-point enables to the bottom 5 bits of fixnum.
	returns a fixnum with the old floating-point enables in the bottom 5 bits.

	Warning: This does not check the argument type.

	Flags: 	V	valid operation		16
		Z	zero divide		 8
		O	overflow		 4
		U	underflow		 2
		I	inexact			 1

	This version is long because it compiles under both 7.4 and 8.0
|#

(declare (usual-integrations))

(define-syntax deflap
  (lambda (name . lap)
    `(define ,name
       (scode-eval
	',((access lap->code (->environment '(compiler top-level)))
	   name
	   lap)
	system-global-environment))))

(define set-floating-error-mask!
  (let ()
    (deflap set-floating-error-mask/8.0!
      (entry-point set-floating-error-mask/8.0!)
      (scheme-object CONSTANT-0 #F)
      (scheme-object CONSTANT-1 0)
      (external-label () #x202 (@pcr set-floating-error-mask/8.0!))

      (LABEL set-floating-error-mask/8.0!)
					; arg = 2, cont = 19
      (fstws () 0 (offset 0 0 21))	; flags to free
      (ldw () (offset 0 0 21) 6)	; flags to reg 6
      (copy () 6 7)			; copy flags to 7
      (dep () 2 31 5 7)			; arg merged with flags in 7
      (stw () 7 (offset 0 0 21))	; new flags to free
      (dep () 6 31 5 2)			; flags merged with arg in 2
      (fldws () (offset 0 0 21) 0)	; store flags
      (bv (n) 0 19)			; return
      )

    (deflap set-floating-error-mask/7.4!
      (entry-point set-floating-error-mask/7.4!)
      (scheme-object CONSTANT-0 #F)
      (scheme-object CONSTANT-1 0)
      (external-label () #x202 (@pcr set-floating-error-mask/7.4!))

      (LABEL set-floating-error-mask/7.4!)

      (fstws () 0 (offset 0 0 21))	; flags to free
      (ldw () (offset 0 0 #x16) 2)	; arg to reg 2
      (ldw () (offset 0 0 21) 6)	; flags to reg 6
      (copy () 6 7)			; copy flags to 7
      (dep () 2 31 5 7)			; arg merged with flags in 7
      (stw () 7 (offset 0 0 21))	; new flags to free
      (dep () 6 31 5 2)			; flags merged with arg in 2
      (fldws () (offset 0 0 21) 0)	; store flags
      (ldo () (offset 4 0 #x16) #x16)	; pop arg
      (ldwm () (offset 4 0 #x16) 6)	; pop ret add
      (dep () 5 5 6 6)			; remove tag
      (bv (n) 0 6)			; return
      )

    (if (object-type? 0 0)		; untagged fixnums?
	set-floating-error-mask/8.0!
	set-floating-error-mask/7.4!)))