;;; -*- Scheme -*-

(DECLARE (USUAL-INTEGRATIONS))	; MIT Scheme-ism: promise not to redefine prims

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;  TEST-WABBIT -- Harey test of wabbit hunting / headhunting g.c.	     ;;
;;									     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|
 |									      |
 | Uses:								      |
 |	tons o' stuff not yet documented as dependencies		      |
 |									      |
 |#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|

;; TODO:
;;
;;	- Document dependencies
;;	- [SCREWS] see last page

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;  TEST-WABBIT -- Harey test of wabbit hunting / headhunting g.c.	     ;;
;;									     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *muobj-wabbit-vector* '--TBA--)

(define (muobj-wabbit-vector/install!)

  (define muobj-pair           (cons            make-unique-object
				               (make-unique-object)))
  (define muobj-vector         (vector         'make 'nique 'bject
				                make-unique-object))
#|
  (define muobj-promise        (delay           make-unique-object))
|#

  (define-structure (muos (conc-name muos/)
			  (constructor make-muos ()))
    (  uobj-slot (make-unique-object))
    ( cuobj-slot (make-unique-object) read-only #T)
    ( muobj-slot  make-unique-object)
    (cmuobj-slot  make-unique-object  read-only #T))

  (define muobj-struct1 (make-muos))
  (define muobj-struct2 (make-muos))

  (define muobj-cell           (make-cell       make-unique-object))
  (define muobj-weak-pair      (weak-cons      (make-unique-object)
					        make-unique-object ))

  (define muobj-weak-car       (weak-car muobj-weak-pair)) ; made UObj
  (define muobj-weak-cdr       (weak-cdr muobj-weak-pair)) ; make-UObj

  (define muobj-apply-hook     (make-apply-hook muobj-weak-car
						make-unique-object))
  (define muobj-entity         (make-entity     muobj-weak-car
						make-unique-object))
  (define muobj-forced-promise (let ((p  (delay make-unique-object)))
				 (force p)
				 p))

  (define muobj-wabbit-vector
    `#(
       ,muobj-weak-car			; Made UObj
       ,muobj-weak-cdr			; Make-UObj

       ,muobj-pair
       ,muobj-vector
#|
       ,muobj-promise
|#
       ;;
       ;; (define-structure (muos (conc-name muos/)
       ;;                        (constructor make-muos ()))
       ;;  (muobj-slot  (make-unique-object))
       ;;  (cmuobj-slot (make-unique-object) read-only true)
       ;;  (muos-slot   make-unique-object)
       ;;  (cmuso-slot  make-unique-object  read-only true))
       ;;
       ,muobj-struct1
       ,muobj-struct2

       ,muobj-cell
       ,muobj-weak-pair
       ,muobj-forced-promise
       ,muobj-apply-hook
       ,muobj-entity
       ))

  (set! *muobj-wabbit-vector* muobj-wabbit-vector)

  (pp (cons 42 make-unique-object))	; Random un-named pair for pp hashing

  'DONE)

(define (forced-promise? x) (and (promise? x) (promise-forced? x)))

(define (muobj-wabbit-hunt)
  (wabbit-hunt
   (make-wabbit-descwiptor false	         ; hunt    disable flag disabled
			   *muobj-wabbit-vector* ; targets of the hunt
			   (make-vector 100 #f)   ; wabbit buffer
			   false	         ; headhunt enable flag disabled
			   )
   (named-lambda (exposing-fudd-thunk)
     (let* ((wabbuf (wabbit-descwiptor/wabbit-buffer (get-wabbit-descwiptor)))
	    (got-em-all?	   (vector-ref wabbuf 0))
	    (last-hole-index (vector-ref wabbuf 1)))
       (display "\n; #(")
       (do ((index 2 (1+ index)))
	   ((>= index last-hole-index)
	    (if got-em-all?
		(display ")\n; Th-th-th-that's all folks!")
		(display ")\n; And many more.... maybe?!?"))
	    (newline))

	 (let ((next-elt (vector-ref wabbuf index)))
	   (if (odd? index)
	       (write next-elt)		; write index of non-skipped elt
	       (let ()
		 (define (space-write-and-skip! object)
		   (space-out!) (write object) (skip!))
		 (define (space-in-write!       object)
		   (space-in!)  (write object)        )
		 (define (space-out!)
		   (write-char #\Space) (write-char #\=) (write-char #\Space))
		 (define (space-in!)
		   (write-char #\Space) (write-char #\-) (write-char #\Space))
		 (define (skip!)	 (set! index (1+ index)))
		 (define (offset) (vector-ref wabbuf (1+ index)))
      
		 (write-char #\[) (write index) (write-char #\])
		 (write-char #\Space)
		 (write (microcode-type-name next-elt))

		 (cond ((pair? next-elt)
			(space-write-and-skip! (if (zero? (offset))
						   (car next-elt)
						   (cdr next-elt))))
		       ((vector? next-elt)
			(space-write-and-skip! (vector-ref next-elt 
							   (-1+ (offset)))))
		       ((record? next-elt)
			(space-write-and-skip! (%record-ref next-elt
							    (-1+ (offset)))))
		       ;;
		       ;; MIT Scheme specific extensions...
		       ;;
		       ((cell? next-elt)
			(space-write-and-skip! (cell-contents next-elt)))
		       ((weak-pair? next-elt)
			(space-write-and-skip! (if (zero? (offset))
						   (weak-car next-elt)
						   (weak-cdr next-elt))))
		       ((forced-promise? next-elt)
			(space-write-and-skip! (force next-elt)))
		       ((promise? next-elt) ; Must follow forced-promise
			(space-write-and-skip!        next-elt ))
		       ((%entity-extra/apply-hook? next-elt)
			(space-write-and-skip! (case (offset)
						 ((0) (system-hunk3-cxr0 next-elt))
						 ((1) (system-hunk3-cxr1 next-elt))
						 ((2) (system-hunk3-cxr2 next-elt)))))
		       ((apply-hook?  next-elt)	; SIGH: hunk3/triple hack uproc
			(space-write-and-skip! (if (zero? (offset))
						   (apply-hook-procedure next-elt)
						   (apply-hook-extra     next-elt))))
		       ((entity?      next-elt)
			(space-write-and-skip! (if (zero? (offset))
						   (entity-procedure next-elt)
						   (entity-extra     next-elt))))
		       ((environment? next-elt)
			(space-write-and-skip! (system-vector-ref next-elt
								  (-1+ (offset)))))
		       ((and (compiled-code-block?                  next-elt)
			     (compiled-code-block/manifest-closure? next-elt))
			(space-write-and-skip! (system-vector-ref next-elt
								  (-1+ (offset)))))
		       ;;
		       ;; Normal compiled code blocks are unsafe since may ref
		       ;;     into the R/W/X cache of the linkage section.
		       (else		
			(space-in-write! next-elt))))))
	 ;(display "\n; #(")	; From above
	 (display  "\n;   ")))))
  )

(define (test-wabbit-go-for-it)
  (muobj-wabbit-vector/install!)  
  (muobj-wabbit-hunt)
  )

#| Until somebody builds the newest Scheme band...

(define   %entity-extra/apply-hook?
  (access %entity-extra/apply-hook? (->environment '(runtime procedure))))
|#

(let-syntax
    ((ucode-type
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (microcode-type (cadr form))))))

  (define   apply-hook-tag 
    (access apply-hook-tag (->environment '(runtime procedure))))

  (define (%entity-extra/apply-hook? extra)
    ;; Ziggy cares about this one.
    (and (object-type? (ucode-type hunk3) extra)
	 (eq? apply-hook-tag (system-hunk3-cxr0 extra))))
  )



;;; fini

(provide "Test Wabbit")

;;; Complete dependencies  (desire = run-time require (not load-time require))

(begin

  (with-working-directory-pathname "Utils/"
    (named-lambda (acknowledge-Utils-desiderata)
      (desire "Unique Objects" "unique-objects")
      ))

  (load-option 'wabbit   )
  (load-option 'pc-sample)

  (with-working-directory-pathname "../ObjectType/"
    (named-lambda (acknowledge-ObjType-desiderata)
      (desire "Object Structural Types" "objtype")
      ))
  )

#| Example run...

;; First time...
(test-wabbit-go-for-it)

;; Thereafter...
(muobj-wabbit-hunt)

(42 . #[compiled-closure 31 ("unique-objects") #xD0 #x7B6D24 #x79276C])

|#

; #([2] pair = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [4] vector = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [6] record = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [8] record = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [10] record = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [12] record = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [14] cell = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [16] weak-cons = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [18] promise = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [20] entity = #[compiled-closure 32 ("unique-objects") #x1A8 #x145801C #x1457C44]
;   [22] entity = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [24] compiled-code-block = #[compiled-closure 32 ("unique-objects") #x1A8 #x145801C #x1457C44]
;   [26] triple = #[compiled-closure 32 ("unique-objects") #x1A8 #x145801C #x1457C44]
;   [28] triple = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [30] compiled-code-block - #[compiled-code-block 33]
;   602
;   [32] quad - #[quad 34]
;   0
;   [34] weak-cons = #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
;   [36] weak-cons = #[compiled-closure 32 ("unique-objects") #x1A8 #x145801C #x1457C44]
;   )
; Th-th-th-that's all folks!
;No value


(begin
  (load "/scheme/700/compiler/etc/disload")
  (load-disassembler))

#|
(compiler:disassemble #@33)

Disassembly of #[compiled-code-block 33] (Block 2 in /sw/ziggy/Projects/Descartes/Wabbit/test-wabbit.inf):
Code:

14DFD24	8	(ble () (offset 0 4 3))
14DFD28	C	(ldi () #x1A #x1C)
14DFD2C	10	(external-label () #x101 (@pco #x14))
14DFD30	14	(combf (<) #x15 #x14 (@pco #x-14))
.
.
.

Constants:

14E0608	8EC	#[LINKAGE-SECTION #x21]
14E060C	8F0	2 argument procedure cache to #[compiled-entry 35 () #xC #x1501DF0]
14E0618	8FC	2 argument procedure cache to #[compiled-entry 36 () #xC #x1501E10]
14E0624	908	3 argument procedure cache to #[compiled-procedure 37 ("uproc" #x1D) #x14 #x392160]
14E0630	914	3 argument procedure cache to #[compiled-procedure 38 ("uproc" #x24) #x14 #x3923D0]
14E063C	920	2 argument procedure cache to #[compiled-procedure 39 ("list" #x14) #x14 #x394808]
14E0648	92C	3 argument procedure cache to #[compiled-procedure 40 ("list" #xF) #x14 #x3945B0]
14E0654	938	2 argument procedure cache to #[compiled-procedure 41 ("list" #x12) #x14 #x3946E8]
14E0660	944	2 argument procedure cache to #[compiled-entry 42 () #xC #x1501E28]
14E066C	950	3 argument procedure cache to #[compiled-entry 43 () #xC #x1501E40]
14E0678	95C	5 argument procedure cache to #[compiled-entry 44 () #xC #x1501E58]
14E0684	968	1 argument procedure cache to #[compiled-closure 31 ("unique-objects") #xD0 #x1457F44 #x1457C5C]
14E0690	974	#[LINKAGE-SECTION #x10001]
14E0694	978	Reference cache to make-unique-object
14E0698	97C	#[LINKAGE-SECTION #x20001]
14E069C	980	Assignment cache to *muobj-wabbit-vector*
14E06A0	984	#[LINKAGE-SECTION #x30003]
14E06A4	988	3 argument procedure cache to #[compiled-entry 45 () #xC #x1501E78]
14E06B0	994	done
14E06B4	998	"muos"
14E06B8	99C	(uobj-slot cuobj-slot muobj-slot cmuobj-slot)
14E06BC	9A0	make
14E06C0	9A4	nique
14E06C4	9A8	bject
14E06C8	9AC	(#[dbg-info 46] "/sw/ziggy/Projects/Descartes/Wabbit/wabbit-
14E06CC	9B0	#[environment 47]

;No value
|#

;;
;; [SCREWS]: Environments (system-vector-ref (-1+ index))
;;	     Compiled code blocks -- appear in linkage section. Indir thru env.
;;	     Quads - what a ref trap points to in a linkage section.
;;		   ...don't sweat it... will lexical-assign w/in env.
;;	     Quotations [scode.scm --- %singleton-set-car!]
;;
