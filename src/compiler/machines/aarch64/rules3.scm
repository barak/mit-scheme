#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Invocations and Entries
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Invocations

(define-rule statement
  (POP-RETURN)
  (let* ((checks (get-exit-interrupt-checks))
         (prefix (clear-map!))
         (suffix
          (if (pair? checks)
              (pop-return/interrupt-check)
              (pop-return))))
    (LAP ,@prefix
         ,@suffix)))

(define (pop-return)
  (LAP ,@(pop rlr)
       ,@(object->address rlr rlr)
       (RET)))

(define (pop-return/interrupt-check)
  (share-instruction-sequence! 'POP-RETURN
    (lambda (shared-label) (LAP (B (@PCR ,shared-label ,regnum:scratch-0))))
    (lambda (shared-label)
      (let ((interrupt-label (generate-label 'INTERRUPT)))
        (LAP (LABEL ,shared-label)
             ,@(interrupt-check '(HEAP) interrupt-label)
             ,@(pop-return)
             (LABEL ,interrupt-label)
             ,@(invoke-interface code:compiler-interrupt-continuation-2))))))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation
  (let* ((prefix (clear-map!))
         (setup (apply-setup frame-size)))
    (LAP ,@prefix
         ,@(pop regnum:applicand)
         ,@setup
         (BR ,regnum:applicand-pc))))

(define (apply-setup frame-size)
  (case frame-size
    ((1) (invoke-hook/subroutine entry:compiler-apply-setup-size-1))
    ((2) (invoke-hook/subroutine entry:compiler-apply-setup-size-2))
    ((3) (invoke-hook/subroutine entry:compiler-apply-setup-size-3))
    ((4) (invoke-hook/subroutine entry:compiler-apply-setup-size-4))
    ((5) (invoke-hook/subroutine entry:compiler-apply-setup-size-5))
    ((6) (invoke-hook/subroutine entry:compiler-apply-setup-size-6))
    ((7) (invoke-hook/subroutine entry:compiler-apply-setup-size-7))
    ((8) (invoke-hook/subroutine entry:compiler-apply-setup-size-8))
    (else
     (LAP ,@(load-unsigned-immediate regnum:utility-arg1 frame-size)
          ,@(invoke-hook/subroutine entry:compiler-apply-setup)))))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       (B (@PCR ,label ,regnum:scratch-0))))

(define (entry->pc pc entry)
  ;; XXX Would be nice to skip the SUB, but LDR doesn't have a signed
  ;; offset without pre/post-increment.
  (LAP (SUB X ,pc ,entry (&U 8))
       (LDR X ,pc ,pc)
       (ADD X ,pc ,pc ,entry)))

(define-rule statement
  (INVOCATION:COMPUTED-JUMP (? frame-size) (? continuation))
  frame-size continuation
  (expect-no-exit-interrupt-checks)
  ;; Tagged entry is on top of stack.
  (LAP ,@(clear-map!)
       ,@(pop regnum:applicand)
       ,@(object->address regnum:applicand regnum:applicand)
       ,@(entry->pc regnum:applicand-pc regnum:applicand)
       (BR ,regnum:applicand-pc)))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation
  (LAP ,@(clear-map!)
       ,@(load-pc-relative-address regnum:utility-arg1 label)
       ,@(load-unsigned-immediate regnum:utility-arg2 number-pushed)
       ,@(invoke-interface/shared 'COMPILER-LEXPR-APPLY
                                  code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation
  (LAP ,@(clear-map!)
       ,@(pop regnum:utility-arg1)
       ,@(object->address regnum:utility-arg1 regnum:utility-arg1)
       ,@(load-unsigned-immediate regnum:utility-arg2 number-pushed)
       ,@(invoke-interface/shared 'COMPILER-LEXPR-APPLY
                                  code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       (B (@PCR (+ ,(free-uuo-link-label name frame-size)
                   (* 4 ,(uuo-link-label-instruction-offset)))
                 ,regnum:scratch-0))))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       (B (@PCR (+ ,(global-uuo-link-label name frame-size)
                   (* 4 ,(uuo-link-label-instruction-offset)))
                ,regnum:scratch-0))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? continuation) (? extension))
  (error "Unsupported RTL:"
         `(INVOCATION:CACHE-REFERENCE ,frame-size ,continuation ,extension)))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? continuation) (? extension))
  (error "Unsupported RTL:"
         `(INVOCATION:CACHE-REFERENCE ,frame-size ,continuation ,extension)))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation
  (cond ((eq? primitive compiled-error-procedure)
         (generate/compiled-error frame-size))
        ;; ((eq? primitive (ucode-primitive set-interrupt-enables!)) ...)
        ;; ((eq? primitive (ucode-primitive with-interrupt-mask)) ...)
        ;; ((eq? primitive (ucode-primitive with-interrupts-reduced)) ...)
        ;; ((eq? primitive (ucode-primitive with-stack-marker)) ...)
        (else
         (generate/generic-primitive frame-size primitive))))

(define (generate/compiled-error frame-size)
  (let* ((prefix (clear-map!))
         (arg1 (load-unsigned-immediate regnum:utility-arg1 frame-size))
         (invocation
          (invoke-interface/shared 'COMPILER-ERROR code:compiler-error)))
    (LAP ,@prefix
         ,@arg1
         ,@invocation)))

(define (generate/generic-primitive frame-size primitive)
  (let* ((prefix (clear-map!))
         (arg1 (load-constant regnum:utility-arg1 primitive)))
    (LAP ,@prefix
         ,@arg1
         ,@(let ((arity (primitive-procedure-arity primitive)))
             (cond ((not (negative? arity))
                    (generate/primitive-apply))
                   ((= arity -1)
                    (generate/primitive-lexpr-apply frame-size))
                   (else
                    (generate/generic-apply frame-size)))))))

(define (generate/primitive-apply)
  (invoke-interface/shared 'COMPILER-PRIMITIVE-APPLY
                           code:compiler-primitive-apply))

(define (generate/primitive-lexpr-apply frame-size)
  (let* ((load-nargs
          (load-unsigned-immediate regnum:scratch-0 (- frame-size 1)))
         (invocation
          (invoke-interface/shared 'COMPILER-PRIMITIVE-LEXPR-APPLY
                                   code:compiler-primitive-lexpr-apply)))
    (LAP ,@load-nargs
         (STR X ,regnum:scratch-0 ,reg:lexpr-primitive-arity)
         ,@invocation)))

(define (generate/generic-apply frame-size)
  (let* ((arg2 (load-unsigned-immediate regnum:utility-arg2 frame-size))
         (invocation
          (invoke-interface/shared 'COMPILER-APPLY code:compiler-apply)))
    (LAP ,@arg2
         ,@invocation)))

(let-syntax
    ((define-primitive-invocation
       (sc-macro-transformer
        (lambda (form environment)
          (let ((name (cadr form)))
            `(define-rule statement
               (INVOCATION:SPECIAL-PRIMITIVE
                (? frame-size)
                (? continuation)
                ,(make-primitive-procedure name #t))
               frame-size continuation
               (expect-no-exit-interrupt-checks)
               #|
               (special-primitive-invocation
                ,(close-syntax (symbol 'CODE:COMPILER- name)
                               environment))
               |#
               (optimized-primitive-invocation
                ,(close-syntax (symbol 'ENTRY:COMPILER- name)
                               environment))))))))

  (define-primitive-invocation &+)
  (define-primitive-invocation &-)
  (define-primitive-invocation &*)
  (define-primitive-invocation &/)
  (define-primitive-invocation &=)
  (define-primitive-invocation &<)
  (define-primitive-invocation &>)
  (define-primitive-invocation 1+)
  (define-primitive-invocation -1+)
  (define-primitive-invocation zero?)
  (define-primitive-invocation positive?)
  (define-primitive-invocation negative?)
  (define-primitive-invocation quotient)
  (define-primitive-invocation remainder))

(define (special-primitive-invocation code)
  (let* ((prefix (clear-map!))
         (invocation (invoke-interface/shared code)))
    (LAP ,@prefix
         ,@invocation)))

(define (optimized-primitive-invocation entry)
  (let* ((prefix (clear-map!))
         (invocation (invoke-hook entry)))
    (LAP ,@prefix
         ,@invocation)))

;;;; Invocation Prefixes

;;; (INVOCATION-PREFIX:MOVE-FRAME-UP <nwords> <address>)
;;;
;;;     Pop <nwords> off the stack, set the stack to <address>, and
;;;     push them back on the stack.
;;;
;;; (INVOCATION-PREFIX:DYNAMIC-LINK <nwords> <address> <dynamic-link>)
;;;
;;;     Pop <nwords> off the stack, set the stack pointer to the larger
;;;     (i.e., more items on the stack, or lower addresses) of
;;;     <address> or <dynamic-link>, and push them back on the stack.

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER (? address)))
  (let ((address (standard-source! address)))
    (assert (not (= address regnum:stack-pointer)))
    (generate/move-frame-up frame-size address)))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
                                  (REGISTER (? address))
                                  (REGISTER (? dynamic-link)))
  ;; Could try to get a temporary out of the dynamic link, but we have
  ;; lots of temporaries and this is probably the dedicated dynamic
  ;; link machine register anyway.
  (let* ((dynamic-link (standard-source! dynamic-link))
         (address (standard-move-to-temporary! address)))
    (assert (not (= address regnum:stack-pointer)))
    (assert (not (= dynamic-link regnum:stack-pointer)))
    (LAP (CMP X ,address ,dynamic-link)
         (CSEL X GT ,address ,address ,dynamic-link)
         ,@(generate/move-frame-up frame-size address))))

(define (generate/move-frame-up frame-size address)
  (assert (not (= address regnum:stack-pointer)))
  (if (<= frame-size 6)                 ;Covers vast majority of cases.
      (generate/move-frame-up/unrolled frame-size address)
      (generate/move-frame-up/loop frame-size address)))

(define (generate/move-frame-up/loop frame-size address)
  (assert (not (= address regnum:stack-pointer)))
  (assert (>= frame-size 2))
  (assert (fits-in-unsigned-12? (* 8 frame-size))) ;XXX
  (assert (= 8 address-units-per-object))
  (let* ((temp1 (allocate-temporary-register! 'GENERAL))
         (temp2 (allocate-temporary-register! 'GENERAL))
         (index (allocate-temporary-register! 'GENERAL))
         (label (generate-label 'MOVE-LOOP))
         ;; Unroll an odd element if there is one; then do an even
         ;; number of iterations.
         (loop-count (- frame-size (remainder frame-size 2))))
    (assert (= loop-count (* (quotient frame-size 2) 2)))
    (assert (not (= index regnum:scratch-0)))
    (LAP (ADD X ,regnum:stack-pointer ,regnum:stack-pointer
              (&U ,(* 8 frame-size)))
         ,@(if (odd? frame-size)
               (LAP (LDR X ,temp1 (PRE+ ,regnum:stack-pointer (& -8)))
                    (STR X ,temp1 (PRE+ ,address (& -8))))
               (LAP))
         ,@(load-unsigned-immediate index loop-count)
        (LABEL ,label)
         (SUB X ,index ,index (&U #x10))
         (LDP X ,temp1 ,temp2 (PRE+ ,regnum:stack-pointer (& (* 8 -2))))
         (STP X ,temp1 ,temp2 (PRE+ ,address (& (* 8 -2))))
         (CBNZ X ,index (@PCR ,label ,regnum:scratch-0))
         ,@(register->register-transfer address regnum:stack-pointer))))

(define (generate/move-frame-up/unrolled frame-size address)
  (assert (not (= address regnum:stack-pointer)))
  (assert (< frame-size 24))       ;Only 24 temporaries, incl. address.
  (assert (= 8 address-units-per-object))
  (let ((temps
         ;; Allocate in order to get reproducible results.
         (let loop ((n frame-size) (temps '()))
           (if (zero? n)
               temps
               (let ((temp (allocate-temporary-register! 'GENERAL)))
                 (loop (- n 1) (cons temp temps)))))))
    (LAP ,@(let loop ((temps temps))
             ;; (pop2 r1 r2) (pop2 r3 r4) (pop r5)
             (if (pair? temps)
                 (if (pair? (cdr temps))
                     (LAP ,@(pop2 (car temps) (cadr temps))
                          ,@(loop (cddr temps)))
                     (pop (car temps)))
                 (LAP)))
         ,@(register->register-transfer address regnum:stack-pointer)
         ,@(let loop ((temps temps))
             ;; (push r5) (push2 r4 r3) (push2 r2 r1)
             (if (pair? temps)
                 (if (pair? (cdr temps))
                     (LAP ,@(loop (cddr temps))
                          ,@(push2 (cadr temps) (car temps)))
                     (push (car temps)))
                 (LAP))))))

;;;; External Labels

;;; Entry point types

(define (make-procedure-code-word min max)
  ;; The "min" byte must be less than #x80; the "max" byte may not
  ;; equal #x80 but can take on any other value.
  (if (or (negative? min) (>= min #x80))
      (error "MAKE-PROCEDURE-CODE-WORD: minimum out of range" min))
  (if (>= (abs max) #x80)
      (error "MAKE-PROCEDURE-CODE-WORD: maximum out of range" max))
  (make-code-word min (if (negative? max) (+ #x100 max) max)))

(define internal-entry-code-word
  (make-code-word #xff #xfe))

(define internal-continuation-code-word
  (make-code-word #xff #xfc))

(define (frame-size->code-word offset default)
  (cond ((not offset)
         default)
        ((< offset #x2000)
         ;; This uses up through (#xff #xdf).
         (let ((qr (integer-divide offset #x80)))
           (make-code-word (+ #x80 (integer-divide-remainder qr))
                           (+ #x80 (integer-divide-quotient qr)))))
        (else
         (error "Unable to encode continuation offset"
                offset))))

(define (continuation-code-word label)
  (frame-size->code-word
   (if label
       (rtl-continuation/next-continuation-offset (label->object label))
       0)
   internal-continuation-code-word))

(define (internal-procedure-code-word rtl-proc)
  (frame-size->code-word
   (rtl-procedure/next-continuation-offset rtl-proc)
   internal-entry-code-word))

;;;; Procedure headers

;;; The following calls MUST appear as the first thing at the entry
;;; point of a procedure.  They assume that the register map is clear
;;; and that no register contains anything of value.
;;;
;;; The only reason that this is true is that no register is live
;;; across calls.  If that were not true, then we would have to save
;;; any such registers on the stack so that they would be GC'ed
;;; appropriately.
;;;
;;; The only exception is the dynamic link register, handled
;;; specially.  Procedures that require a dynamic link use a different
;;; interrupt handler that saves and restores the dynamic link
;;; register.

(define (interrupt-check checks label)
  (LAP ,@(if (or (memq 'INTERRUPT checks) (memq 'HEAP checks))
             (LAP (LDR X ,regnum:scratch-0 ,reg:memtop)
                  (CMP X ,regnum:free-pointer ,regnum:scratch-0)
                  (B. GE (@PCR ,label ,regnum:scratch-0)))
             (LAP))
       ,@(if (memq 'STACK checks)
             (LAP (LDR X ,regnum:scratch-0 ,reg:stack-guard)
                  (CMP X ,regnum:stack-pointer ,regnum:scratch-0)
                  (B. LT (@PCR ,label ,regnum:scratch-0)))
             (LAP))))

(define (generate-procedure-header code-word label generate-interrupt-stub)
  (let ((checks (get-entry-interrupt-checks))
        (interrupt-label (generate-label 'INTERRUPT)))
    ;; Put the interrupt check branch target after the branch so that
    ;; it is a forward branch, which CPUs will predict not taken by
    ;; default, in the absence of dynamic branch prediction profile
    ;; data.
    (if (pair? checks)
        (add-end-of-block-code!
         (lambda ()
           (LAP (LABEL ,interrupt-label)
                ,@(generate-interrupt-stub)))))
    (LAP ,@(make-external-label code-word label)
         ,@(interrupt-check checks interrupt-label))))

(define (simple-procedure-header code-word label name code)
  (generate-procedure-header
   code-word
   label
   (lambda ()
     (invoke-interface/shared-reentry name code label))))

(define (dlink-procedure-header code-word label)
  (generate-procedure-header
   code-word
   label
   (lambda ()
     ;; Save the dynamic link to an interpreter register, and then ask
     ;; for help from the microcode.
     ;;
     ;; XXX The goal of sharing here is to reduce code size; it would
     ;; be nice if we could ask the assembler to not share if we're so
     ;; far away from the label that we require an indirect branch.
     (LAP (ADR X ,regnum:utility-arg1 (@PCR ,label ,regnum:scratch-0))
          ,@(interrupt-procedure-dlink)))))

(define (interrupt-procedure-dlink)
  ;; Caller must arrange to load the entry into arg1.
  (share-instruction-sequence! 'INTERRUPT-PROCEDURE-DLINK
    (lambda (subroutine)
      (LAP (B (@PCR ,subroutine ,regnum:scratch-0))))
    (lambda (subroutine)
      (LAP (LABEL ,subroutine)
           (STR X ,regnum:dynamic-link ,reg:dynamic-link)
           ,@(invoke-interface code:compiler-interrupt-dlink)))))

(define-rule statement
  (CONTINUATION-ENTRY (? internal-label))
  (expect-no-entry-interrupt-checks)
  (make-external-label (continuation-code-word internal-label)
                       internal-label))

(define-rule statement
  (CONTINUATION-HEADER (? internal-label))
  #|
  (simple-procedure-header (continuation-code-word internal-label)
                           internal-label
                           'INTERRUPT-CONTINUATION
                           code:compiler-interrupt-continuation)
  |#
  (expect-no-entry-interrupt-checks)
  (make-external-label (continuation-code-word internal-label)
                       internal-label))

(define-rule statement
  (IC-PROCEDURE-HEADER (? internal-label))
  (error "IC procedures not supported:"
         `(IC-PROCEDURE-HEADER ,internal-label)))

(define-rule statement
  (OPEN-PROCEDURE-HEADER (? internal-label))
  (let* ((rtl-proc (label->object internal-label))
         (code-word (internal-procedure-code-word rtl-proc)))
    (LAP (EQUATE ,(rtl-procedure/external-label rtl-proc) ,internal-label)
         ,@(if (rtl-procedure/dynamic-link? rtl-proc)
               (dlink-procedure-header code-word internal-label)
               (simple-procedure-header code-word
                                        internal-label
                                        'INTERRUPT-PROCEDURE
                                        code:compiler-interrupt-procedure)))))

(define-rule statement
  (PROCEDURE-HEADER (? internal-label) (? min) (? max))
  (LAP (EQUATE ,(rtl-procedure/external-label (label->object internal-label))
               ,internal-label)
       ,@(simple-procedure-header (make-procedure-code-word min max)
                                  internal-label
                                  'INTERRUPT-PROCEDURE
                                  code:compiler-interrupt-procedure)))

;;;; Closures

(define-rule statement
  (CLOSURE-HEADER (? internal-label) (? nentries) (? entry))
  entry                                 ;ignore
  (let* ((rtl-proc (label->object internal-label))
         (external-label (rtl-procedure/external-label rtl-proc))
         (checks (get-entry-interrupt-checks))
         (type type-code:compiled-entry))
    (define (label+adjustment)
      (assert (not (= regnum:applicand regnum:scratch-0)))
      (LAP ,@(make-external-label internal-entry-code-word external-label)
           ;; regnum:applicand holds the untagged entry address.
           ;; Push and tag it.
           ,@(affix-type regnum:applicand type regnum:applicand
                         (lambda () regnum:scratch-0))
           ,@(push regnum:applicand)
          (LABEL ,internal-label)))
    (cond ((zero? nentries)
           (LAP (EQUATE ,external-label ,internal-label)
                ,@(simple-procedure-header
                   (internal-procedure-code-word rtl-proc)
                   internal-label
                   'INTERRUPT-PROCEDURE
                   code:compiler-interrupt-procedure)))
          ((pair? checks)
           (LAP ,@(label+adjustment)
                ,@(interrupt-check checks (closure-interrupt-label))))
          (else
           (label+adjustment)))))

(define (closure-interrupt-label)
  ;; XXX Would be nice if we could ask the assembler to duplicate this
  ;; whenever we're getting far enough that the conditional branch
  ;; target requires branch tensioning.
  (or (block-association 'INTERRUPT-CLOSURE)
      (let ((label (generate-label 'INTERRUPT-CLOSURE)))
        (add-end-of-block-code!
         (lambda ()
           (LAP (LABEL ,label)
                ,@(invoke-interface code:compiler-interrupt-closure))))
        (block-associate! 'INTERRUPT-CLOSURE label)
        label)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
                        (? min) (? max) (? size)))
  (generate/cons-closure target procedure-label min max size))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (CONS-MULTICLOSURE (? nentries) (? size) (? entries)))
  (case nentries
    ((0)
     ;; Allocate a vector, initialized with garbage -- caller must
     ;; initialize it before we can GC.
     (let* ((target (standard-target! target))
            (Free regnum:free-pointer))
       (LAP ,@(load-tagged-immediate type-code:manifest-vector size target)
            (STR X ,target ,Free)
            ,@(register->register-transfer Free target)
            ,@(add-immediate Free Free
                             (* address-units-per-object (+ 1 size))))))
    ((1)
     (let ((entry (vector-ref entries 0)))
       (generate/cons-closure target
                              (car entry) (cadr entry) (caddr entry)
                              size)))
    (else
     (generate/cons-multiclosure target nentries size
                                 (vector->list entries)))))

(define (generate/cons-closure target label min max size)
  (let* ((target (standard-target! target))
         (temp (allocate-temporary-register! 'GENERAL))
         (manifest-type type-code:manifest-closure)
         (manifest-size (closure-manifest-size size))
         (Free regnum:free-pointer))
    (LAP ,@(load-tagged-immediate manifest-type manifest-size temp)
         (STR X ,temp (POST+ ,Free (& 8)))
         ,@(generate-closure-entry label 1 min max 1 temp)
         ;; Free now points at the entry.  Save it in target.
         ,@(register->register-transfer Free target)
         ;; Bump Free to point at the last component, one word before
         ;; the next object.  We do this because we need to set the
         ;; last component here, but we do not have negative load/store
         ;; offsets without pre/post-increment.
         ,@(add-immediate Free Free (* 8 size))
         ;; Set the last component to be the relocation reference point.
         ,@(affix-type temp type-code:compiled-entry target
                       (lambda () (allocate-temporary-register! 'GENERAL)))
         (STR X ,temp (POST+ ,Free (& 8))))))

(define (generate/cons-multiclosure target nentries size entries)
  (let* ((target (standard-target! target))
         (temp (allocate-temporary-register! 'GENERAL))
         (manifest-type type-code:manifest-closure)
         (manifest-size (multiclosure-manifest-size nentries size))
         ;; 8 for manifest, 8 for padding & format word, 8 for PC offset.
         (offset0 #x18)
         (Free regnum:free-pointer))
    (define (generate-primary-entry entry)
      (let ((label (car entry)) (min (cadr entry)) (max (caddr entry)))
        (generate-closure-entry label nentries min max offset0 temp)))
    (define (generate-subsidiary-entry entry n)
      (let ((label (car entry))
            (min (cadr entry))
            (max (caddr entry))
            (offset (+ offset0 (* n address-units-per-closure-entry))))
        (generate-closure-entry label 0 min max offset temp)))
    (define (generate-subsidiary-entries entries n)
      (assert (pair? entries))
      (LAP ,@(generate-subsidiary-entry (car entries) n)
           ,@(if (pair? (cdr entries))
                 (generate-subsidiary-entries (cdr entries) (+ n 1))
                 (LAP))))
    (LAP ,@(load-tagged-immediate manifest-type manifest-size temp)
         (STR X ,temp (POST+ ,Free (& 8)))
         ,@(generate-primary-entry (car entries))
         ,@(register->register-transfer Free target)
         ,@(generate-subsidiary-entries (cdr entries) 1)
         ;; Bump Free to point at the last component, one word before
         ;; the next object.  We do this because we need to set the
         ;; last component here, but we do not have negative load/store
         ;; offsets without pre/post-increment.
         ,@(add-immediate Free Free (* 8 size))
         ;; Set the last component to be the relocation reference point.
         ,@(affix-type temp type-code:compiled-entry target
                       (lambda () (allocate-temporary-register! 'GENERAL)))
         (STR X ,temp (POST+ ,Free (& 8))))))

(define (generate-closure-entry label padding min max offset temp)
  (let* ((label* (rtl-procedure/external-label (label->object label)))
         (code-word (make-procedure-code-word min max))
         (Free regnum:free-pointer))
    ;; Could avoid zeroing the padding if we don't need it, but there's
    ;; no advantage.
    (define (padded-word)
      ;; padding(32) || code-word(16) || offset(16)
      (case endianness
        ((BIG)
         (bitwise-ior (shift-left padding 32)
                      (bitwise-ior (shift-left code-word 16)
                                   offset)))
        ((LITTLE)
         (bitwise-ior padding
                      (bitwise-ior (shift-left code-word 32)
                                   (shift-left offset 48))))
        (else
         (error "Unknown endianness:" endianness))))
    (assert (not (= temp regnum:scratch-0)))
    (LAP ,@(load-unsigned-immediate temp (padded-word))
         (STR X ,temp (POST+ ,Free (& 8)))
         ;; Set temp := label - 8.
         (ADR X ,temp (@PCR (- ,label* 8) ,regnum:scratch-0))
         ;; Set temp := label - 8 - free = label - (free + 8).
         (SUB X ,temp ,temp ,Free)
         ;; Store the PC offset.
         (STR X ,temp (POST+ ,Free (& 8))))))

(define (closure-manifest-size size)
  (multiclosure-manifest-size 1 size))

(define (multiclosure-manifest-size nentries size)
  ;; Each entry occupies two object-sized units.
  (+ (* 2 nentries)
     ;; Add one for the relocation reference point.
     (+ size 1)))

;;;; Entry Header

;;; XXX Why are these hand-coded assembly routines and not C functions?
;;; For that matter, why aren't they just the job of the loader?

;;; (GENERATE/QUOTATION-HEADER <environment-label> <free-ref-label> <nsects>)
;;;
;;;     Store the interpreter's environment register in this block's
;;;     environment slot; then call link(block_addr, constants_addr,
;;;     nsects).

(define (generate/quotation-header environment-label free-ref-label n-sections)
  (let ((continuation-label (generate-label 'LINKED)))
    (assert (not (= r0 regnum:scratch-0)))
    (assert (not (= r1 regnum:scratch-0)))
    (LAP (LDR X ,r0 ,reg:environment)
         (ADR X ,r1 (@PCR ,environment-label ,regnum:scratch-0))
         (STR X ,r0 ,r1)
         (ADR X ,regnum:utility-arg2 (@PCR ,*block-label* ,regnum:scratch-0))
         (ADR X ,regnum:utility-arg3 (@PCR ,free-ref-label ,regnum:scratch-0))
         ,@(load-unsigned-immediate regnum:utility-arg4 n-sections)
         ,@(invoke-interface/call code:compiler-link continuation-label)
         ,@(make-external-label (continuation-code-word #f)
                                continuation-label))))

(define (generate/remote-link code-block-label
                              environment-offset
                              free-ref-offset
                              n-sections)
  (let ((continuation-label (generate-label 'LINKED))
        ;; arg1 will be the return address.
        (arg2 regnum:utility-arg2)
        (arg3 regnum:utility-arg3)
        (arg4 regnum:utility-arg4)
        (temp r1))
    (LAP (LDR X ,temp ,reg:environment)
         ;; arg2 := block address
         ,@(load-pc-relative arg2 code-block-label)
         ,@(object->address arg2 arg2)
         ;; Set this block's environment.
         (STR X ,temp (+ ,arg2 (&U (* 8 ,environment-offset))))
         ;; arg3 := constants address
         ,@(add-immediate arg3 arg2 free-ref-offset)
         ;; arg4 := n sections
         ,@(load-unsigned-immediate arg4 n-sections)
         ,@(invoke-interface/call code:compiler-link continuation-label)
         ,@(make-external-label (continuation-code-word #f)
                                continuation-label))))

(define (generate/remote-links n-blocks vector-label nsects)
  (if (zero? n-blocks)
      (LAP)
      (let* ((loop-label (generate-label 'LOOP))
             (nsects-label (generate-label 'NSECTS))
             (end-label (generate-label 'END))
             (continuation-label (generate-label 'LINKED))
             (counter r24)              ;unallocated, callee-saves
             (temp1 r1)                 ;unallocated
             (temp2 r2)                 ;unallocated
             ;; arg1 will be return address.
             (arg2 regnum:utility-arg2)
             (arg3 regnum:utility-arg3)
             (arg4 regnum:utility-arg4))
        (assert (not (= counter regnum:scratch-0)))
        (LAP ,@(load-unsigned-immediate counter n-blocks)
            (LABEL ,loop-label)
             ,@(load-pc-relative arg2 vector-label)     ;arg2 := vector
             ,@(object->address arg2 arg2)              ;arg2 := vector addr
             (LDR X ,arg2 (+ ,arg2 (LSL ,counter 3)))   ;arg2 := vector[ctr-1]
             ,@(object->address arg2 arg2)              ;arg2 := block addr
             (LDR X ,temp1 ,reg:environment)            ;temp1 := environment
             (LDR X ,temp2 ,arg2)                       ;temp2 := manifest
             ,@(object->datum temp2 temp2)              ;temp2 := block length
             (STR X ,temp1 (+ ,arg2 (LSL ,temp2 3)))    ;set block environment
             (LDR X ,temp1 (+ ,arg2 (&U (* 8 1))))      ;temp1 := manifest-nmv
             ,@(object->datum temp1 temp1)              ;temp1 := unmarked size
             (ADD X ,temp1 ,temp1 (&U #x10))            ;temp1 := consts offset
             (ADD X ,arg3 ,arg2 ,temp1)                 ;temp1 := consts addr
             (SUB X ,counter ,counter (&U 1))           ;ctr := ctr - 1
             (ADR X ,arg4 (@PCR ,nsects ,regnum:scratch-0)) ;arg4 := nsects
             (LDR B ,arg4 (+ ,arg4 ,counter))           ;arg4 := nsects[ctr]
             ,@(invoke-interface/call code:compiler-link continuation-label)
             ,@(make-external-label (continuation-code-word #f)
                                    continuation-label)
             (CBNZ X ,counter                           ;repeat if ctr != 0
                   (@PCR ,loop-label ,regnum:scratch-0))
             (B (@PCR ,end-label ,regnum:scratch-0))    ;otherwise go on
            (LABEL ,nsects-label)
             ,@(generate/nsects nsects)
            (LABEL ,end-label)))))

(define (generate/nsects nsects)
  (let ((n (vector-length nsects)))
    (define (adjoin/be byte word bits)
      (bitwise-ior (shift-left byte bits) word))
    (define (adjoin/le byte word bits)
      bits
      (bitwise-ior byte (shift-left word 8)))
    (define adjoin
      (case endianness
        ((BIG) adjoin/be)
        ((LITTLE) adjoin/le)
        (else (error "Unknown endianness:" endianness))))
    (let loop
        ((i (* (quotient (+ n 7) 8) 8))
         (words (LAP)))
      (if (< 0 i)
          (let subloop ((j 0) (word 0))
            (if (< j 8)
                (let ((byte (if (< (+ i j) n) (vector-ref nsects (+ i j)) 0)))
                  (subloop (+ j 1) (adjoin byte word (* j 8))))
                (loop (- i 8)
                      (LAP (DATA 64 U ,word)
                           ,@words))))
          words))))

(define (generate/constants-block constants references assignments
                                  uuo-links global-links static-vars)
  (let ((constant-info
         (declare-constants 0 (transmogrifly uuo-links)
           (declare-constants 1 references
             (declare-constants 2 assignments
               (declare-constants 3 (transmogrifly global-links)
                 (declare-constants false
                     (map (lambda (pair)
                            (cons false (cdr pair)))
                          static-vars)
                   (declare-constants false constants
                     (cons false (LAP))))))))))
    (let ((free-ref-label (car constant-info))
          (constants-code (cdr constant-info))
          (debugging-information-label (allocate-constant-label))
          (environment-label (allocate-constant-label))
          (n-sections
           (+ (if (null? uuo-links) 0 1)
              (if (null? references) 0 1)
              (if (null? assignments) 0 1)
              (if (null? global-links) 0 1))))
      (values
       (LAP ,@constants-code
            ;; Place holder for the debugging info filename
            (SCHEME-OBJECT ,debugging-information-label DEBUGGING-INFO)
            ;; Place holder for the load time environment if needed
            (SCHEME-OBJECT ,environment-label
                           ,(if (null? free-ref-label) 0 'ENVIRONMENT)))
       environment-label
       free-ref-label
       n-sections))))

(define (declare-constants tag constants info)
  (define (inner constants)
    (if (null? constants)
        (cdr info)
        (let ((entry (car constants)))
          (LAP (SCHEME-OBJECT ,(cdr entry) ,(car entry))
               ,@(inner (cdr constants))))))
  (if (and tag (not (null? constants)))
      (let ((label (allocate-constant-label)))
        (cons label
              (inner
               `((,(let ((datum (length constants)))
                     (if (> datum #xffff)
                         (error "datum too large" datum))
                     (+ (* tag #x10000) datum))
                  . ,label)
                 ,@constants))))
      (cons (car info) (inner constants))))

(define (transmogrifly variable.caches-list)
  (append-map
   (lambda (variable.caches)
     (append-map (let ((variable (car variable.caches)))
                   (lambda (cache)
                     (let ((frame-size (car cache))
                           (label (cdr cache)))
                       ;; Must match UUO_LINK_SIZE in cmpintmd/aarch64.h.
                       (case endianness
                         ((BIG)
                          `((,variable . ,(allocate-constant-label))
                            (#f . ,label)
                            (#f . ,(allocate-constant-label))
                            (,frame-size . ,(allocate-constant-label))))
                         ((LITTLE)
                          `((,variable . ,(allocate-constant-label))
                            (,frame-size . ,label)
                            (#f . ,(allocate-constant-label))
                            (#f . ,(allocate-constant-label))))
                         (else
                          (error "Unknown endianness:" endianness))))))
                 (cdr variable.caches)))
   variable.caches-list))

(define (uuo-link-label-instruction-offset)
  (case endianness
    ;; On big-endian systems, the label points exactly at the code,
    ;; aligned on an object boundary.
    ((BIG) 0)
    ;; On little-endian systems, the code starts halfway in the middle
    ;; of the frame size object, clobbering the fixnum tag but leaving
    ;; the 16-bit value intact.
    ((LITTLE) 1)
    (else (error "Unknown endianness:" endianness))))
