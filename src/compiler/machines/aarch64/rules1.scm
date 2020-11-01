#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Data Transfers.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Register Assignments

(assert (zero? (remainder address-units-per-object 4)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (assign-register->register target source))

;;;; Tagging and detagging

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (CONS-POINTER (REGISTER (? type))
                        (REGISTER (? datum))))
  (standard-binary target type datum
    (lambda (target type datum)
      (LAP (ORR X ,target ,datum (LSL ,type ,scheme-datum-width))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (CONS-POINTER (MACHINE-CONSTANT (? type))
                        (REGISTER (? datum))))
  (standard-unary target datum
    (lambda (target datum)
      (affix-type target type datum general-temporary!))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (standard-unary target source object->type))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (REGISTER (? source))))
  (standard-unary target source object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (standard-unary target source object->address))

;;;; Loading constants

(define-rule statement
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? n)))
  (load-signed-immediate (standard-target! target) n))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? object)))
  (load-constant (standard-target! target) object))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (CONS-POINTER (MACHINE-CONSTANT (? type))
                        (MACHINE-CONSTANT (? datum))))
  (load-tagged-immediate (standard-target! target) type datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ENTRY:PROCEDURE (? label)))
  (load-pc-relative-address
   (standard-target! target)
   (rtl-procedure/external-label (label->object label))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ENTRY:CONTINUATION (? label)))
  (rtl-target:=machine-register! target regnum:link-register)
  (let ((linked (generate-label 'LINKED)))
    (LAP (BL (@PCR ,linked ,regnum:scratch-0))
         (B (@PCR ,label ,regnum:scratch-0))
        (LABEL ,linked))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (load-pc-relative (standard-target! target) (free-reference-label name)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (load-pc-relative (standard-target! target) (free-assignment-label name)))

;;;; Address arithmetic

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (BYTE-OFFSET-ADDRESS (REGISTER (? base))
                               (REGISTER (? offset))))
  (load-indexed-address target base offset 1))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (BYTE-OFFSET-ADDRESS (REGISTER (? base))
                               (MACHINE-CONSTANT (? offset))))
  (load-displaced-address target base offset 1))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (OFFSET-ADDRESS (REGISTER (? base))
                          (REGISTER (? offset))))
  (load-indexed-address target base offset address-units-per-object))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (OFFSET-ADDRESS (REGISTER (? base))
                          (MACHINE-CONSTANT (? offset))))
  (load-displaced-address target base offset address-units-per-object))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLOAT-OFFSET-ADDRESS (REGISTER (? base))
                                (REGISTER (? offset))))
  (load-indexed-address target base offset address-units-per-float))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLOAT-OFFSET-ADDRESS (REGISTER (? base))
                                (MACHINE-CONSTANT (? offset))))
  (load-displaced-address target base offset address-units-per-float))

;;;; Loads and stores

;;; Load indexed

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (OFFSET (REGISTER (? base)) (REGISTER (? offset))))
  (QUALIFIER (not (= offset rsp)))
  (standard-binary target base offset
    (lambda (target base offset)
      (LAP (LDR X ,target (+ ,base (LSL ,offset 3)))))))

;;; Store indexed

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? base)) (REGISTER (? offset)))
          (? source register-expression))
  (QUALIFIER (not (= offset rsp)))
  (standard-ternary-effect base offset source
    (lambda (base offset source)
      (LAP (STR X ,source (+ ,base (LSL ,offset 3)))))))

;;; Load with displacement

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (OFFSET (REGISTER (? base)) (MACHINE-CONSTANT (? offset))))
  (QUALIFIER (fits-in-unsigned-12? offset))
  (standard-unary target base
    (lambda (target base)
      (LAP (LDR X ,target (+ ,base (&U (* 8 ,offset))))))))

;;; Store with displacement

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? base)) (MACHINE-CONSTANT (? offset)))
          (? source register-expression))
  (QUALIFIER (fits-in-unsigned-12? offset))
  (standard-binary-effect base source
    (lambda (base source)
      (LAP (STR X ,source (+ ,base (&U (* 8 ,offset))))))))

;;;; Loads and stores with pre/post-increment

;;; Load with pre-increment: *++x

(define-rule statement
  (ASSIGN (REGISTER (? target)) (PRE-INCREMENT (REGISTER (? sp)) (? offset)))
  (QUALIFIER (fits-in-signed-9? (* address-units-per-object offset)))
  (standard-unary target sp
    (lambda (target sp)
      (LAP (LDR X ,target
		(PRE+ ,sp (& ,(* address-units-per-object offset))))))))

;;; Load with post-increment: *x++

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER (? sp)) (? offset)))
  (QUALIFIER (fits-in-signed-9? (* address-units-per-object offset)))
  (standard-unary target sp
    (lambda (target sp)
      (LAP (LDR X ,target
		(POST+ ,sp (& ,(* address-units-per-object offset))))))))

;;; Store with pre-increment: *++x = y

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER (? sp)) (? offset))
          (? source register-expression))
  (QUALIFIER (fits-in-signed-9? (* address-units-per-object offset)))
  (standard-binary-effect source sp
    (lambda (source sp)
      (LAP (STR X ,source
		(PRE+ ,sp (& ,(* address-units-per-object offset))))))))

;;; Store with post-increment: *x++ = y

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER (? sp)) (? offset))
          (? source register-expression))
  (QUALIFIER (fits-in-signed-9? (* address-units-per-object offset)))
  (standard-binary-effect source sp
    (lambda (source sp)
      (LAP (STR X ,source
		(POST+ ,sp (& ,(* address-units-per-object offset))))))))

;;;; Byte access

;;; Detagging a character -- no ASCII->CHAR because that's just
;;; CONS-NON-POINTER = CONS-POINTER.

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CHAR->ASCII (REGISTER (? source))))
  (standard-unary target source
    (lambda (target source)
      (LAP (AND X ,target ,source (&U #xff))))))

;;; Load byte indexed

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (BYTE-OFFSET (REGISTER (? base)) (REGISTER (? offset))))
  (QUALIFIER (not (= offset rsp)))
  (standard-binary target base offset
    (lambda (target base offset)
      (LAP (LDR B ,target (+ ,base ,offset))))))

;;; Store byte indexed

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? base)) (REGISTER (? offset)))
          (? source register-expression))
  (standard-ternary-effect base offset source
    (lambda (base offset source)
      (LAP (STR B ,source (+ ,base ,offset))))))

;;; Detag and store byte indexed

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? base)) (REGISTER (? offset)))
          (CHAR->ASCII (? source register-expression)))
  (standard-ternary-effect base offset source
    (lambda (base offset source)
      (LAP (STR B ,source (+ ,base ,offset))))))

;;; Load byte with displacement

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (BYTE-OFFSET (REGISTER (? base))
                       (MACHINE-CONSTANT (? offset))))
  (QUALIFIER (not (= offset rsp)))
  (standard-unary target base
    (lambda (target base)
      (LAP (LDR B ,target (+ ,base (&U ,offset)))))))

;;; Store byte with displacement

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? base))
                       (MACHINE-CONSTANT (? offset)))
          (? source register-expression))
  (QUALIFIER (not (= offset rsp)))
  (standard-binary-effect source base
    (lambda (source base)
      (LAP (STR B ,source (+ ,base (&U ,offset)))))))

;;; Detag and store byte with displacement

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? base))
                       (MACHINE-CONSTANT (? offset)))
          (CHAR->ASCII (? source register-expression)))
  (QUALIFIER (not (= offset rsp)))
  (standard-binary-effect source base
    (lambda (source base)
      (LAP (STR B ,source (+ ,base (&U ,offset)))))))
