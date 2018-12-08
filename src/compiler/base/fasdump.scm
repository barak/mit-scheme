#| -*-Scheme-*-

Copyright (C) 2013, 2018 Taylor R Campbell

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

;;;; Portable fasdumper
;;; package: (runtime portable-fasdump)

(declare (usual-integrations))

;;;; Fasdump formats

(define-structure (fasdump-format
                   (safe-accessors)
                   (conc-name format.)
                   (keyword-constructor make-fasdump-format))
  (version #f read-only #t)
  (architecture #f read-only #t)
  (marker #f read-only #t)
  (bits-per-type #f read-only #t)
  (bits-per-datum #f read-only #t)
  (bits-per-byte #f read-only #t)
  (bytes-per-word #f read-only #t)
  (words-per-float #f read-only #t)
  (float-align-words #f read-only #t)
  (bits-per-bignum-digit #f read-only #t)
  (words-per-bignum-digit #f read-only #t)
  (greatest-fixnum #f read-only #t)
  (least-fixnum #f read-only #t)
  (write-word #f read-only #t)
  (write-untagged-word #f read-only #t)
  (write-bignum-digit #f read-only #t)
  (write-float #f read-only #t))

(define (make-std-fasdump-format architecture bytes-per-word
          write-word write-untagged-word write-bignum-digit write-float)
  (make-fasdump-format
   'VERSION             10              ;FASL_VERSION_C_CODE
   'ARCHITECTURE        architecture
   'MARKER              (do ((i 0 (+ i 1)) ;#xfafafa...
                             (m 0 (replace-bit-field 8 (* 8 i) m #xfa)))
                            ((>= i bytes-per-word) m))
   'BITS-PER-TYPE       6
   'BITS-PER-DATUM      (- (* bytes-per-word 8) 6)
   'BITS-PER-BYTE       8
   'BYTES-PER-WORD      bytes-per-word
   'WORDS-PER-FLOAT     (/ 8 bytes-per-word)
   'FLOAT-ALIGN-WORDS   (/ 8 bytes-per-word) ;XXX may want stricter
   'BITS-PER-BIGNUM-DIGIT       (- (* 8 bytes-per-word) 2)
   'WORDS-PER-BIGNUM-DIGIT      1
   'GREATEST-FIXNUM     (bit-mask (* bytes-per-word 8) 0)
   'LEAST-FIXNUM        (- -1 (bit-mask (* bytes-per-word 8) 0))
   'WRITE-WORD          write-word
   'WRITE-UNTAGGED-WORD write-untagged-word
   'WRITE-BIGNUM-DIGIT  write-bignum-digit
   'WRITE-FLOAT         write-float))

(define (make-std32be-fasdump-format architecture)
  (make-std-fasdump-format architecture 4
                           write-std32be-word
                           write-std32be-untagged-word
                           write-std32be-bignum-digit
                           write-ieee754-binary64-be))

(define (make-std32le-fasdump-format architecture)
  (make-std-fasdump-format architecture 4
                           write-std32le-word
                           write-std32le-untagged-word
                           write-std32le-bignum-digit
                           write-ieee754-binary64-le))

(define (make-std64be-fasdump-format architecture)
  (make-std-fasdump-format architecture 8
                           write-std64be-word
                           write-std64be-untagged-word
                           write-std64be-bignum-digit
                           write-ieee754-binary64-be))

(define (make-std64le-fasdump-format architecture)
  (make-std-fasdump-format architecture 8
                           write-std64le-word
                           write-std64le-untagged-word
                           write-std64le-bignum-digit
                           write-ieee754-binary64-le))

;;;; Bits

(define (write-std32be-word type datum output-port)
  (write-std32-word type datum write-be-halves output-port))

(define (write-std32le-word type datum output-port)
  (write-std32-word type datum write-le-halves output-port))

(define (write-std64be-word type datum output-port)
  (write-std64-word type datum write-be-halves output-port))

(define (write-std64le-word type datum output-port)
  (write-std64-word type datum write-le-halves output-port))

(define (write-std32-word type datum write-halves output-port)
  (assert (<= 0 type #x3f))
  (assert (zero? (shiftout datum #xfc000000)))
  (let ((high (shiftout datum #x03ff0000))
        (low  (shiftout datum #x0000ffff)))
    (let ((high
           (bitwise-ior (shiftin type #xfc00)
                        (shiftin high #x03ff))))
      (write-halves write-halves write-16 low high output-port))))

(define (write-std64-word type datum write-halves output-port)
  (assert (<= 0 type #x3f))
  (assert (zero? (shiftout datum #xfc00000000000000)))
  (let ((high (shiftout datum #x03ffffff00000000))
        (low  (shiftout datum #x00000000ffffffff)))
    (let ((high
           (bitwise-ior (shiftin type #xfc000000)
                        (shiftin high #x03ffffff))))
      (write-halves write-halves write-32 low high output-port))))

(define (write-std32le-untagged-word word output-port)
  (write-32 write-le-halves word output-port))

(define (write-std32be-untagged-word word output-port)
  (write-32 write-be-halves word output-port))

(define (write-std64le-untagged-word word output-port)
  (write-64 write-le-halves word output-port))

(define (write-std64be-untagged-word word output-port)
  (write-64 write-be-halves word output-port))

(define (write-std32le-bignum-digit digit output-port)
  (write-32 write-le-halves digit output-port))

(define (write-std32be-bignum-digit digit output-port)
  (write-32 write-be-halves digit output-port))

(define (write-std64le-bignum-digit digit output-port)
  (write-64 write-le-halves digit output-port))

(define (write-std64be-bignum-digit digit output-port)
  (write-64 write-be-halves digit output-port))

(define (write-halves* write-halves write-half bits n output-port)
  (assert (< 0 bits))
  (assert (= n (extract-bit-field (* 2 bits) 0 n)))
  (let ((low (extract-bit-field bits 0 n))
        (high (extract-bit-field bits bits n)))
    (write-halves write-halves write-half low high output-port)))

(define (write-le-halves write-halves write-half low high output-port)
  (write-half write-halves low output-port)
  (write-half write-halves high output-port))

(define (write-be-halves write-halves write-half low high output-port)
  (write-half write-halves high output-port)
  (write-half write-halves low output-port))

(define (write-64 write-halves n output-port)
  (write-halves* write-halves write-32 32 n output-port))

(define (write-32 write-halves n output-port)
  (write-halves* write-halves write-16 16 n output-port))

(define (write-16 write-halves n output-port)
  (write-halves* write-halves write-8 8 n output-port))

(define (write-8 write-halves n output-port)
  write-halves                          ;ignore
  (write-octet n output-port))

(define (write-octet octet output-port)
  (write-u8 octet output-port))

(define (write-ieee754-binary64-be x output-port)
  (write-ieee754-binary64 write-be-halves x output-port))

(define (write-ieee754-binary64-le x output-port)
  (write-ieee754-binary64 write-le-halves x output-port))

(define (write-ieee754-binary64 write-halves x output-port)
  (receive (sign biased-exponent trailing-significand)
           (decompose-ieee754-binary64 x)
    (let ((low  (shiftout trailing-significand #x00000000ffffffff))
          (high (shiftout trailing-significand #x000fffff00000000)))
      (let* ((sign&exponent
              (bitwise-ior (shiftin sign            #x80000000)
                           (shiftin biased-exponent #x7ff00000)))
             (high
              (bitwise-ior sign&exponent
                           (shiftin high            #x000fffff))))
        (write-halves write-halves write-32 low high output-port)))))

;;;;; Known formats

(define fasdump-format:i386     (make-std32le-fasdump-format  6))
(define fasdump-format:sparc32  (make-std32le-fasdump-format 14))
(define fasdump-format:mips32be (make-std32be-fasdump-format 15))
(define fasdump-format:mips32le (make-std32le-fasdump-format 15))
(define fasdump-format:alpha    (make-std64le-fasdump-format 18))
(define fasdump-format:ppc32    (make-std32be-fasdump-format 20))
(define fasdump-format:amd64    (make-std64le-fasdump-format 21))
(define fasdump-format:arm32    (make-std32le-fasdump-format 24))

#;
(define fasdump-format:pdp10
  (make-fasdump-format
   'VERSION             10              ;FASL_VERSION_C_CODE
   'ARCHITECTURE        1
   'BITS-PER-TYPE       6
   'BITS-PER-DATUM      30
   'BITS-PER-BYTE       36
   'BYTES-PER-WORD      1
   'WORDS-PER-FLOAT     42              ;XXX
   'FLOAT-ALIGN-WORDS   ???
   'BITS-PER-BIGNUM-DIGIT       18      ;XXX
   'WORDS-PER-BIGNUM-DIGIT      1/2     ;XXX
   'GREATEST-FIXNUM     #x1fffffff
   'LEAST-FIXNUM        #x-20000000
   'WRITE-WORD          write-pdp10-word
   'WRITE-BIGNUM-DIGIT  write-pdp10-bignum-digit
   'WRITE-FLOAT         write-pdp10-float))

;;;; Fasdump top-level

(define-structure (state
                   (safe-accessors)
                   (conc-name state.)
                   (constructor make-state (format output-port)))
  (format #f read-only #t)
  (output-port #f read-only #t)
  (n-words 1)                           ;Always one object at the start.
  (addresses (make-strong-eq-hash-table) read-only #t)
  (primitive-name->number (make-string-hash-table) read-only #t)
  (primitives-reversed '())
  (queue (make-queue) read-only #t))

(define (fasdump-error state message . irritants)
  ;; XXX
  state
  (apply error message irritants))

(define (portable-fasdump object pathname format)
  (let ((temporary
         (let ((root (string-append (->namestring pathname) ".tmp")))
           (let loop ((i 0))
             (if (> i 100)
                 (error "Unable to allocate temporary file!"))
             (let ((temporary (string-append root (number->string i))))
               (if (allocate-temporary-file temporary)
                   temporary
                   (loop (+ i 1))))))))
    (dynamic-wind
     (let ((done? #f))
       (lambda ()
         (if done? (error "Re-entry into fasdump not allowed!"))))
     (lambda ()
       (call-with-binary-output-file temporary
         (lambda (output-port)
           (let ((state (make-state format output-port)))
             (set-port-position! output-port
                                 (* fasl-header-n-words
                                    (format.bytes-per-word format)))
             (assert (fasdump-at-address? state 0))
             (fasdump-object state object)
             (assert (fasdump-at-address? state (format.bytes-per-word format)))
             (do () ((queue-empty? (state.queue state)))
               (let ((object.n-words (dequeue! (state.queue state))))
                 (let ((object (car object.n-words))
                       (n-words (cdr object.n-words)))
                   (with-fasdump-words state n-words
                     (lambda ()
                       (fasdump-storage state object))))))
             (fasdump-primitive-table state)
             (fasdump-header state))))
       (rename-file temporary pathname))
     (lambda ()
       (deallocate-temporary-file temporary)))))

(define (fasdump-primitive-table state)
  (for-each (lambda (primitive)
              (fasdump-primitive-table-entry state primitive))
            (reverse (state.primitives-reversed state))))

(define (fasdump-primitive-table-entry state primitive)
  (let ((name (car primitive))
        (arity (cdr primitive)))
    (let ((n-words (fasdump-legacy-string-n-words (state.format state) name)))
      (fasdump-word state tc:fixnum (fixnum->datum (state.format state) arity))
      ;; One word for number of bytes, one word for content.
      (fasdump-word state tc:manifest-nm-vector (+ 1 n-words))
      (fasdump-word state 0 (string-length name))
      (fasdump-legacy-string state name))))

(define (count-primitive-table-entries state)
  (length (state.primitives-reversed state)))

(define (count-primitive-table-words state)
  (define (count-words entry)
    ;; One word for arity, one word for manifest-nm-vector, one word
    ;; for number of bytes.
    (+ 3 (fasdump-legacy-string-n-words (state.format state) (car entry))))
  (reduce + 0 (map count-words (state.primitives-reversed state))))

(define (fixnum->datum format fixnum)
  (signed->unsigned (format.bits-per-datum format) fixnum))

(define fasl-header-n-words 50)

(define (fasdump-header state)
  (let ((version (format.version (state.format state)))
        (architecture (format.architecture (state.format state)))
        (marker (format.marker (state.format state)))
        (bits-per-type (format.bits-per-type (state.format state)))
        (bits-per-byte (format.bits-per-byte (state.format state)))
        (bytes-per-word (format.bytes-per-word (state.format state)))
        (write-word (format.write-word (state.format state)))
        (write-untagged-word (format.write-untagged-word (state.format state)))
        (output-port (state.output-port state)))
    (define (tagged type datum)
      (write-word type datum output-port))
    (define (untagged word)
      (write-untagged-word word output-port))
    (set-port-position! output-port 0)
    (untagged marker)                   ;0 fasl-marker
    (tagged tc:broken-heart             ;1 heap size in words
            (state.n-words state))
    (tagged tc:broken-heart 0)          ;2 heap start address
    (tagged tc:false 0)                 ;3 dumped object address
    (tagged tc:broken-heart 0)          ;4 constant size in words
    (tagged tc:broken-heart 0)          ;5 constant start address
    (tagged 1                           ;6 fasl format version/architecture
            (let* ((a (/ (* bits-per-byte bytes-per-word) 2))
                   (v (- a bits-per-type)))
              (bitwise-ior (replace-bit-field v a 0 version)
                           (replace-bit-field a 0 0 architecture))))
    (tagged tc:broken-heart 0)          ;7 stack start address
    (tagged tc:broken-heart             ;8 no. of entries in primitive table
            (count-primitive-table-entries state))
    (tagged tc:broken-heart             ;9 no. of words in primitive table
            (count-primitive-table-words state))
    (tagged tc:false 0)                 ;10 compiled interface version (0, 0)
    (tagged tc:false 0)                 ;11 compiled utilities address (#f)
    (untagged 0)                        ;12 header and data checksum (not yet?)
    (tagged tc:broken-heart 0)          ;13 no. of entries in C code table
    (tagged tc:broken-heart 0)          ;14 no. of words in C code table
    (untagged 0)                        ;15 memory base
    (tagged tc:broken-heart 0)          ;16 stack size
    (untagged 0)                        ;17 bytes of heap reserved
    (untagged 0)                        ;18 no. of ephemerons in fasl
    (assert (<= 19 fasl-header-n-words))
    (do ((i 19 (+ i 1))) ((>= i fasl-header-n-words))
      (untagged 0))
    (assert
     (= (* fasl-header-n-words (format.bytes-per-word (state.format state)))
        (port-position output-port)))))

(define (with-fasdump-words state n-words procedure)
  (let ((format (state.format state))
        (output-port (state.output-port state)))
    (let ((bytes-per-word (format.bytes-per-word format))
          (before (port-position output-port)))
      (begin0 (procedure)
        (let ((after (port-position output-port)))
          (assert (= (- after before) (* n-words bytes-per-word))
            `(n-words ,n-words)
            `(n-bytes ,(* n-words bytes-per-word))
            `(before ,before)
            `(after ,after))
          ;; Make sure it stays around in case we enter the debugger.
          (assert (reference-barrier procedure)))))))

(define (fasdump-word state type datum)
  (let ((format (state.format state)))
    (assert (<= 0 type (bit-mask (format.bits-per-type format) 0)))
    (assert (<= 0 datum (bit-mask (format.bits-per-datum format) 0)))
    ((format.write-word format) type datum (state.output-port state))))

(define (fasdump-align state overhead alignment)
  (let* ((unaligned-address (fasdump-address state))
         (aligned-address (round-up (+ unaligned-address overhead) alignment))
         (n-words (- aligned-address (+ unaligned-address overhead))))
    (with-fasdump-words state n-words
      (lambda ()
        (do ((i 0 (+ i 1))) ((>= i n-words))
          (fasdump-word state tc:false 0))))))

(define (fasdump-float state value)
  (let ((format (state.format state)))
    ((format.write-float format) value (state.output-port state))))

(define (fasdump-legacy-string-n-words format string)
  (let ((n-cps (string-length string)))
    (do ((i 0 (+ i 1)))
        ((>= i n-cps))
      (if (not (<= 0 (char->integer (string-ref string i)) 255))
          (error "Non-byte string:" string)))
    (let ((n-bytes n-cps))
      ;; Add a terminating null byte.
      (quotient (+ 1 n-bytes (- (format.bytes-per-word format) 1))
                (format.bytes-per-word format)))))

(define (fasdump-legacy-string state string)
  (let ((format (state.format state))
        (output-port (state.output-port state)))
    (let ((bytes (string-length string))
          (n-words (fasdump-legacy-string-n-words format string))
          (bytes-per-word (format.bytes-per-word format)))
      (with-fasdump-words state n-words
        (lambda ()
          (let ((n-zeros (- (* n-words bytes-per-word) bytes)))
            (do ((i 0 (+ i 1)))
                ((>= i (string-length string)))
              (let ((cp (char->integer (string-ref string i))))
                (assert (<= 0 cp 255))
                (write-octet cp output-port)))
            (do ((i 0 (+ i 1))) ((>= i n-zeros))
              ;; XXX fasdump-byte, not write-octet
              (write-octet 0 output-port)))))
      (assert (zero? (modulo (port-position output-port) bytes-per-word))))))

(define (fasdump-bytevector-n-words format bytevector)
  (let ((n-bytes (bytevector-length bytevector)))
    (quotient (+ n-bytes (- (format.bytes-per-word format) 1))
              (format.bytes-per-word format))))

(define (fasdump-bytevector state bytevector)
  (let ((format (state.format state))
        (output-port (state.output-port state)))
    (let ((bytes (bytevector-length bytevector))
          (n-words (fasdump-bytevector-n-words format bytevector))
          (bytes-per-word (format.bytes-per-word format)))
      (with-fasdump-words state n-words
        (lambda ()
          (let ((n-zeros (- (* n-words bytes-per-word) bytes)))
            (do ((i 0 (+ i 1)))
                ((>= i (bytevector-length bytevector)))
              (write-octet (bytevector-u8-ref bytevector i) output-port))
            (do ((i 0 (+ i 1))) ((>= i n-zeros))
              ;; XXX fasdump-byte, not write-octet
              (write-octet 0 output-port)))))
      (assert (zero? (modulo (port-position output-port) bytes-per-word))))))

(define (fasdump-ustring-n-words format string)
  (let ((n-cps (string-length string))
        (bpc (max-bytes-per-cp string)))
    (let ((n-bytes (* n-cps bpc)))
      ;; Add a terminating null byte.
      (quotient (+ 1 n-bytes (- (format.bytes-per-word format) 1))
                (format.bytes-per-word format)))))

(define (fasdump-ustring-flags format string)
  format string
  1)

(define (max-bytes-per-cp string)
  (define (cp-bytes cp)
    (let ((bytes (quotient (+ (integer-length cp) 7) 8)))
      (assert (<= 0 bytes 3))
      bytes))
  (do ((i 0 (+ i 1))
       (bpc 1 (max bpc (cp-bytes (char->integer (string-ref string i))))))
      ((>= i (string-length string))
       bpc)))

(define (fasdump-ustring state string)
  ;; XXX cop-out
  (assert (= 1 (max-bytes-per-cp string)))
  (fasdump-legacy-string state string))

(define (fasdump-bit-string-n-words format bit-string)
  (let ((bits-per-byte (format.bits-per-byte format))
        (bytes-per-word (format.bytes-per-word format)))
    (let ((bits-per-word (* bits-per-byte bytes-per-word)))
      (quotient (+ (bit-string-length bit-string) (- bits-per-word 1))
                bits-per-word))))

(define (fasdump-bit-string state bit-string)
  (let ((format (state.format state))
        (port (state.output-port state))
        (n (bit-string-length bit-string)))
    (let ((write-untagged-word (format.write-untagged-word format))
          (bits-per-byte (format.bits-per-byte format))
          (bytes-per-word (format.bytes-per-word format)))
      (let ((bits-per-word (* bits-per-byte bytes-per-word)))
        (with-fasdump-words state
            (fasdump-bit-string-n-words format bit-string)
          (lambda ()
            (let loop ((i 0))
              (if (< i n)
                  (let ((i* (min n (+ i bits-per-word)))
                        (word (make-bit-string bits-per-word #f)))
                    (bit-substring-move-right! bit-string i i* word 0)
                    (let ((integer (bit-string->unsigned-integer word)))
                      (write-untagged-word integer port))
                    (loop i*))))))))))

(define (fasdump-bignum-n-digits format integer)
  (assert (exact-integer? integer))
  (let ((bits-per-digit (format.bits-per-bignum-digit format)))
    (let loop ((magnitude (abs integer)) (digits 0))
      (if (zero? magnitude)
          digits
          (loop (shift-right magnitude bits-per-digit) (+ digits 1))))))

(define (fasdump-bignum-n-words format integer)
  (assert (exact-integer? integer))
  (* (format.words-per-bignum-digit format)
     ;; Add one for the header.
     (+ 1 (fasdump-bignum-n-digits format integer))))

(define (fasdump-bignum-digit state digit)
  (let ((format (state.format state)))
    ((format.write-bignum-digit format) digit (state.output-port state))))

(define (fasdump-bignum state integer)
  (let ((format (state.format state)))
    (let ((n-digits (fasdump-bignum-n-digits format integer))
          (shift (format.bits-per-bignum-digit format)))
      (with-fasdump-words state (fasdump-bignum-n-words format integer)
        (lambda ()
          (let ((mask (bit-mask shift 0)))
            (assert (<= 0 n-digits))
            (assert (= n-digits (bitwise-and n-digits mask)))
            (let ((sign (if (< integer 0) 1 0))
                  (magnitude (abs integer)))
              (let ((header (replace-bit-field 1 shift n-digits sign)))
                (fasdump-bignum-digit state header)
                (let loop ((magnitude magnitude) (digits 0))
                  (if (zero? magnitude)
                      (assert (= digits n-digits))
                      (let ((digit (bitwise-and magnitude mask)))
                        (fasdump-bignum-digit state digit)
                        (loop (shift-right magnitude shift)
                              (+ digits 1)))))))))))))

;;;; Fasdumping an object

(define (fasdump-object state object)
  (receive (type datum) (fasdump-encode-object state object)
    (fasdump-word state type datum)))

(define (fasdump-encode-object state object)
  (fasdump-classify state object
    (lambda (type datum)                ;if-non-pointer
      (values type datum))
    (lambda (type name arity)           ;if-primitive
      (values type (get-primitive-number state name arity)))
    (lambda (type n-words)              ;if-pointer
      (values type (get-object-address state object n-words 0 1)))
    (lambda (type n-words overhead alignment) ;if-aligned-pointer
      (values type
              (get-object-address state object n-words overhead alignment)))))

(define (get-primitive-number state name arity)
  (let* ((primitive-name->number (state.primitive-name->number state))
         (n (hash-table-size primitive-name->number)))
    (hash-table-intern! primitive-name->number name
      (lambda ()
        (set-state.primitives-reversed!
         state
         (cons (cons name arity) (state.primitives-reversed state)))
        n))))

(define (get-object-address state object n-words overhead alignment)
  (hash-table-intern! (state.addresses state) object
    (lambda ()
      (let* ((unaligned-address (state.n-words state))
             (aligned-address
              (round-up (+ unaligned-address overhead) alignment))
             (n-padding-words
              (- aligned-address (+ unaligned-address overhead))))
        (set-state.n-words! state (+ aligned-address n-words))
        (do ((i 0 (+ i 1))) ((>= i n-padding-words))
          (enqueue! (state.queue state) (cons #f 1)))
        (enqueue! (state.queue state) (cons object (+ overhead n-words)))
        (* (- aligned-address overhead)
           (format.bytes-per-word (state.format state)))))))

(define (fasdump-address state)
  (- (port-position (state.output-port state))
     (* fasl-header-n-words
        (format.bytes-per-word (state.format state)))))

(define (fasdump-at-address? state address)
  (= (fasdump-address state) address))

;;;;; Object classification

(define (fasdump-classify state object
          if-non-pointer if-primitive if-pointer if-aligned-pointer)
  (let ((format (state.format state)))
    (cond ((pair? object) (if-pointer tc:list 2))
          ((vector? object)
           (if-pointer tc:vector (+ 1 (vector-length object))))
          ((string? object)
           (if-pointer tc:unicode-string
                       ;; manifest, length/flags
                       (+ 1 1 (fasdump-ustring-n-words format object))))
          ((bytevector? object)
           (if-pointer tc:bytevector
                       ;; manifest, length
                       (+ 1 1 (fasdump-bytevector-n-words format object))))
          ((bit-string? object)
           (if-pointer tc:bit-string
                       ;; One for the real length, one for the manifest.
                       (+ 2 (fasdump-bit-string-n-words format object))))
          ((symbol? object)
           (let ((type
                  (if (uninterned-symbol? object)
                      tc:uninterned-symbol
                      tc:interned-symbol)))
             (if-pointer type 2)))
          ((primitive-procedure? object)
           (if-primitive tc:primitive
                         (symbol->string (primitive-procedure-name object))
                         (primitive-procedure-arity object)))
          ((reference-trap? object)
           (let ((kind (reference-trap-kind object)))
             (if (<= kind trap-max-immediate)
                 (if-non-pointer tc:reference-trap kind)
                 (if-pointer tc:reference-trap 2))))
          ((interpreter-return-address? object)
           (if-non-pointer tc:return-code (return-address/code object)))
          ((number? object)
           (fasdump-classify/number state object
             if-non-pointer if-pointer if-aligned-pointer))
          ((scode? object)
           (fasdump-classify/scode state object if-pointer if-non-pointer))
          ((char? object) (if-non-pointer tc:character (char->integer object)))
          ((eqv? object #f) (if-non-pointer tc:false false:false))
          ((eqv? object #t) (if-non-pointer tc:constant constant:true))
          ((eqv? object (aux-object))
           (if-non-pointer tc:constant constant:aux))
          ((eqv? object (default-object))
           (if-non-pointer tc:constant constant:default))
          ((eqv? object (eof-object))
           (if-non-pointer tc:constant constant:eof))
          ((eqv? object (key-object))
           (if-non-pointer tc:constant constant:key))
          ((eqv? object (eof-object))
           (if-non-pointer tc:constant constant:eof))
          ((eqv? object (optional-object))
           (if-non-pointer tc:constant constant:optional))
          ((eqv? object (rest-object))
           (if-non-pointer tc:constant constant:rest))
          ((eqv? object (unspecific-object))
           (if-non-pointer tc:constant constant:unspecific))
          ((null? object)
           (if-non-pointer tc:constant constant:null))
          (else
           (fasdump-error state "Invalid object for fasdump:" object)))))

(define (fasdump-classify/number state object
          if-non-pointer if-pointer if-aligned-pointer)
  (let ((format (state.format state)))
    (cond ((exact-integer? object)
           (if (and (<= (format.least-fixnum format) object)
                    (<= object (format.greatest-fixnum format)))
               (if-non-pointer tc:fixnum (fixnum->datum format object))
               (if-pointer tc:big-fixnum
                           (+ 1 (fasdump-bignum-n-words format object)))))
          ((exact-rational? object) (if-pointer tc:ratnum 2))
          ((inexact-real? object)
           (let ((words-per-float (format.words-per-float format))
                 (float-align-words (format.float-align-words format)))
             (if-aligned-pointer tc:big-flonum
                                 words-per-float
                                 1
                                 float-align-words)))
          ((complex? object) (if-pointer tc:complex 2))
          (else
           (fasdump-error state "Invalid number for fasdump:" object)))))

;;;;;; Scode classification

(define (fasdump-classify/scode state scode if-pointer if-non-pointer)
  (cond ((scode-access? scode) (if-pointer tc:access 2))
        ((scode-assignment? scode) (if-pointer tc:assignment 2))
        ((scode-combination? scode)
         (if-pointer tc:combination
                     ;; One for the manifest; one for the operator.
                     (+ 2 (length (scode-combination-operands scode)))))
        ((scode-comment? scode) (if-pointer tc:comment 2))
        ((scode-conditional? scode) (if-pointer tc:conditional 3))
        ((scode-definition? scode) (if-pointer tc:definition 2))
        ((scode-delay? scode) (if-pointer tc:delay 1))
        ((scode-disjunction? scode) (if-pointer tc:disjunction 2))
        ((scode-lambda? scode)
         (fasdump-classify/lambda state scode if-pointer))
        ((scode-quotation? scode) (if-pointer tc:scode-quote 1))
        ((scode-sequence? scode) (if-pointer tc:sequence 2))
        ((scode-the-environment? scode) (if-non-pointer tc:the-environment 0))
        ((scode-variable? scode) (if-pointer tc:variable 3))
        (else (error "This is not scode!" scode))))

(define (fasdump-classify/lambda state scode if-pointer)
  (lambda-components* scode
    (lambda (name required optional rest body)
      name body                         ;ignore
      (if (or (pair? optional) rest)
          (begin
            (if (not (and (length<=? required #xff)
                          (length<=? optional #xff)))
                (fasdump-error state "Lambda too large!" scode))
            (if-pointer tc:extended-lambda 3))
          (if-pointer tc:lambda 2)))))

;;;; Fasdumping a pointer object's storage

(define (fasdump-storage state object)
  (assert (let ((address
                 (or (hash-table-ref/default (state.addresses state) object #f)
                     (error "Unallocated queued object:" object))))
            (fasdump-at-address? state address))
    `(object ,object)
    `(object address
             ,(hash-table-ref/default (state.addresses state) object #f))
    `(current address ,(fasdump-address state)))
  (let ((format (state.format state)))
    (cond ((pair? object)
           (fasdump-object state (car object))
           (fasdump-object state (cdr object)))
          ((vector? object)
           (fasdump-word state tc:manifest-vector (vector-length object))
           (with-fasdump-words state (vector-length object)
             (lambda ()
               (do ((i 0 (+ i 1))) ((>= i (vector-length object)))
                 (let ((element
                        (map-reference-trap
                         (lambda () (vector-ref object i)))))
                   (fasdump-object state element))))))
          ((string? object)
           (let ((n-words (fasdump-ustring-n-words format object))
                 (flags (fasdump-ustring-flags format object)))
             ;; One word for number of bytes, one word for content.
             (fasdump-word state tc:manifest-nm-vector (+ 1 n-words))
             (with-fasdump-words state (+ 1 n-words)
               (lambda ()
                 (fasdump-word state flags (string-length object))
                 (fasdump-ustring state object)))))
          ((bytevector? object)
           (let ((n-words (fasdump-bytevector-n-words format object)))
             (fasdump-word state tc:manifest-nm-vector (+ 1 n-words))
             (with-fasdump-words state (+ 1 n-words)
               (lambda ()
                 (fasdump-word state 0 (bytevector-length object))
                 (fasdump-bytevector state object)))))
          ((bit-string? object)
           (let ((n-words (fasdump-bit-string-n-words format object)))
             ;; One word for number of bits, one word for content.
             (fasdump-word state tc:manifest-nm-vector (+ 1 n-words))
             (with-fasdump-words state (+ 1 n-words)
               (lambda ()
                 (fasdump-word state 0 (bit-string-length object))
                 (fasdump-bit-string state object)))))
          ((symbol? object)
           (with-fasdump-words state 2
             (lambda ()
               (fasdump-object state (string->utf8 (symbol->string object)))
               (if (uninterned-symbol? object)
                   (fasdump-word state tc:reference-trap trap:unbound)
                   ;; Fasloader uses this to distinguish interned
                   ;; symbols from uninterned ones.
                   (fasdump-word state tc:broken-heart 0)))))
          ((reference-trap? object)
           (assert (> (reference-trap-kind object) trap-max-immediate))
           (with-fasdump-words state 2
             (lambda ()
               (fasdump-object state (reference-trap-kind object))
               (fasdump-object state (reference-trap-extra object)))))
          ((number? object)
           (fasdump-storage/number state object))
          ((scode? object)
           (fasdump-storage/scode state object))
          ((eqv? object #f)             ;XXX Alignment kludge...
           (fasdump-word state tc:false 0))
          (else
           (error "Fasdump bug -- object should have been rejected:"
                  object)))))

(define (fasdump-storage/number state object)
  (let ((format (state.format state)))
    (cond ((exact-integer? object)
           (assert (or (< object (format.least-fixnum format))
                       (< (format.greatest-fixnum format) object)))
           (let ((n-words (fasdump-bignum-n-words format object)))
             (fasdump-word state tc:manifest-nm-vector n-words)
             (with-fasdump-words state n-words
               (lambda ()
                 (fasdump-bignum state object)))))
          ((exact-rational? object)
           (with-fasdump-words state 2
             (lambda ()
               (fasdump-object state (numerator object))
               (fasdump-object state (denominator object)))))
          ((inexact-real? object)
           (let ((words-per-float (format.words-per-float format)))
             (fasdump-align state 1 words-per-float)
             (fasdump-word state tc:manifest-nm-vector words-per-float)
             (with-fasdump-words state words-per-float
               (lambda ()
                 (fasdump-float state object)))))
          ((complex? object)
           (with-fasdump-words state 2
             (lambda ()
               (fasdump-object state (real-part object))
               (fasdump-object state (imag-part object)))))
          (else
           (error "Fasdump bug -- number should have been rejected:"
                  object)))))

;;;;; Fasdumping an scode pointer's storage

(define (fasdump-storage/scode state scode)
  (cond ((scode-access? scode)
         (with-fasdump-words state 2
           (lambda ()
             (fasdump-object state (scode-access-environment scode))
             (fasdump-object state (scode-access-name scode)))))
        ((scode-assignment? scode)
         (with-fasdump-words state 2
           (lambda ()
             (fasdump-object state
                             (make-scode-variable
                              (scode-assignment-name scode)))
             (fasdump-object state (scode-assignment-value scode)))))
        ((scode-combination? scode)
         (let* ((operands (scode-combination-operands scode))
                (n-words (+ 1 (length operands))))
           (fasdump-word state tc:manifest-vector n-words)
           (with-fasdump-words state n-words
             (lambda ()
               (fasdump-object state (scode-combination-operator scode))
               (for-each (lambda (operand)
                           (fasdump-object state operand))
                         operands)))))
        ((scode-comment? scode)
         (with-fasdump-words state 2
           (lambda ()
             (fasdump-object state (scode-comment-expression scode))
             (fasdump-object state (scode-comment-text scode)))))
        ((scode-conditional? scode)
         (with-fasdump-words state 3
           (lambda ()
             (fasdump-object state (scode-conditional-predicate scode))
             (fasdump-object state (scode-conditional-consequent scode))
             (fasdump-object state (scode-conditional-alternative scode)))))
        ((scode-definition? scode)
         (with-fasdump-words state 2
           (lambda ()
             (fasdump-object state (scode-definition-name scode))
             (fasdump-object state (scode-definition-value scode)))))
        ((scode-delay? scode)
         (with-fasdump-words state 1
           (lambda ()
             (fasdump-object state (scode-delay-expression scode)))))
        ((scode-disjunction? scode)
         (with-fasdump-words state 2
           (lambda ()
             (fasdump-object state (scode-disjunction-predicate scode))
             (fasdump-object state (scode-disjunction-alternative scode)))))
        ((scode-lambda? scode)
         (scode-lambda-components scode
           (lambda (name required optional rest aux decls body)
             (let* ((body
                     (if (pair? decls)
                         (make-scode-sequence
                          (list (make-scode-block-declaration decls)
                                body))
                         body))
                    (body (make-auxiliary-lambda aux body)))
               (if (or (pair? optional) rest)
                   (fasdump-xlambda state name required optional rest body)
                   (fasdump-lambda state name required body))))))
        ((scode-quotation? scode)
         (with-fasdump-words state 1
           (lambda ()
             (fasdump-object state (scode-quotation-expression scode)))))
        ((scode-sequence? scode)
         (with-fasdump-words state 2
           (lambda ()
             (let ((actions (scode-sequence-actions scode)))
               (assert (not (length<=? actions 1)))
               (fasdump-object state (car actions))
               (fasdump-object state
                               (if (length<=? actions 2)
                                   (cadr actions)
                                   (make-scode-sequence (cdr actions))))))))
        ((scode-variable? scode)
         (with-fasdump-words state 3
           (lambda ()
             (fasdump-object state (scode-variable-name scode))
             ;; XXX Hysterical raisins...
             (fasdump-object state #t)
             (fasdump-object state '()))))
        (else
         (error "Fasdump bug -- this is not scode!" scode))))

(define (fasdump-lambda state name required body)
  (with-fasdump-words state 2
    (lambda ()
      (fasdump-object state body)
      (fasdump-object state (list->vector (cons name required))))))

(define (fasdump-xlambda state name required optional rest body)
  (with-fasdump-words state 3
    (lambda ()
      (assert (length<=? required #xff))
      (assert (length<=? optional #xff))
      (let ((variables
             (cons name (append required optional (if rest (list rest) '()))))
            (arity
             (encode-xlambda-arity (length required)
                                   (length optional)
                                   (if rest #t #f))))
        (fasdump-object state body)
        (fasdump-object state (list->vector variables))
        (fasdump-word state tc:fixnum arity)))))

(define (encode-xlambda-arity n-required n-optional rest?)
  (assert (<= 0 n-required #xff))
  (assert (<= 0 n-optional #xff))
  (let ((a (shiftin (if rest? 1 0)      #x10000))
        (b (shiftin n-required          #x0ff00))
        (c (shiftin n-optional          #x000ff)))
    (bitwise-ior a (bitwise-ior b c))))

(define lambda-tag:internal-lambda '|#[internal-lambda]|)

(define (make-auxiliary-lambda auxiliaries body)
  (if (not (pair? auxiliaries))
      body
      (make-scode-combination
       ;; NOTE: The list of auxiliaries must be empty here to avoid
       ;; infinite recursion!
       (let ((name lambda-tag:internal-lambda)
             (required auxiliaries)
             (optional '())
             (rest #f)
             (aux '())
             (decls '())
             (body body))
         (make-scode-lambda name required optional rest aux decls body))
       (map (lambda (auxiliary)
              auxiliary                 ;ignore
              (make-unassigned-reference-trap))
            auxiliaries))))

;;;; Type codes and other magic numbers

(define tc:access #x1f)
(define tc:assignment #x23)
(define tc:big-fixnum #x0e)
(define tc:big-flonum #x06)
(define tc:bit-string #x2f)
(define tc:broken-heart #x22)
(define tc:bytevector #x33)
(define tc:character #x02)
(define tc:character-string #x1e)
(define tc:combination #x26)
(define tc:comment #x15)
(define tc:complex #x3c)
(define tc:conditional #x34)
(define tc:constant #x08)
(define tc:definition #x21)
(define tc:delay #x11)
(define tc:disjunction #x35)
(define tc:extended-lambda #x14)
(define tc:false #x00)
(define tc:fixnum #x1a)
(define tc:interned-symbol #x1d)
(define tc:lambda #x17)
(define tc:list #x01)                   ;pair
(define tc:manifest-nm-vector #x27)
(define tc:primitive #x18)
(define tc:ratnum #x3a)
(define tc:reference-trap #x32)
(define tc:return-code #x0b)
(define tc:scode-quote #x03)
(define tc:sequence #x19)
(define tc:the-environment #x2d)
(define tc:unicode-string #x1b)
(define tc:uninterned-symbol #x05)
(define tc:variable #x2c)
(define tc:vector #x0a)

(define tc:manifest-vector tc:false)

(define false:false 0)

(define constant:true 0)
(define constant:unspecific 1)
(define constant:optional 3)
(define constant:rest 4)
(define constant:key 5)
(define constant:eof 6)
(define constant:default 7)
(define constant:aux 8)
(define constant:null 9)

(define trap:unbound 2)
(define trap-max-immediate 9)

(define (reference-trap-extra trap)
  (error 'reference-trap-extra trap))

;;;; Utilities

(define (scode? object)
  (or (scode-access? object)
      (scode-assignment? object)
      (scode-combination? object)
      (scode-comment? object)
      (scode-conditional? object)
      (scode-definition? object)
      (scode-delay? object)
      (scode-disjunction? object)
      (scode-lambda? object)
      (scode-quotation? object)
      (scode-sequence? object)
      (scode-the-environment? object)
      (scode-variable? object)))

(define (shiftout n mask)
  (shift-right (bitwise-and n mask) (first-set-bit mask)))

(define (shiftin n mask)
  (replace-bit-field (bit-count mask) (first-set-bit mask) 0 n))

(define (shift-right n bits)
  (assert (>= bits 0))
  (arithmetic-shift n (- 0 bits)))

(define (round-up n alignment)
  (assert (<= 0 n))
  (assert (< 0 alignment))
  (* alignment (quotient (+ n (- alignment 1)) alignment)))

(define (signed->unsigned bits n)
  (bitwise-and n (bit-mask bits 0)))

(define (length<=? list length)
  (let loop ((list list) (length length))
    (cond ((pair? list) (and (> length 0) (loop (cdr list) (- length 1))))
          ((null? list) #t)
          (else (error "Invalid list:" list)))))

(define (truncate->exact x)
  (inexact->exact (truncate x)))

(define (exact-integer? x)
  (and (integer? x)
       (exact? x)))

(define (exact-rational? x)
  (and (rational? x)
       (exact? x)))

(define (inexact-real? x)
  (and (real? x)
       (inexact? x)))

;;; XXX Hurk.

(define (aux-object) #!aux)
(define (default-object) #!default)
(define (eof-object) (call-with-input-string "" read)) ;XXX
(define (key-object) #!key)
(define (optional-object) #!optional)
(define (rest-object) #!rest)
(define (unspecific-object) #!unspecific)

(define (port-position port)
  ((access binary-port-position (->environment '(runtime binary-port))) port))

(define (set-port-position! port position)
  ((access set-binary-port-position! (->environment '(runtime binary-port)))
   port
   position))