;; DO NOT: (declare (usual-integrations))

;;;; (scode-optimizer cross)

(define cross-sf/false-value #F)
(define cross-sf/true-value #T)
(define cross-sf/null-value '())
(define cross-sf/unspecific-value unspecific)

(define cross-sf/constants/false #F)   ; the value of #F when reading a file
(define cross-sf/constants/true #T)    ; not used
(define cross-sf/constants/null '())   ; not used
(define cross-sf/constants/unspecific '())   ; not used

(define cross-sf/bin-pathname-type #f)  ; if not #F, replacement type

;; if #F, typecodes are the same on target system
;; if not #F, a utabmd.scm file decribing new typecodes
(define cross-sf/utab-file #f)
;; Cached fixed-objects-vector as specified by cross-sf/utab-file
(define cross-sf/fov #f)


(define (cross-sf/get-fixed-objects-vector)

  (define (read-utabmd filename)
    ;; `interpret' the utabmd file.  Relies on the very simple format of the
    ;; source.
    (display "\n;; Cross-SF: Typecodes specified by ")
    (display filename)
    (let* ((fov  (make-vector (vector-length (get-fixed-objects-vector)))))
      (with-input-from-file filename
        (lambda ()
          (let loop ()
            (let ((expr (read)))
              (cond ((eof-object? expr)  fov)
                    ((and (pair? expr)
                          (equal? (car expr) 'vector-set!))
                     (vector-set! fov (third expr) (fourth expr))
                     (loop))
                    (else (loop)))))))))

  (if cross-sf/utab-file
      (or cross-sf/fov
          (begin (set! cross-sf/fov (read-utabmd cross-sf/utab-file))
                 cross-sf/fov))
      (get-fixed-objects-vector)))


;;; The following 3 procedures are trivially renamed from
;;; runtime/utabs.scm because GET-FIXED-OBJECTS-VECTOR is an
;;; integrated primitive an thus we cant just fluid-let it.

(define (cross-sf/ucode-type type-name)
  (or (cross-sf/microcode-type/name->code type-name)
      (error "CROSS-SF/MICROCODE-TYPE: Unknown name" type-name)))

(define (cross-sf/microcode-type/name->code name)
  (let ((types-slot (fixed-object/name->code 'MICROCODE-TYPES-VECTOR)))
    (cross-sf/microcode-table-search types-slot name)))

(define (cross-sf/microcode-table-search slot name)
  (let ((vector (vector-ref (cross-sf/get-fixed-objects-vector) slot)))
    (let ((end (vector-length vector)))
      (define (loop i)
        (and (not (= i end))
             (let ((entry (vector-ref vector i)))
               (if (if (pair? entry)
                       (memq name entry)
                       (eq? name entry))
                   i
                   (loop (1+ i))))))
      (loop 0))))


(define (cross-sf/hack-sharp-f-reader!)
  (define (cross-sf/parse-object/false)
    (parse-object/false) 
    cross-sf/false-value)
  (parser-table/set-entry! system-global-parser-table
                           '("#f" "#F")
                           cross-sf/parse-object/false)
  'DONE)


(define (with-cross-sf thunk)
  (cross-sf/hack-sharp-f-reader!)

  ;; Cross-sf parameters:
  (fluid-let ((bin-pathname-type          (or cross-sf/bin-pathname-type
                                              bin-pathname-type))
              (cross-sf/false-value       cross-sf/constants/false)
              (cross-sf/true-value        cross-sf/constants/true)
              (cross-sf/null-value        cross-sf/constants/null)
              (cross-sf/unspecific-value  cross-sf/constants/unspecific)
              (cross-sf/fov               #f) ; clear cache
              (microcode-type             cross-sf/ucode-type)
              )

    ;; Effecting parameters on the system:
    (fluid-let ((usual-integrations/expansion-alist
                 (usual-integrations/make-expansion-alist)))

      (dynamic-wind
       (lambda ()
         ;;; Global integrable bindings dependent upon parameters:
         ;;  It is assumed that these names have all been integrated in any code
         ;;  reachable from USUAL-INTEGRATIONS/CACHE!, so that these
         ;;  redefinitions will not change it's behaviour.

	 (fluid-let ((false                      cross-sf/false-value)
		     (true                       cross-sf/true-value)
		     (unspecific                 cross-sf/unspecific-value)
		     ;; There are assumptions! They should be checked against
		     ;; their definitions in the microcode/runtime.
		     (*the-non-printing-object*  cross-sf/unspecific-value)
		     (the-empty-stream           cross-sf/null-value)
		     (system-global-environment  cross-sf/false-value)
		     )
	   
	   (usual-integrations/cache!)))

       thunk

       (lambda ()
	 ;; undo bindings to global integrable constants
	 (usual-integrations/cache!))))))



