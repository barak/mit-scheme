;; Test for correctness and idempotency:
(do ((i 0 (fix:+ i 1)))
    ((not (fix:< i 256)))
  (do ((j (fix:+ i 1) (fix:+ j 1)))
      ((not (fix:<= j 256)))
    (let ((cp (make-cp i j)))
      (let ((components (split-cp-by-prefix cp)))
        ;; Test that each component has a well-defined prefix.
        (for-each
         (lambda (cp)
           (let ((low (cp-start cp))
                 (high (fix:- (cp-end cp) 1)))
             (receive (low* high*) (low-bracket low high)
               (if (not (and (fix:= low low*)
                             (fix:= high high*)))
                   (error "Split range's low bracket should be itself:"
                          low high low* high*)))
             (receive (low* high*) (high-bracket low high)
               (if (not (and (fix:= low low*)
                             (fix:= high high*)))
                   (error "Split range's high bracket should be itself:"
                          low high low* high*)))))
         components)
        ;; Test that all the components are adjacent.
        (do ((cps components (cdr cps)))
            ((not (pair? (cdr cps))))
          (if (not (cps-adjacent? (car cps) (cadr cps)))
              (error "Split range has non-adjacent components:"
                     (car cps) (cadr cps))))
        ;; Test that they components merge back to the original.
        (let ((merged (merge-cp-list components)))
          (if (not (and (fix:= 1 (length merged))
                        (equal? cp (car merged))))
              (error "Split range doesn't re-merge correctly:"
                     cp components merged)))))))