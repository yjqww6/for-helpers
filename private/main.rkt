#lang racket/base

(require (for-syntax racket/base syntax/parse racket/list
                     syntax/stx racket/syntax
                     "helper.rkt")
         "common.rkt")
(provide in-mapped in-filtered in-filter&map
         (for-syntax current-optimize))

(define-for-syntax current-optimize (make-parameter values))

(define-for-syntax (expand stx)
  (parameterize ([current-recorded-disappeared-uses '()])
    (syntax-parse ((current-optimize) stx)
      [[(Id:id ...) ((~literal in-mapped) Proc:expr S:expr ...+)]
       #:with (Temp ...) (generate-temporaries #'(S ...))
       #:with (([(outer-id ...) outer-expr] ...)
               outer-check
               ([loop-id loop-expr] ...)
               pos-guard
               ([(inner-id ...) inner-expr] ...)
               pre-guard
               post-guard
               (loop-arg ...))
       (compose-single-valued #'(Temp ...) #'(S ...))
       #:with (falsy ...) (map (λ (_) #'#f)
                               (syntax->list #'(Id ... inner-id ... ...)))
       #:with (Disappeared ...) (or (current-recorded-disappeared-uses) '())
       (for-clause-syntax-protect
        #'[(Id ...)
           (:do-in
            ([(outer-id ...) outer-expr] ... [(proc) Proc])
            (begin (for-disappeared Disappeared ...) outer-check)
            ([loop-id loop-expr] ...)
            pos-guard
            ([(ok Id ... inner-id ... ...)
              (let-values ([(inner-id ...) inner-expr] ...)
                (if pre-guard
                    (let-values ([(Id ...) (proc Temp ...)])
                      (values #t Id ... inner-id ... ...))
                    (values #f falsy ...)))])
            ok
            post-guard
            (loop-arg ...)
            )])]
      [[(Id:id ...+) ((~literal in-filtered) Proc S ...+)]
       #:with (([(outer-id ...) outer-expr] ...)
               outer-check
               ([loop-id loop-expr] ...)
               pos-guard
               ([(inner-id ...) inner-expr] ...)
               pre-guard
               post-guard
               (loop-arg ...))
       (compose-single-valued #'(Id ...) #'(S ...))
       
       #:with (inner-loop-id ...) (remove-duplicates (syntax->list #'(inner-id ... ... loop-id ...))
                                                     bound-identifier=?)
       #:with (falsy ...) (stx-map (λ (_) #'#f) #'(inner-loop-id ...))
       #:with (next) (generate-temporaries '(next))
       #:with (Disappeared ...) (or (current-recorded-disappeared-uses) '())
       (for-clause-syntax-protect
        #'[(Id ...)
           (:do-in
              
            ([(next outer-id ... ...)
              (let-values ([(outer-id ...) outer-expr] ...
                           [(proc) Proc])
                (define (next loop-id ...)
                  (if pos-guard
                      (let-values ([(inner-id ...) inner-expr] ...)
                        (if pre-guard
                            (if (proc Id ...)
                                (values #t inner-loop-id ...)
                                (if post-guard
                                    (next loop-arg ...)
                                    (values #f falsy ...)))
                            (values #f falsy ...)))
                      (values #f falsy ...)))
                outer-check
                (values next outer-id ... ...))])

            (for-disappeared Disappeared ...)

            ([loop-id loop-expr] ...)

            #t

            ([(ok inner-loop-id ...) (next loop-id ...)])

            ok

            post-guard

            (loop-arg ...)
            )])]

      [[(Id:id ...+) ((~literal in-filter&map) Proc S ...+)]
       #:with (Tmp ...) (generate-temporaries #'(S ...))
       #:with (([(outer-id ...) outer-expr] ...)
               outer-check
               ([loop-id loop-expr] ...)
               pos-guard
               ([(inner-id ...) inner-expr] ...)
               pre-guard
               post-guard
               (loop-arg ...))
       (compose-single-valued #'(Tmp ...) #'(S ...))
       
       #:with (inner-loop-id ...) (remove-duplicates (syntax->list #'(Id ... inner-id ... ... loop-id ...))
                                                     bound-identifier=?)
       #:with (falsy ...) (stx-map (λ (_) #'#f) #'(inner-loop-id ...))
       #:with (ok next) (generate-temporaries '(ok next))
       #:with (Disappeared ...) (or (current-recorded-disappeared-uses) '())
       (for-clause-syntax-protect
        #'[(Id ...)
           (:do-in
              
            ([(next outer-id ... ...)
              (let-values ([(outer-id ...) outer-expr] ...
                           [(proc) Proc])
                (define (next loop-id ...)
                  (if pos-guard
                      (let-values ([(inner-id ...) inner-expr] ...)
                        (if pre-guard
                            (let-values ([(ok Id ...) (proc Tmp ...)])
                              (if ok
                                  (values #t inner-loop-id ...)
                                  (if post-guard
                                      (next loop-arg ...)
                                      (values #f falsy ...))))
                            (values #f falsy ...)))
                      (values #f falsy ...)))
                outer-check
                (values next outer-id ... ...))])

            (for-disappeared Disappeared ...)

            ([loop-id loop-expr] ...)

            #t

            ([(done inner-loop-id ...) (next loop-id ...)])

            done

            post-guard

            (loop-arg ...)
            )])]
      )))

(define-sequence-syntax in-mapped
  (syntax-rules ())
  expand)

(define-sequence-syntax in-filtered
  (syntax-rules ())
  expand)

(define-sequence-syntax in-filter&map
  (syntax-rules ())
  expand)