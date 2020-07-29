#lang racket/base

(require (for-syntax racket/base syntax/parse racket/list
                     syntax/stx syntax/unsafe/for-transform
                     "helper.rkt"))
(provide in-mapped in-filtered in-filter-mapped
         (for-syntax current-optimize))

(define-for-syntax current-optimize (make-parameter values))

(define-for-syntax (expand stx)  
  (syntax-parse ((current-optimize) stx)
    [[(Id:id ...) ((~literal in-mapped) Proc:expr S:expr ...+)]
     #:with (Temp ...) (generate-temporaries #'(S ...))
     (syntax-parse (compose-single-valued #'(Temp ...) #'(S ...))
       [(([(outer-id ...) outer-expr] ...)
         outer-check
         ([loop-id loop-expr] ...)
         pos-guard
         ([(inner-id ...) inner-expr] ...)
         pre-guard
         post-guard
         (loop-arg ...))
        #:with (falsy ...) (map (λ (_) #'#f)
                                (syntax->list #'(Id ... inner-id ... ...)))
        (for-clause-syntax-protect
         #'[(Id ...)
            (:do-in
             ([(outer-id ...) outer-expr] ... [(proc) Proc])
             outer-check
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
             )])])]
    [[(Id:id ...+) ((~literal in-filtered) Proc S ...+)]
     (syntax-parse (compose-single-valued #'(Id ...) #'(S ...))
       [(([(outer-id ...) outer-expr] ...)
         outer-check
         ([loop-id loop-expr] ...)
         pos-guard
         ([(inner-id ...) inner-expr] ...)
         pre-guard
         post-guard
         (loop-arg ...))
        #:with (inner-loop-id ...) (remove-duplicates (syntax->list #'(inner-id ... ... loop-id ...))
                                                      bound-identifier=?)
        #:with (falsy ...) (stx-map (λ (_) #'#f) #'(inner-loop-id ...))
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

             (void)

             ([loop-id loop-expr] ...)

             #t

             ([(ok inner-loop-id ...) (next loop-id ...)])

             ok

             post-guard

             (loop-arg ...)
             )])])]
    [[(Id:id) ((~literal in-filter-mapped) Proc S ...+)]
     #:with (Tmp ...) (generate-temporaries #'(S ...))
     (syntax-parse (compose-single-valued #'(Tmp ...) #'(S ...))
       [(([(outer-id ...) outer-expr] ...)
         outer-check
         ([loop-id loop-expr] ...)
         pos-guard
         ([(inner-id ...) inner-expr] ...)
         pre-guard
         post-guard
         (loop-arg ...))
        #:with (inner-loop-id ...) (remove-duplicates (syntax->list #'(inner-id ... ... loop-id ...))
                                                      bound-identifier=?)
        #:with (falsy ...) (stx-map (λ (_) #'#f) #'(inner-loop-id ...))
        (for-clause-syntax-protect
         #'[(Id)
            (:do-in
              
             ([(next outer-id ... ...)
               (let-values ([(outer-id ...) outer-expr] ...
                            [(proc) Proc])
                 (define (next loop-id ...)
                   (if pos-guard
                       (let-values ([(inner-id ...) inner-expr] ...)
                         (if pre-guard
                             (let ([Id (proc Tmp ...)])
                               (if Id
                                   (values Id inner-loop-id ...)
                                   (if post-guard
                                       (next loop-arg ...)
                                       (values #f falsy ...))))
                             (values #f falsy ...)))
                       (values #f falsy ...)))
                 outer-check
                 (values next outer-id ... ...))])

             (void)

             ([loop-id loop-expr] ...)

             #t

             ([(Id inner-loop-id ...) (next loop-id ...)])

             Id

             post-guard

             (loop-arg ...)
             )])])]))

(define-sequence-syntax in-mapped
  (syntax-rules ())
  expand)

(define-sequence-syntax in-filtered
  (syntax-rules ())
  expand)

(define-sequence-syntax in-filter-mapped
  (syntax-rules ())
  expand)