#lang racket/base
(require (for-syntax racket/base syntax/parse
                     syntax/stx syntax/unsafe/for-transform
                     racket/sequence
                     syntax/id-set)
         racket/unsafe/ops)

(provide in-lists)

(define-sequence-syntax in-lists
  (syntax-rules ())
  (λ (stx)
    (syntax-parse stx
      [[(Id:id) (_ S:expr)]
       #:with (next x tmp) (generate-temporaries '(next x tmp))
       (syntax-parse (expand-for-clause #'S #'[(tmp) S])
         [(([(outer-id ...) outer-expr] ...)
           outer-check
           ([loop-id loop-expr] ...)
           pos-guard
           ([(inner-id ...) inner-expr] ...)
           pre-guard
           post-guard
           (loop-arg ...))
          #:do [(define loop-set (for/fold ([b (immutable-bound-id-set (list #'x))])
                                           ([id (in-syntax #'(loop-id ...))])
                                   (bound-id-set-add b id)))]
          #:with (inner-loop-id ...) (filter (λ (id)
                                               (not (bound-id-set-member? loop-set id)))
                                             (syntax->list #'(inner-id ... ...)))
          #:with (falsy ...) (stx-map (λ (_) #'#f) #'(inner-id ... ...))
          #'[(Id)
             (:do-in
              ([(next outer-id ... ...)
                (let-values ([(outer-id ...) outer-expr] ...)
                  outer-check
                  (define (next loop-id ...)
                    (if pos-guard
                        (let-values ([(inner-id ...) inner-expr]
                                     ...)
                          (if pre-guard
                              (begin
                                (unless (list? tmp)
                                  (raise-argument-error
                                   'Id "list?" tmp))
                                (if (null? tmp)
                                    (if post-guard
                                        (next loop-arg ...)
                                        (values #f #f falsy ...))
                                    (values (unsafe-car tmp)
                                            (unsafe-cdr tmp)
                                            inner-id ... ...)))
                              (values #f #f falsy ...)))
                        (values #f #f falsy ...)))
                  
                  (values next outer-id ... ...))])
              (void)
              ([x '()] [loop-id loop-expr] ...
                       [inner-loop-id #f] ...)
              #t
              ([(Id x inner-id ... ...)
                (if (null? x)
                    (next loop-id ...)
                    (values (unsafe-car x) (unsafe-cdr x)
                            inner-id ... ...))])
              x
              (or (not (null? x)) post-guard)
              (x (if (null? x) loop-arg loop-id) ...
                 inner-loop-id ...))]])])))