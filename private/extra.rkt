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
       (syntax-parse (expand-for-clause #'S #'[(tmp) S])
         [(([(outer-id ...) outer-expr] ...)
           outer-check
           ([loop-id loop-expr] ...)
           pos-guard
           ([(inner-id ...) inner-expr] ...)
           pre-guard
           post-guard
           (loop-arg ...))
          #'[(Id)
             (:do-in
              ([(outer-id ...) outer-expr] ...)
              outer-check
              ([x '()] [post #t] [loop-id loop-expr] ...)
              #t
              ([(Id rst post loop-id ...)
                (if (not (null? x))
                    (values (unsafe-car x)
                            (unsafe-cdr x)
                            post
                            loop-id ...)
                    (if post
                        (let loop ([loop-id loop-id] ...)
                          (if pos-guard
                              (let-values ([(inner-id ...) inner-expr] ...)
                                (if pre-guard
                                    (begin
                                      (unless (list? tmp)
                                        (raise-argument-error 'tmp "list?" tmp))
                                      (if (null? tmp)
                                          (if post-guard
                                              (loop #f loop-arg ...)
                                              (values #f #f #f loop-id ...))
                                          (if post-guard
                                              (values (unsafe-car tmp) (unsafe-cdr tmp)
                                                      #t
                                                      loop-arg ...)
                                              (values (unsafe-car tmp) (unsafe-cdr tmp)
                                                      #f
                                                      loop-id ...))))
                                    (values #f #f #f loop-id ...)))
                              (values #f #f #f loop-id ...)))
                        (values #f #f #f loop-id ...)))])
              rst
              #t
              (rst post loop-id ...))]])])))