#lang racket/base

(require syntax/unsafe/for-transform
         racket/syntax syntax/stx
         racket/sequence syntax/parse
         (for-template racket/base))

(provide (all-defined-out))

(define (compose-single-valued ids ss)
  (define (group id+ss)
    (syntax-parse id+ss
      [() '()]
      [([Id:id S:expr] [Id* (~literal _)] ... . rest)
       (cons (expand-for-clause #'S #'[(Id Id* ...) S])
             (group #'rest))]))
  (with-syntax* ([(Id ...) ids]
                 [(S ...) ss]
                 [[(([(outer-id ...) outer-expr] ...)
                    outer-check
                    ([loop-id loop-expr] ...)
                    pos-guard
                    ([(inner-id ...) inner-expr] ...)
                    pre-guard
                    post-guard
                    (loop-arg ...))
                   ...]
                  (group #'([Id S] ...))])
    #'(([(outer-id ...) outer-expr] ... ...)
       (begin outer-check ...)
       ([loop-id loop-expr] ... ...)
       (and pos-guard ...)
       ([(inner-id ...) inner-expr] ... ...)
       (and pre-guard ...)
       (and post-guard ...)
       (loop-arg ... ...))))
