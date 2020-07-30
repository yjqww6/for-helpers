#lang racket/base

(require syntax/unsafe/for-transform
         racket/syntax syntax/stx
         racket/sequence
         (for-template racket/base))

(provide (all-defined-out))

(define (compose-single-valued ids ss)
  (with-syntax* ([(Id ...) ids]
                 [(S ...) ss]
                 [(P ...) #'([(Id) S] ...)]
                 [[(([(outer-id ...) outer-expr] ...)
                    outer-check
                    ([loop-id loop-expr] ...)
                    pos-guard
                    ([(inner-id ...) inner-expr] ...)
                    pre-guard
                    post-guard
                    (loop-arg ...))
                   ...]
                  (stx-map expand-for-clause #'(S ...) #'(P ...))])
    #'(([(outer-id ...) outer-expr] ... ...)
       (begin outer-check ...)
       ([loop-id loop-expr] ... ...)
       (and pos-guard ...)
       ([(inner-id ...) inner-expr] ... ...)
       (and pre-guard ...)
       (and post-guard ...)
       (loop-arg ... ...))))