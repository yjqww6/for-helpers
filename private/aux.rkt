#lang racket/base

(require syntax/unsafe/for-transform
         racket/syntax syntax/stx
         racket/sequence
         (for-template racket/base))

(provide (all-defined-out))

(define (compose-single-valued ids ss)
    
  (define (compose acc o1 s1)
    (syntax-case acc ()
      [(([(outer-id0 ...) outer-expr0] ...)
        outer-check0
        ([loop-id0 loop-expr0] ...)
        pos-guard0
        ([(inner-id0 ...) inner-expr0] ...)
        pre-guard0
        post-guard0
        (loop-arg0 ...))
       (syntax-case (expand-for-clause o1 s1) ()
         [(([(outer-id1 ...) outer-expr1] ...)
           outer-check1
           ([loop-id1 loop-expr1] ...)
           pos-guard1
           ([(inner-id1 ...) inner-expr1] ...)
           pre-guard1
           post-guard1
           (loop-arg1 ...))
          #'(([(outer-id1 ...) outer-expr1] ...
              [(outer-id0 ...) outer-expr0] ...)
             (begin outer-check1 outer-check0)
             ([loop-id1 loop-expr1] ... [loop-id0 loop-expr0] ...)
             (and pos-guard1 pos-guard0)
             ([(inner-id1 ...) inner-expr1] ...
              [(inner-id0 ...) inner-expr0] ...)
             (and pre-guard1 pre-guard0)
             (and post-guard1 post-guard0)
             (loop-arg1 ... loop-arg0 ...))])]))
    
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