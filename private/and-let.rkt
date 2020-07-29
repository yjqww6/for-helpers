#lang racket/base
(provide and-let*)
(define-syntax and-let*
  (syntax-rules ()
    [(_ () body ...)
     (let () body ...)]
    [(_ ([X Pred Expr] . r) body ...)
     (let ([X Expr])
       (and (Pred X)
            (and-let* r body ...)))]))