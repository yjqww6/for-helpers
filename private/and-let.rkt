#lang racket/base
(provide and-let*)
(define-syntax and-let*
  (syntax-rules ()
    [(_ () body ...)
     (let () body ...)]
    [(_ ([X Expr] . r) body ...)
     (let ([X Expr])
       (and X
            (and-let* r body ...)))]))