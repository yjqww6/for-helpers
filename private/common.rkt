#lang racket/base
(require (for-syntax racket/base syntax/stx))

(provide for-disappeared)

(define-syntax (for-disappeared stx)
  (syntax-case stx ()
    [(_ s ...)
     (syntax-property
      #'(void)
      'disappeared-use
      (stx-map syntax-local-introduce #'(s ...)))]))