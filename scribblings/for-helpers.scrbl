#lang scribble/manual
@require[@for-label["../main.rkt"
                    racket/base
                    racket/generator]
         racket/sandbox scribble/example racket/runtime-path]

@title{for-helpers}
@author{yjqww6}

@defmodule[for-helpers]

Helper macros for racket for macros to avoid temporary sequences.

@(define-runtime-path main "../main.rkt")
@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket/base
                     #:requires (list 'racket/match main))))

@defform[(in-mapped proc sequence ...+)]{
 Returns a sequence similar to
 @racketblock[(in-generator
               (let ([p proc])
                 (for ([s sequence] ...)
                   (call-with-values
                    (λ () (p s ...))
                    yield))))]
 , without touching continuations or building temporary sequences.
 
 This Macro cannot be used outside @racket[for] clauses.

 @examples[#:eval my-evaluator
           (for/list ([(a b)
                       (in-mapped
                        (match-lambda [(cons a b) (values a b)])
                        '((1 . 2) (3 . 4) (4 . 5)))])
             (cons a b))
           (for/list ([a (in-mapped cons
                                    (in-mapped cons
                                               (in-range 5)
                                               (in-range 5 10))
                                    (in-mapped cons
                                               (in-range 10 15)
                                               (in-range 15 20)))])
             a)]
}

@defform[(in-filtered pred sequence ...+)]{
 Returns a sequence similar to
 @racketblock[(in-generator
               (let ([p pred])
                 (for ([s sequence] ...
                       #:when (p s ...))
                   (call-with-values
                    (λ () (values s ...))
                    yield))))]
 , without touching continuations or building temporary sequences.
 
 This Macro cannot be used outside @racket[for] clauses.

 @examples[#:eval my-evaluator
           (for/list ([(a b) (in-filtered <
                                          '(1 3 5 -1)
                                          (in-list '(6 4 2 0)))])
             (cons a b))
           (for/list ([a (in-filtered odd? (in-range 5))]
                      [b (in-filtered even? (in-range 5))])
             (cons a b))]
}

@defform[(in-filter-mapped pred sequence ...+)]{
 Returns a sequence similar to
 @racket[(in-filtered values (in-mapped pred sequence ...))].
 
 This Macro cannot be used outside @racket[for] clauses.

 @examples[#:eval my-evaluator
           (for/list ([a (in-filter-mapped (λ (x) (and (odd? x) (add1 x))) (in-range 5))])
             a)]
}