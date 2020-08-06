#lang scribble/manual
@require[@for-label["../main.rkt" "../extra.rkt"
                    racket/base syntax/unsafe/for-transform
                    racket/generator]
         racket/sandbox scribble/example racket/runtime-path]

@title{for-helpers}
@author{yjqww6}

Helper macros for racket/for.

@section{APIs}

@defmodule[for-helpers]

@(define-runtime-path main "../main.rkt")
@(define-runtime-path extra "../extra.rkt")
@(define-runtime-path private/main "../private/main.rkt")
@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket/base
                     #:requires (list 'racket/match private/main main extra
                                      '(for-syntax racket/base racket/pretty)))))

@defform[(in-mapped proc sequence ...+)]{
 Returns a sequence similar to
 @racketblock[(in-generator
               (let ([p proc])
                 (for ([s sequence] ...)
                   (call-with-values
                    (λ () (p s ...))
                    yield))))]
 , without touching continuations or building intermediate sequences.
 
 This macro cannot be used outside @racket[for] clauses.

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
                   (yield s ...))))]
 , without touching continuations or building intermediate sequences.
 
 This macro cannot be used outside @racket[for] clauses.

 @examples[#:eval my-evaluator
           (for/list ([(a b) (in-filtered <
                                          '(1 3 5 -1)
                                          (in-list '(6 4 2 0)))])
             (cons a b))
           (for/list ([a (in-filtered odd? (in-range 5))]
                      [b (in-filtered even? (in-range 5))])
             (cons a b))]
}
@subsection{Performance Notes}
@(require (for-label racket/list))
@(define in-filter-mapped #f)
@(define optimize #f)

Due to the limitations of code structures of @racket[:do-in],
@racket[in-mapped] and @racket[in-filtered] do not compose well without optimizations.
This package does optimize nested forms, which should cover most use cases.
See @italic{tests/bench.rkt}.

@(my-evaluator
  '(define-syntax-rule (optimize form)
     (begin-for-syntax
       (pretty-display
        (syntax->datum
         ((current-optimize)
          #'form))))))
@examples[#:eval my-evaluator
          (optimize
           [(a) (in-filtered positive?
                             (in-filtered even?
                                          (in-range -5 5)))])
          (optimize
           [(a) (in-mapped cons
                           (in-mapped cons
                                      (in-range 500)
                                      (in-range 500 1000))
                           (in-mapped cons
                                      (in-range 1000 1500)
                                      (in-range 1500 2000)))])

          (optimize
           [(a) (in-filtered
                 odd?
                 (in-mapped add1
                            (in-filtered
                             even?
                             (in-mapped (λ (v) (* 2 v))
                                        (in-range 10)))))])
          ]

Currently, it is not suggested to define something like @racket[in-filter-mapped]
as @racket[(in-filtered values (in-mapped _ ...))] using @racket[define-sequence-syntax],
since there is no partial expansion support from @racket[expand-for-clause],
which disables potential optimizations when nested.

@section{More APIs}

@defmodule[for-helpers/extra]

@defform[(in-filter&map proc sequence ...+)]{
 Returns a sequence similar to
 @racketblock[(in-generator
               (let ([p proc])
                 (for ([s sequence] ...)
                   (let-values ([(ok x ...) (proc s ...)])
                     (when ok
                       (yield x ...))))))]
 , without touching continuations or building intermediate sequences.
 
 This macro cannot be used outside @racket[for] clauses.

 @examples[#:eval my-evaluator
           (for/list ([(a b) (in-filter&map (λ (a b)
                                              (values (< a b) (+ a b) (- a b)))
                                            (in-list '(1 3 5 -1))
                                            (in-list '(6 4 2 0)))])
             (cons a b))]
}

@defform[(in-lists sequence)]{
 Returns a sequence similar to
 @racketblock[(in-generator
               (for* ([s sequence]
                      [x (in-list s)])
                 (yield x)))]
 , without touching continuations or building intermediate sequences.
 
 This macro cannot be used outside @racket[for] clauses.

 @examples[#:eval my-evaluator
           (for/list ([x (in-lists (in-lists (in-list '(((1 2 3) (4 5 6))
                                                        ((7 8 9) (a b c))))))])
             x)]
}

@defform[(in-nested ([(s ...) sequences] ...) sequence)]{
 Returns a sequence similar to
 @racketblock[(in-generator
               (for* ([(s ...) sequences] ...
                      [(x ...) sequence])
                 (yield x ...)))]
 , without touching continuations or building intermediate sequences.
 
 This macro cannot be used outside @racket[for] clauses.

 @examples[#:eval my-evaluator
           (for/list ([a (in-nested
                          ([(b) (in-list '((0) (1 2 3) (4 5)))]
                           [(x) (in-list b)])
                          (in-range x))])
             a)]
 @bold{Warning:} This macro may bloat your code.
}

@subsection{Performance Notes for @racket[for-helpers/extra]}
Benchmarks show in RacketCS, the performance of @racket[in-lists] is nearly optimal,
and @racket[in-nested] is also reasonably good.
But unfortunately, it seems RacketBC doesn't properly optimize these forms,
which results in poor performance.

@section{Composing Multiple Values}
For forms supporting multiple sequence inputs, @italic{n} consecutive @racket[_] can be specified to indicating
the followed sequence returns @italic{n+1} values.
@examples[#:eval my-evaluator
          (for/list ([a (in-mapped +
                                   (in-hash #hash((1 . 2)
                                                  (3 . 4)))
                                   _)])
            a)
          (for/list ([(a b) (in-filtered (λ (a b) (odd? a))
                                         (in-hash #hash((1 . 3)
                                                        (2 . 4)))
                                         _)])
            (list a b))]
