#lang racket/base
(require syntax/parse syntax/stx racket/sequence
         (for-template racket/base "main.rkt")
         racket/control)

(define (optimize-mapped form)
  (define params '())
  (define clauses '())
  (define bindings '())
  (define (make-procedure proc s)
    (let ()
      (define body
        #`(#,proc #,@(let recur ([s s])
                     (cond
                       [(null? s) '()]
                       [else
                        (syntax-parse (car s)
                          [((~literal in-mapped) Proc^:expr S^ ...+)
                           #:with (proc) (generate-temporaries #'(Proc^))
                           (set! bindings (cons #'[proc Proc^] bindings))
                           (cons #`(proc #,@(recur (syntax->list #'(S^ ...))))
                                 (recur (cdr s)))]
                          [_
                           #:with (tmp) (generate-temporaries '(tmp))
                           (set! params (cons #'tmp params))
                           (set! clauses (cons (car s) clauses))
                           (cons #'tmp (recur (cdr s)))])]))))
      #`(let* #,(reverse bindings)
          (λ #,(reverse params)
            #,body))))
  
  (syntax-parse form
    [[(Id:id ...) ((~literal in-mapped) Proc:expr S:expr ...+)]
     #:with (proc) (generate-temporaries #'(Proc))
     (set! bindings (cons #'[proc Proc] bindings))
     (define procedure (make-procedure #'proc (syntax->list #'(S ...))))
     #`[(Id ...) (in-mapped #,procedure #,@(reverse clauses))]]
    
    [_ form]))

(current-optimize optimize-mapped)