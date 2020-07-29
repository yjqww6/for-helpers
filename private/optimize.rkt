#lang racket/base
(require syntax/parse syntax/stx racket/sequence
         (for-template racket/base "main.rkt" "and-let.rkt")
         racket/control)

(define (optimize form)
  
  (syntax-parse form
    [[(Id:id ...) ((~literal in-mapped) Proc:expr S:expr ...+)]
     #:when (for/or ([s (in-syntax #'(S ...))])
              (syntax-parse s
                [((~literal in-mapped) . _) #t]
                [else #f]))
     #:with (proc) (generate-temporaries #'(Proc))
     
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
  
     (set! bindings (cons #'[proc Proc] bindings))
     (define procedure (make-procedure #'proc (syntax->list #'(S ...))))
     (optimize
      #`[(Id ...) (in-mapped #,procedure #,@(reverse clauses))])]

    [[(Id:id) ((~literal in-filtered)
               Pred0:expr
               ((~literal in-mapped)
                Proc0:expr
                ((~literal in-filtered)
                 Pred1:expr
                 ((~literal in-mapped)
                  Proc1:expr
                  S:expr))))]
     #:with (pred0 pred1 proc0 proc1) (generate-temporaries #'(Pred0 Pred1 Proc0 Proc1))
     #:do
     [(define preds (list #'[pred1 Pred1] #'[pred0 Pred0]))
      (define procs (list #'[proc1 Proc1] #'[proc0 Proc0]))]
     #:with S^
     (let loop ([s #'S])
       (syntax-parse s
         [((~literal in-filtered)
           Pred:expr
           ((~literal in-mapped)
            Proc:expr
            S:expr))
          #:with (pred proc) (generate-temporaries #'(Pred Proc))
          (set! procs (cons #'[proc Proc] procs))
          (set! preds (cons #'[pred Pred] preds))
          (loop #'S)]
         [else s]))
     #:with [(proc Proc) ...] procs
     #:with [(pred Pred) ...] preds
     (optimize
      #'[(Id)
         (in-filtered values
                      (in-mapped (let ([proc Proc] ...
                                       [pred Pred] ...)
                                   (λ (v)
                                     (let ([a v])
                                       (and-let* ([a pred (proc a)] ...)
                                                 a))))
                                 S^))])]

    [[(Id:id) ((~literal in-filtered) Pred:expr (~and S:expr ((~literal in-filtered) . _)))]
     #:with (pred) (generate-temporaries #'(Pred))
     (define bindings (list #'[pred Pred]))
     (define clause
       (let loop ([s #'S])
         (syntax-parse s
           [((~literal in-filtered) Pred:expr S:expr)
            #:with (pred) (generate-temporaries #'(Pred))
            (set! bindings (cons #'[pred Pred] bindings))
            (loop #'S)
            ]
           [else
            s])))
     (with-syntax ([([pred Pred] ...) bindings])
       (optimize
        #`[(Id) (in-filtered (let ([pred Pred] ...)
                               (λ (v)
                                 (and (pred v) ...)))
                             #,clause)]))]
    
    [_ form]))

(current-optimize optimize)