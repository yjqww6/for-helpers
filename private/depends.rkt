#lang racket/base
(require syntax/kerncase racket/list
         syntax/stx racket/sequence
         syntax/id-table syntax/id-set
         (for-template racket/base))

(provide find-depends)

(define (find-depends vars bd outer)
  (define body
    (local-expand
     #`(#%plain-lambda
        #,outer
        (#%plain-lambda #,vars #,bd))
     'expression
     '()))
  (kernel-syntax-case body #f
    [(#%plain-lambda _ (#%plain-lambda (formals ...)
                                       body ...))
     (let ([captured (find-depends-helper
                      (syntax->list #'(formals ...))
                      #'(begin body ...))])
       (for/list ([x (in-syntax #'(formals ...))]
                  [o (in-list vars)]
                  #:when
                  (free-id-set-member? captured x))
         o))]))

(define (find-depends-helper vars fpe)
  (define (->set x)
    (immutable-free-id-set
     (cond
       [(identifier? x)  (list x)]
       [(not x) '()]
       [else x])))

  (define (remove vars formals)
    (if (syntax? formals)
        (syntax-case formals ()
          [() vars]
          [(x . r)
           (remove (free-id-table-remove vars #'x) #'r)]
          [x
           (identifier? #'x)
           (free-id-table-remove vars #'x)])
        (for/fold ([vars vars])
                  ([x (in-free-id-set formals)])
          (free-id-table-remove vars x))))
  
  (define init
    (make-immutable-free-id-table
     (for/hash ([v (in-list vars)])
       (values v v))))
  (let loop ([vars init] [fpe fpe])
    (kernel-syntax-case fpe #f
      [x (identifier? #'x)
         (->set (free-id-table-ref vars #'x #f))]
      [(#%plain-lambda formals expr ...)
       (loop (remove vars #'formals)
             #'(begin expr ...))]
      [(case-lambda (formals expr ...) ...)
       (apply
        free-id-set-union
        (stx-map (λ (f e)
                   (loop (remove vars f) e))
                 #'(formals ...)
                 #'((begin expr ...) ...)))]
      [(if e1 e2 e3)
       (loop vars #'(begin e1 e2 e3))]
      [(begin expr ...)
       (for/fold ([r (immutable-free-id-set)])
                 ([e (in-syntax #'(expr ...))])
         (free-id-set-union r (loop vars e)))]
      [(begin0 expr ...)
       (loop vars #'(begin expr ...))]
      [(let-values ([(id ...) e0] ...)
         e1 ...)
       (let ([init (loop vars #'(begin e0 ...))])
         (free-id-set-union
          init
          (loop (remove vars init)
                #'(begin e1 ...))))]
      [(letrec-values ([(id ...) e0] ...)
         e1 ...)
       (let ([init (immutable-free-id-set (syntax->list #'(id ... ...)))])
         (loop (remove vars init)
               #'(begin e0 ... e1 ...)))]
      [(set! id e)
       (loop vars #'(begin id e))]
      [(quote _) (immutable-free-id-set)]
      [(quote-syntax _) (immutable-free-id-set)]
      [(quote-syntax _ #:local) (immutable-free-id-set)]
      [(with-continuation-mark e1 e2 e3)
       (loop vars #'(begin e1 e2 e3))]
      [(#%plain-app e ...)
       (loop vars #'(begin e ...))]
      [(#%top . x)
       (->set (free-id-table-ref vars #'x #f))]
      [(#%variable-reference id)
       (->set (free-id-table-ref vars #'id #f))]
      [(#%variable-reference (#%top . id))
       (->set (free-id-table-ref vars #'id #f))]
      [(#%variable-reference)
       (immutable-free-id-set)]
      [(#%expression e)
       (loop vars #'e)])))