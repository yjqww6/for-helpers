#lang racket/base
(require syntax/parse syntax/stx racket/sequence racket/syntax
         (for-template racket/base "main.rkt")
         racket/control)

(define mp (make-continuation-prompt-tag))
(define mc (make-continuation-prompt-tag))

(define-syntax with-Ps
  (syntax-rules ()
    [(_ () body ...)
     (let () body ...)]
    [(_ (p p* ...) body ...)
     (prompt-at p (with-Ps (p* ...) body ...))]))

(define-syntax with-C
  (syntax-rules ()
    [(_ () body ...)
     (let () body ...)]
    [(_ ([k p] . r) body ...)
     (control-at p k (with-C r body ...))]))

(define (optimize form)
  (define (_-free? s)
    (not
     (for/or ([s (in-syntax s)])
       (and (identifier? s)
            (free-identifier=? s #'_)))))
  (syntax-parse form
    ;;nested in-mapped, multiple values handled
    [[(Id:id ...) ((~literal in-mapped) Proc:expr S:expr ...+)]
     #:when (_-free? #'(S ...))
     #:when (for/or ([s (in-syntax #'(S ...))])
              (syntax-parse s
                [((~literal in-mapped) Proc:expr (~not (~literal _)) ...+) #t]
                [else #f]))
     #:with (proc) (generate-temporaries #'(Proc))
     #:do [(define-values (params clauses) (values '() '()))]
     (optimize
      (with-Ps (mc mp)
        (define procedure
          (let recur ([s #'(in-mapped Proc S ...)])
            (syntax-parse s
              [((~and (~literal in-mapped) Dis) Proc:expr S:expr ...+)
               #:when (_-free? #'(S ...))
               #:with (proc) (generate-temporaries #'(Proc))
               (record-disappeared-uses #'Dis #f)
               (with-C ([k mp])
                 #`(let ([proc Proc])
                     #,(with-Ps (mp)
                         (k #`(proc #,@(stx-map recur #'(S ...)))))))]
              [else
               #:with (Tmp) (generate-temporaries #'(tmp))
               (set! params (cons #'Tmp params))
               (set! clauses (cons s clauses))
               #'Tmp])))
        (with-C ([k mp] [kc mc])
          #`[(Id ...) (in-mapped #,(kc #`(λ #,(reverse params)
                                           #,(k procedure)))
                                 #,@(reverse clauses))])))]

    ;;nested single-valued in-filtered and in-mapped
    [[(Id:id) (~and S:expr
                    (~or* ((~literal in-filtered)
                           _
                           ((~literal in-mapped) _ _))
                          ((~literal in-mapped)
                           _
                           ((~literal in-filtered) _ _))))]
     #:with (v) (generate-temporaries #'(v))
     (with-Ps (mc mp)
       (let loop ([s #'S] [tail #t])
         (syntax-parse s
           [((~and (~literal in-mapped) Dis) Proc:expr S:expr)
            #:with (proc) (generate-temporaries #'(Proc))
            (record-disappeared-uses #'Dis #f)
            (with-C ([k mp])
              #`(let ([proc Proc])
                  #,(with-Ps (mp)
                      (if tail
                          (k #`(values #t (proc #,(loop #'S #f))))
                          (k #`(proc #,(loop #'S #f)))))))]
           [((~and (~literal in-filtered) Dis) Pred:expr S:expr)
            #:with (tmp pred) (generate-temporaries #'(tmp Pred))
            (record-disappeared-uses #'Dis #f)
            (with-C ([k mp])
              #`(let ([pred Pred])
                  #,(with-Ps (mp)
                      #`(let ([tmp #,(loop #'S #f)])
                          (if (pred tmp)
                              #,(if tail
                                    (k #'(values #t tmp))
                                    (k #'tmp))
                              (values #f #f))))))]
           [else
            (with-C ([k mp] [kc mc])
              #`[(Id)
                 (in-filter&map
                  #,(kc #`(λ (v)
                            #,(k #'v)))
                  #,s)])])))]

    ;;nested single-valued in-filtered
    [[(Id:id) ((~literal in-filtered) Pred:expr (~and S:expr ((~literal in-filtered) . _)))]
     #:with (pred v) (generate-temporaries #'(Pred v))
     (optimize
      (with-Ps (mc mp)
        (let loop ([s #'(in-filtered Pred S)])
          (syntax-parse s
            [((~and (~literal in-filtered) Dis) Pred:expr S:expr)
             #:with (pred) (generate-temporaries #'(Pred))
             (record-disappeared-uses #'Dis #f)
             (with-C ([k mp])
               #`(let ([pred Pred])
                   #,(with-Ps (mp)
                       (k (cons #'(pred v)
                                (loop #'S))))))]
            [else
             (with-C ([kp mp] [kc mc])
               #`[(Id) (in-filtered
                        #,(kc #`(λ (v) (and #,@(reverse (kp '())))))
                        #,s)])]))))]
    
    [_ form]))

(current-optimize optimize)