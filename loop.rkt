#lang racket/base
(require (for-syntax racket/base syntax/parse
                     racket/sequence racket/syntax
                     syntax/unsafe/for-transform))

(provide define-loop-syntax loop in-mapped in-filtered)

(define-syntax (define-loop-syntax stx)
  (syntax-parse stx
    [(_ name:id proc:expr)
     #'(define-syntax name
         (loop-syntax 'name proc))]
    [(_ (name:id . r) body:expr ...+)
     #'(define-loop-syntax name (λ r body ...))]))

(begin-for-syntax
  (struct loop-syntax (name proc)
    #:property prop:procedure
    (λ (self stx)
      (raise-syntax-error (loop-syntax-name self) "use outside loop")))
  
  (define-syntax-class ids
    (pattern x:id #:attr (id 1) (list #'x))
    (pattern (y:id ...) #:attr (id 1) (syntax->list #'(y ...))))

  (define-syntax-class clause
    (pattern (~and S [x:ids (c:id _:expr ...)])
             #:do [(define proc (syntax-local-value
                                 #'c
                                 (λ () #f)))]
             #:when (and proc (loop-syntax? proc))
             #:do [(define intro (make-syntax-introducer))
                   (define-values (loop-ids
                                   outer-setup
                                   inner-setup
                                   loop-setup)
                     (call-with-values
                      (λ ()
                        ((loop-syntax-proc proc) (intro #'S)))
                      (case-lambda
                        [(s) (syntax-parse s
                               [cl:clause
                                (values #'(cl.loop-id ...)
                                        (attribute cl.outer-setup)
                                        (attribute cl.inner-setup)
                                        (attribute cl.loop-setup))])]
                        [(a b c d) (values a b c d)])))]
             #:with (loop-id ...) (intro loop-ids)
             #:attr outer-setup
             (λ (outer) (intro (outer-setup (intro outer))))
             #:attr inner-setup
             (λ (inner done)
               (intro (inner-setup (intro inner)
                                   (intro done))))
             #:attr loop-setup
             (λ (loop done)
               (intro (loop-setup (intro loop)
                                  (intro done)))))
    
    (pattern (~and S [x:ids s:expr])
             #:with
             (([(outer-id ...) outer-expr] ...)
              outer-check
              ([loop-id loop-expr] ...)
              pos-guard
              ([(inner-id ...) inner-expr] ...)
              pre-guard
              post-guard
              (loop-arg ...))
             (expand-for-clause #'S #'[(x.id ...) s])
             #:attr outer-setup
             (λ (outer)
               #`(let-values ([(outer-id ...) outer-expr] ...)
                   outer-check
                   (let ([loop-id loop-expr] ...)
                     #,outer)))
             #:attr inner-setup
             (λ (inner done)
               #`(if pos-guard
                     (let-values ([(inner-id ...) inner-expr] ...)
                       (if pre-guard
                           #,inner
                           #,done))
                     #,done))
             #:attr loop-setup
             (λ (loop done)
               #`(if post-guard
                     (let ([loop-id loop-arg] ...)
                       #,loop)
                     #,done)))
    ))

(define-loop-syntax (in-mapped stx)
  (syntax-parse stx
    [[x:ids (_ proc:expr ls:expr ...+)]
     #:with (tmp ...) (generate-temporaries #'(ls ...))
     (syntax-parse #'([(tmp) ls] ...)
       [(ls:clause ...+)
        #:do [(define outer-setup (apply compose1 (attribute ls.outer-setup)))
              (define (inner-setup inner done)
                ((apply compose1 (map (λ (f) (λ (inner) (f inner done))) (attribute ls.inner-setup)))
                 inner))
              (define (loop-setup loop done)
                ((apply compose1
                        (map (λ (f) (λ (loop) (f loop done)))
                             (attribute ls.loop-setup)))
                 loop))]
        (values #'(ls.loop-id ... ...)
                (λ (outer)
                  #`(let ([p proc])
                      #,(outer-setup outer)))
                (λ (inner done)
                  (inner-setup
                   #`(let-values ([(x.id ...) (p tmp ...)])
                       #,inner)
                   done))
                loop-setup)])]))

(define-loop-syntax (in-filtered stx)
  (syntax-parse stx
    [[x:ids (_ proc:expr ls:expr ...+)]
     (syntax-parse #'([(x.id) ls] ...)
       [(ls:clause ...)
        #:do [(define outer-setup (apply compose1 (attribute ls.outer-setup)))
              (define (inner-setup inner done)
                ((apply compose1 (map (λ (f) (λ (inner) (f inner done))) (attribute ls.inner-setup)))
                 inner))
              (define (loop-setup loop done)
                ((apply compose1
                        (map (λ (f) (λ (loop) (f loop done)))
                             (attribute ls.loop-setup)))
                 loop))]
        (values #'(ls.loop-id ... ...)
                (λ (outer)
                  #`(let ([p proc])
                      #,(outer-setup outer)))
                (λ (inner done)
                  #`(let loop ([ls.loop-id ls.loop-id] ... ...)
                      #,(inner-setup
                         #`(if (p x.id ...)
                               #,inner
                               #,(loop-setup
                                  #'(loop ls.loop-id ... ...)
                                  done))
                         done)))
                loop-setup)])]))

(define-syntax (loop stx)
  (syntax-parse stx
    [(_ next-turn:id
        (~optional (~seq #:extra ([extra:id e:expr] ...))
                   #:defaults ([(extra 1) '()]
                               [(e 1) '()]))
        (clause ...)
        (~optional (~seq #:done done:expr)
                   #:defaults ([done #'(void)]))
        body:expr ...+)
     #:declare clause clause
     #:with next (generate-temporary 'next)
     #:do [(define n
             (for/fold ([acc #'(next extra ... clause.loop-id ... ...)])
                       ([loop-setup
                         (in-list
                          (reverse
                           (attribute clause.loop-setup)))])
               (loop-setup acc #'done)))
           (define bd
             (for/fold ([acc #`(let ([next-turn (λ (extra ...) #,n)]) body ...)])
                       ([inner-setup
                         (in-list
                          (reverse
                           (attribute clause.inner-setup)))])
               (inner-setup acc #'done)))
           (define all
             (for/fold ([acc #`(let next ([extra e] ... [clause.loop-id clause.loop-id]
                                                    ... ...)
                                 #,bd)])
                       ([outer-setup
                         (in-list
                          (reverse
                           (attribute clause.outer-setup)))])
               (outer-setup acc)))]
     all
     ]))