#lang racket/base
(require (for-syntax racket/base syntax/parse
                     racket/sequence racket/syntax
                     syntax/unsafe/for-transform
                     racket/list "private/depends.rkt"))

(provide define-loop-syntax loop in-mapped in-filtered in-nested)

(define-syntax (define-loop-syntax stx)
  (syntax-parse stx
    [(_ name:id proc:expr)
     #'(define-syntax name
         (loop-syntax 'name proc))]
    [(_ (name:id . r) body:expr ...+)
     #'(define-loop-syntax name (λ r body ...))]))

(begin-for-syntax
  (define (group id+ss)
    (syntax-parse id+ss
      [() '()]
      [([(Id:id) S:expr] [(Id*:id) (~and (~literal _) Dis)] ... . rest)
       (cons #'[(Id Id* ...) S]
             (group #'rest))]))
  
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
                   (define-values (outer-ids
                                   loop-ids
                                   inner-ids
                                   outer-setup
                                   inner-setup
                                   loop-setup)
                     (call-with-values
                      (λ ()
                        ((loop-syntax-proc proc) (intro #'S)))
                      (case-lambda
                        [(s) (syntax-parse s
                               [cl:clause
                                (values #'(cl.outer-id ...)
                                        #'(cl.loop-id ...)
                                        #'(cl.inner-id ...)
                                        (attribute cl.outer-setup)
                                        (attribute cl.inner-setup)
                                        (attribute cl.loop-setup))])]
                        [(a b c d e f) (values a b c d e f)])))]
             #:with (outer-id ...) (intro outer-ids)
             #:with (loop-id ...) (intro loop-ids)
             #:with (inner-id ...) (intro inner-ids)
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
             (([(outer-id^ ...) outer-expr] ...)
              outer-check
              ([loop-id loop-expr] ...)
              pos-guard
              ([(inner-id^ ...) inner-expr] ...)
              pre-guard
              post-guard
              (loop-arg ...))
             (expand-for-clause #'S #'[(x.id ...) s])
             #:with (outer-id ...) #'(outer-id^ ... ...)
             #:with (inner-id ...) #'(inner-id^ ... ...)
             #:attr outer-setup
             (λ (outer)
               #`(let-values ([(outer-id^ ...) outer-expr] ...)
                   outer-check
                   (let ([loop-id loop-expr] ...)
                     #,outer)))
             #:attr inner-setup
             (λ (inner done)
               #`(if pos-guard
                     (let-values ([(inner-id^ ...) inner-expr] ...)
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

(define-loop-syntax (in-nested stx)
  (syntax-parse stx
    [[x:ids (_ () S:expr)]
     #'[x S]]
    [[x:ids (_ ([x0:ids S0:expr] r ...) S:expr)]
     #'[x (in-nested1 x0 S0 (in-nested (r ...) S))]]))

(define-loop-syntax (in-nested1 stx)
  (syntax-parse stx
    [[x:ids (_ I:ids S0:expr S1:expr)]
     #:with cl0:clause (syntax/loc stx [I S0])
     #:with cl1:clause (syntax/loc stx [x S1])
     #:with (cl0.loop-setup.depends ...)
     (find-depends (remove-duplicates (syntax->list #'(cl0.inner-id ... cl0.loop-id ...))
                                      bound-identifier=?)
                   ((attribute cl0.loop-setup) #'(void) #'(void))
                   (syntax->list #'(cl0.outer-id ...)))
     #:with (loop-id ...) (remove-duplicates
                           (syntax->list #'(cl1.loop-id ... cl1.outer-id ... I.id ... cl0.loop-setup.depends ...))
                           bound-identifier=?)
     #:with (inner-id ...) #'(cl1.inner-id ...)
     #:with init? (generate-temporary 'init)
     (values #'(cl0.outer-id ...)
             #'(init? loop-id ...)
             #'(init? cl1.inner-id ...)
             (λ (outer)
               #`(let ([loop-id #f] ...)
                   #,((attribute cl0.outer-setup)
                      #`(let ([init? #f] [loop-id loop-id] ...)
                          #,outer))))
             (λ (inner done)
               #`(letrec ([outer-loop (λ (cl0.loop-id ...)
                                        #,((attribute cl0.inner-setup)
                                           ((attribute cl1.outer-setup)
                                            #`(let inner-loop ([cl1.loop-id cl1.loop-id] ...)
                                                #,((attribute cl1.inner-setup)
                                                   #`(let ([init? #t])
                                                       #,inner)
                                                   ((attribute cl0.loop-setup)
                                                    #`(outer-loop cl0.loop-id ...)
                                                    done))))
                                           done))])
                   (if (not init?)
                       (outer-loop cl0.loop-id ...)
                       #,((attribute cl1.inner-setup)
                          inner
                          ((attribute cl0.loop-setup)
                           #`(outer-loop cl0.loop-id ...)
                           done)))))
             (λ (loop done)
               ((attribute cl1.loop-setup)
                loop
                ((attribute cl0.loop-setup)
                 #`(let ([init? #f])
                     #,loop)
                 done))))
     ]))

(define-loop-syntax (in-mapped stx)
  (syntax-parse stx
    [[x:ids (_ proc:expr ls:expr ...+)]
     #:with (tmp ...) (generate-temporaries #'(ls ...))
     (syntax-parse (group #'([(tmp) ls] ...))
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
        (values #'(p ls.outer-id ... ...)
                #'(ls.loop-id ... ...)
                #'(x.id ... ls.inner-id ... ...)
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
     (syntax-parse (group #'([(x.id) ls] ...))
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
        (values #'(p ls.outer-id ... ...)
                #'(ls.loop-id ... ...)
                #'(ls.inner-id ... ...)
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