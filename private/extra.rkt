#lang racket/base
(require (for-syntax racket/base syntax/parse
                     syntax/stx syntax/unsafe/for-transform
                     racket/sequence racket/list racket/syntax
                     syntax/id-set)
         racket/unsafe/ops "common.rkt")

(provide in-lists in-nested)

(define-for-syntax (remove-loop-ids loop-ids extra-ids)
  (with-syntax ([(extra-loop-id ...)
                 (remove-duplicates (append (syntax->list loop-ids)
                                            (syntax->list extra-ids))
                                    bound-identifier=?)])
    (list-tail (syntax->list #'(extra-loop-id ...))
               (length (syntax->list loop-ids)))))

(define-sequence-syntax in-nested
  (syntax-rules ())
  (λ (stx)
    (syntax-parse stx
      [[(Id:id ...) (_ () S:expr)]
       #'[(Id ...) S]]
      [[(Id:id ...) (_ ([(I:id ...) S0:expr] rest ...) S:expr)]
       #'[(Id ...) (in-nested1 (I ...) S0
                               (in-nested (rest ...) S))]])))

(define-for-syntax (try-omit-state loop-ids pos-guard)
  (define-syntax-class compare #:literals (if < > unsafe-fx< unsafe-fx>)
    [pattern ((~or < > unsafe-fx< unsafe-fx>) x:id _:expr)]
    [pattern (if _:expr
                 ((~or < > unsafe-fx< unsafe-fx>) x:id _:expr)
                 ((~or < > unsafe-fx< unsafe-fx>) y:id _:expr))
             #:when (free-identifier=? #'x #'y)])
  (syntax-parse pos-guard #:literals (null? not pair?)
    [#t
     #:when (not (null? loop-ids))
     #:with uninit (syntax-local-lift-expression #'(gensym 'uninit))
     (values (car loop-ids)
             #`(not (eq? #,(car loop-ids) uninit))
             (cons #'uninit (map (λ (_) #'#f) (cdr loop-ids))))]
    [x:id
     #:when (for/or ([id (in-list loop-ids)])
              (free-identifier=? id #'x))
     (values (for/first ([id (in-list loop-ids)]
                         #:when (free-identifier=? id #'x))
               id)
             pos-guard
             (map (λ (_) #'#f) loop-ids))]
    
    [(~or (pair? x:id)
          (not (null? x:id)))
     #:when (for/or ([id (in-list loop-ids)])
              (free-identifier=? id #'x))
     (values (for/first ([id (in-list loop-ids)]
                         #:when (free-identifier=? id #'x))
               id)
             pos-guard
             (for/list ([id (in-list loop-ids)])
               (if (free-identifier=? id #'x)
                   #''()
                   #'#f)))]
    
    [e:compare
     #:when (for/or ([id (in-list loop-ids)])
              (free-identifier=? id #'e.x))
     #:with x (for/first ([id (in-list loop-ids)]
                          #:when (free-identifier=? id #'e.x))
                id)
     (values #'x
             #`(and x #,pos-guard)
             (map (λ (_) #'#f) loop-ids))]
    
    [else
     (values #f pos-guard (map (λ (_) #'#f) loop-ids))]))

(define-sequence-syntax in-nested1
  (syntax-rules ())
  (λ (stx)
    (parameterize ([current-recorded-disappeared-uses '()])
      (syntax-parse stx
        [[(Id:id ...) (_ (I:id ...) S0:expr S1:expr)]
         #:with (Tmp ...) (generate-temporaries #'(Id ...))
         #:do [(define intro (make-syntax-introducer #t))
               (define s0 (expand-for-clause #'S0 #`[#,(intro #'(I ...)) S0]))
               (define s1 (expand-for-clause #'S1 #`[(Tmp ...) #,(intro #'S1)]))
               (record-disappeared-uses
                (or (syntax-property s0 'disappeared-use) '()))
               (record-disappeared-uses
                (or (syntax-property s1 'disappeared-use) '()))]
         #:with (([(outer-id0 ...) outer-expr0] ...)
                 outer-check0
                 ([loop-id0 loop-expr0] ...)
                 pos-guard0
                 ([(inner-id0 ...) inner-expr0] ...)
                 pre-guard0
                 post-guard0
                 (loop-arg0 ...))
         s0
         #:with (([(outer-id1 ...) outer-expr1] ...)
                 outer-check1
                 ([loop-id1 loop-expr1] ...)
                 pos-guard1
                 ([(inner-id1 ...) inner-expr1] ...)
                 pre-guard1
                 post-guard1
                 (loop-arg1 ...))
         s1
         #:with (outer-loop-id1 ...)
         (remove-loop-ids #'(loop-id1 ...) #'(outer-id1 ... ...))
         #:with (inner-loop-id0 ...)
         (remove-loop-ids #'(loop-id0 ...) #'(inner-id0 ... ...))
         #:with (inner-loop-id1 ...)
         (remove-loop-ids #'(loop-id1 ... outer-loop-id1 ...) #'(inner-id1 ... ...))
         #:with (inner-id2 ...)
         #'(inner-loop-id1 ... loop-id1 ... outer-loop-id1 ... inner-loop-id0 ... loop-id0 ...)
         #:with (inner-arg2 ...)
         #'(inner-loop-id1 ... loop-id1 ... outer-loop-id1 ... inner-loop-id0 ... loop-arg0 ...)
         #:with (falsy ...)
         (stx-map (λ (_) #'#f) #'(Tmp ... inner-id2 ...))
         #:with (Dis ...) (current-recorded-disappeared-uses)
         
         ;; first case
         #:do [(define-values (omit-state? new-pos-guard1 new-loop-inits)
                 (try-omit-state (syntax->list #'(loop-id1 ...)) #'pos-guard1))]
         #:with (loop-init1 ...) new-loop-inits
         ;; end first case

         (cond
           [(and (eq? (syntax-e #'post-guard0) #t)
                 (eq? (syntax-e #'post-guard1) #t)
                 omit-state?)
            #`[(Id ...)
               (:do-in
                ([(outer-id0 ...) outer-expr0] ...)
                (begin (for-disappeared Dis ...) outer-check0)
                ([loop-id1 loop-init1] ... [outer-loop-id1 #f] ... [inner-loop-id0 #f] ... [loop-id0 loop-expr0] ...)
                #t
                ([(Id ... inner-id2 ...)
                  (letrec ([loop
                            (λ (loop-id0 ...)
                              (if pos-guard0
                                  (let-values ([(inner-id0 ...) inner-expr0] ...)
                                    (if pre-guard0
                                        (begin
                                          (let-values ([(outer-id1 ...) outer-expr1] ...)
                                            outer-check1
                                            (let-values ([(loop-id1) loop-expr1] ...)
                                              (if pos-guard1
                                                  (let-values ([(inner-id1 ...) inner-expr1] ...)
                                                    (if pre-guard1
                                                        (values Tmp ... inner-arg2 ...)
                                                        (loop loop-arg0 ...)))
                                                  (loop loop-arg0 ...)))))
                                        (values falsy ...)))
                                  (values falsy ...)))])
                    (cond
                      [#,new-pos-guard1
                       (let-values ([(inner-id1 ...) inner-expr1] ...)
                         (if pre-guard1
                             (values Tmp ... inner-id2 ...)
                             (loop loop-id0 ...)))]
                      [else
                       (loop loop-id0 ...)]))])
                #,omit-state?
                #t
                (loop-arg1 ... outer-loop-id1 ... inner-loop-id0 ... loop-id0 ...)
                )]]
           [(eq? (syntax-e #'post-guard0) #t)
            #'[(Id ...)
               (:do-in
                ([(outer-id0 ... ...)
                  (let-values ([(outer-id0 ...) outer-expr0] ...)
                    outer-check0
                    (values outer-id0 ... ...))])
                (for-disappeared Dis ...)
                ([state #f] [loop-id1 #f] ... [outer-loop-id1 #f] ... [inner-loop-id0 #f] ... [loop-id0 loop-expr0] ...)
                #t
                ([(state Id ... inner-id2 ...)
                  (letrec ([loop
                            (λ (loop-id0 ...)
                              (if pos-guard0
                                  (let-values ([(inner-id0 ...) inner-expr0] ...)
                                    (if pre-guard0
                                        (begin
                                          (let-values ([(outer-id1 ...) outer-expr1] ...)
                                            outer-check1
                                            (let-values ([(loop-id1) loop-expr1] ...)
                                              (if pos-guard1
                                                  (let-values ([(inner-id1 ...) inner-expr1] ...)
                                                    (if pre-guard1
                                                        (values #t Tmp ... inner-arg2 ...)
                                                        (loop loop-arg0 ...)))
                                                  (loop loop-arg0 ...)))))
                                        (values #f falsy ...)))
                                  (values #f falsy ...)))])
                    (cond
                      [(and state pos-guard1)
                       (let-values ([(inner-id1 ...) inner-expr1] ...)
                         (if pre-guard1
                             (values #t Tmp ... inner-id2 ...)
                             (loop loop-id0 ...)))]
                      [else
                       (loop loop-id0 ...)]))])
                state
                #t
                (post-guard1 loop-arg1 ... outer-loop-id1 ... inner-loop-id0 ... loop-id0 ...)
                )]]
           [else
            #'[(Id ...)
               (:do-in
                ([(outer-id0 ... ...)
                  (let-values ([(outer-id0 ...) outer-expr0] ...)
                    outer-check0
                    (values outer-id0 ... ...))])
                (for-disappeared Dis ...)
                ([state #f] [loop-id1 #f] ... [outer-loop-id1 #f] ... [inner-loop-id0 #f] ... [loop-id0 loop-expr0] ...)
                #t
                ([(state Id ... inner-id2 ...)
                  (letrec ([loop
                            (λ (loop-id0 ...)
                              (if pos-guard0
                                  (let-values ([(inner-id0 ...) inner-expr0] ...)
                                    (if pre-guard0
                                        (begin
                                          (let-values ([(outer-id1 ...) outer-expr1] ...)
                                            outer-check1
                                            (let-values ([(loop-id1) loop-expr1] ...)
                                              (if pos-guard1
                                                  (let-values ([(inner-id1 ...) inner-expr1] ...)
                                                    (if pre-guard1
                                                        (if post-guard0
                                                            (values #t Tmp ... inner-arg2 ...)
                                                            (values 'post Tmp ... inner-id2 ...))
                                                        (if post-guard0
                                                            (loop loop-arg0 ...)
                                                            (values #f falsy ...))))
                                                  (loop loop-arg0 ...)))))
                                        (values #f falsy ...)))
                                  (values #f falsy ...)))])
                    (cond
                      [(eq? state #t)
                       (if pos-guard1
                           (let-values ([(inner-id1 ...) inner-expr1] ...)
                             (if pre-guard1
                                 (values #t Tmp ... inner-id2 ...)
                                 (loop loop-id0 ...)))
                           (loop loop-id0 ...))]
                      [(eq? state #f)
                       (loop loop-id0 ...)]
                      [else ;'post
                       (if pos-guard1
                           (let-values ([(inner-id1 ...) inner-expr1] ...)
                             (if pre-guard1
                                 (values state Tmp ... inner-id2 ...)
                                 (values #f falsy ...)))
                           (values #f falsy ...))]))])
                state
                #t
                ((if post-guard1 state #f) loop-arg1 ... outer-loop-id1 ... inner-loop-id0 ... loop-id0 ...)
                )]])]))))

(define-sequence-syntax in-lists
  (syntax-rules ())
  (λ (stx)
    (parameterize ([current-recorded-disappeared-uses '()])
      (syntax-parse stx
        [[(Id:id) (_ S:expr)]
         #:do [(define s (expand-for-clause #'S #'[(tmp) S]))
               (record-disappeared-uses (or (syntax-property s 'disappeared-use) '()))]
         #:with (Dis ...) (current-recorded-disappeared-uses)
         #:with (([(outer-id ...) outer-expr] ...)
                 outer-check
                 ([loop-id loop-expr] ...)
                 pos-guard
                 ([(inner-id ...) inner-expr] ...)
                 pre-guard
                 post-guard
                 (loop-arg ...))
         s
         
         (cond
           [(eq? (syntax-e #'post-guard) #t)
            #'[(Id)
               (:do-in
                ([(outer-id ...) outer-expr] ...)
                (begin (for-disappeared Dis ...) outer-check)
                ([x '()] [loop-id loop-expr] ...)
                #t
                ([(Id rst loop-id ...)
                  (if (not (null? x))
                      (values (unsafe-car x)
                              (unsafe-cdr x)
                              loop-id ...)
                      (let loop ([loop-id loop-id] ...)
                        (if pos-guard
                            (let-values ([(inner-id ...) inner-expr] ...)
                              (if pre-guard
                                  (begin
                                    (unless (list? tmp)
                                      (raise-argument-error 'tmp "list?" tmp))
                                    (if (null? tmp)
                                        (loop #f loop-arg ...)
                                        (values (unsafe-car tmp) (unsafe-cdr tmp)
                                                loop-arg ...)))
                                  (values #f #f loop-id ...)))
                            (values #f #f loop-id ...))))])
                rst
                #t
                (rst loop-id ...))]]
           [else
            #'[(Id)
               (:do-in
                ([(outer-id ...) outer-expr] ...)
                (begin (for-disappeared Dis ...) outer-check)
                ([x '()] [post #t] [loop-id loop-expr] ...)
                #t
                ([(Id rst post loop-id ...)
                  (if (not (null? x))
                      (values (unsafe-car x)
                              (unsafe-cdr x)
                              post
                              loop-id ...)
                      (if post
                          (let loop ([loop-id loop-id] ...)
                            (if pos-guard
                                (let-values ([(inner-id ...) inner-expr] ...)
                                  (if pre-guard
                                      (begin
                                        (unless (list? tmp)
                                          (raise-argument-error 'tmp "list?" tmp))
                                        (if (null? tmp)
                                            (if post-guard
                                                (loop #f loop-arg ...)
                                                (values #f #f #f loop-id ...))
                                            (if post-guard
                                                (values (unsafe-car tmp) (unsafe-cdr tmp)
                                                        #t
                                                        loop-arg ...)
                                                (values (unsafe-car tmp) (unsafe-cdr tmp)
                                                        #f
                                                        loop-id ...))))
                                      (values #f #f #f loop-id ...)))
                                (values #f #f #f loop-id ...)))
                          (values #f #f #f loop-id ...)))])
                rst
                #t
                (rst post loop-id ...))]])]))))