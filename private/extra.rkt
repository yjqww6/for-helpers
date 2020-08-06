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
  (syntax-parse pos-guard #:literals (null? not pair?)
    [#t
     #:when (not (null? loop-ids))
     (values (car loop-ids) (map (λ (_) #'#f) loop-ids))]
    [(~or (pair? x:id)
          (not (null? x:id)))
     #:when (for/or ([id (in-list loop-ids)])
              (free-identifier=? id #'x))
     (values (for/first ([id (in-list loop-ids)]
                         #:when (free-identifier=? id #'x))
               id)
             (for/list ([id (in-list loop-ids)])
               (if (free-identifier=? id #'x)
                   #''()
                   #'#f)))]
    [else
     (values #f (map (λ (_) #'#f) loop-ids))]))

(define-sequence-syntax in-nested1
  (syntax-rules ())
  (λ (stx)
    (parameterize ([current-recorded-disappeared-uses '()])
      (syntax-parse stx
        [[(Id:id ...) (_ (I:id ...) S0:expr S1:expr)]
         #:do [(define intro (make-syntax-introducer #t))
               (define s0 (expand-for-clause #'S0 #`[#,(intro #'(I ...)) S0]))
               (define s1 (expand-for-clause #'S1 #`[(Id ...) #,(intro #'S1)]))
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
         (stx-map (λ (_) #'#f) #'(inner-id2 ...))
         #:with (Dis ...) (current-recorded-disappeared-uses)
         #:do [(define-values (omit-state? loop-init1s)
                 (try-omit-state (syntax->list #'(loop-id1 ...)) #'pos-guard1))]
         #:with (loop-init1 ...) loop-init1s

         (cond
           [(not (eq? (syntax-e #'post-guard0) #t))
            #'[(Id ...)
               (:do-in
                ([(next outer-id0 ... ...)
                  (let-values ([(outer-id0 ...) outer-expr0] ...)
                    outer-check0
                    (define (loop loop-id0 ...)
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
                                                (values (if post-guard0 #t 'post) inner-arg2 ...)
                                                (if post-guard0
                                                    (loop loop-arg0 ...)
                                                    (values #f falsy ...))))
                                          (loop loop-arg0 ...)))))
                                (values #f falsy ...)))
                          (values #f falsy ...)))
                    (values loop outer-id0 ... ...))])
                (for-disappeared Dis ...)
                ([state #f] [loop-id1 #f] ... [outer-loop-id1 #f] ... [inner-loop-id0 #f] ... [loop-id0 loop-expr0] ...)
                #t
                ([(state inner-id2 ...)
                  (cond
                    [(eq? state #t)
                     (if pos-guard1
                         (let-values ([(inner-id1 ...) inner-expr1] ...)
                           (if pre-guard1
                               (values #t inner-id2 ...)
                               (next loop-id0 ...)))
                         (next loop-id0 ...))]
                    [(eq? state #f)
                     (next loop-id0 ...)]
                    [else ;'post
                     (if pos-guard1
                         (let-values ([(inner-id1 ...) inner-expr1] ...)
                           (if pre-guard1
                               (values state inner-id2 ...)
                               (values #f falsy ...)))
                         (values #f falsy ...))])])
                state
                #t
                ((if post-guard1 state #f) loop-arg1 ... outer-loop-id1 ... inner-loop-id0 ... loop-id0 ...)
                )]]
           [(or (not (eq? (syntax-e #'post-guard1) #t))
                (not omit-state?))
            #'[(Id ...)
               (:do-in
                ([(next outer-id0 ... ...)
                  (let-values ([(outer-id0 ...) outer-expr0] ...)
                    outer-check0
                    (define (loop loop-id0 ...)
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
                                                (values #t inner-arg2 ...)
                                                (loop loop-arg0 ...)))
                                          (loop loop-arg0 ...)))))
                                (values #f falsy ...)))
                          (values #f falsy ...)))
                    (values loop outer-id0 ... ...))])
                (for-disappeared Dis ...)
                ([state #f] [loop-id1 #f] ... [outer-loop-id1 #f] ... [inner-loop-id0 #f] ... [loop-id0 loop-expr0] ...)
                #t
                ([(state inner-id2 ...)
                  (cond
                    [(and state pos-guard1)
                     (let-values ([(inner-id1 ...) inner-expr1] ...)
                       (if pre-guard1
                           (values #t inner-id2 ...)
                           (next loop-id0 ...)))]
                    [else
                     (next loop-id0 ...)])])
                state
                #t
                (post-guard1 loop-arg1 ... outer-loop-id1 ... inner-loop-id0 ... loop-id0 ...)
                )]]
           [else
            #`[(Id ...)
               (:do-in
                ([(outer-id0 ...) outer-expr0] ...)
                (begin (for-disappeared Dis ...) outer-check0)
                ([loop-id1 loop-init1] ... [outer-loop-id1 #f] ... [inner-loop-id0 #f] ... [loop-id0 loop-expr0] ...)
                #t
                ([(inner-id2 ...)
                  (letrec ([loop (λ (loop-id0 ...)
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
                                                             (values inner-arg2 ...)
                                                             (loop loop-arg0 ...)))
                                                       (loop loop-arg0 ...)))))
                                             (values falsy ...)))
                                       (values falsy ...)))])
                    (cond
                      [pos-guard1
                       (let-values ([(inner-id1 ...) inner-expr1] ...)
                         (if pre-guard1
                             (values inner-id2 ...)
                             (loop loop-id0 ...)))]
                      [else
                       (loop loop-id0 ...)]))])
                #,omit-state?
                #t
                (loop-arg1 ... outer-loop-id1 ... inner-loop-id0 ... loop-id0 ...)
                )]])]))))

(define-sequence-syntax in-lists
  (syntax-rules ())
  (λ (stx)
    (syntax-parse stx
      [[(Id:id) (_ S:expr)] #'[(Id) (in-nested1 (tmp) S (in-list tmp))]])))