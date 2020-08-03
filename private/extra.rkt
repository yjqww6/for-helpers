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

(define-sequence-syntax in-nested1
  (syntax-rules ())
  (λ (stx)
    (parameterize ([current-recorded-disappeared-uses '()])
      (syntax-parse stx
        [[(Id:id ...) (_ (I:id ...) S0:expr S1:expr)]
         #:do [(define s0 (expand-for-clause #'S0 #'[(I ...) S0]))
               (define s1 (expand-for-clause #'S1 #'[(Id ...) S1]))
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
         #:with (falsy ...)
         (stx-map (λ (_) #'#f) #'(inner-id2 ...))
         #:with (Dis ...) (current-recorded-disappeared-uses)
         #:with state (generate-temporary 'state)

         #'[(Id ...)
            (:do-in
             ([(outer-id0 ...) outer-expr0] ...)
             (begin (for-disappeared Dis ...) outer-check0)
             ;;state #f : uninit, 0 : ok, 1 : post-guard0 failed, 2 : loop0
             ([state #f] [loop-id1 #f] ... [outer-loop-id1 #f] ... [inner-loop-id0 #f] ... [loop-id0 loop-expr0] ...)
             #t
             ([(state inner-loop-id1 ... loop-id1 ... outer-loop-id1 ... inner-loop-id0 ... loop-id0 ...)
               (cond
                 [(eq? state 1)
                  (if pos-guard1
                      (let-values ([(inner-id1 ...) inner-expr1] ...)
                        (if pre-guard1
                            (values state inner-id2 ...)
                            (values #f falsy ...)))
                      (values #f falsy ...))]
                 [else
                  (let go ([state state] [loop-id0 loop-id0] ...)
                    (cond
                      [(not state)
                       (go 2 loop-id0 ...)]
                      [(eq? state 0)
                       (if pos-guard1
                           (let-values ([(inner-id1 ...) inner-expr1] ...)
                             (if pre-guard1
                                 (values 0 inner-id2 ...)
                                 (go 2 loop-arg0 ...)))
                           (go 2 loop-arg0 ...))]
                      [(eq? state 1)
                       (if pos-guard1
                           (let-values ([(inner-id1 ...) inner-expr1] ...)
                             (if pre-guard1
                                 (values state inner-id2 ...)
                                 (values #f falsy ...)))
                           (values #f falsy ...))]
                      [(eq? state 2)
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
                                                 (values (if post-guard0 0 1) inner-id2 ...)
                                                 (if post-guard0
                                                     (go 2 loop-arg0 ...)
                                                     (values #f falsy ...))))
                                           (go 2 loop-arg0 ...)))))
                                 (values #f falsy ...)))
                           (values #f falsy ...))]))])])
             state
             #t
             ((if post-guard1 state #f) loop-arg1 ... outer-loop-id1 ... inner-loop-id0 ... loop-id0 ...)
             )]]))))

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