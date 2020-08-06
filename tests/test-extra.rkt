#lang racket/base
(require "../extra.rkt" (for-syntax racket/base))


(module+ test
  (require rackunit racket/list racket/match)

  (check-equal?
   (for/list ([x (in-lists (in-lists (in-list '(((1 2 3) (4 5 6))
                                                ((7 8 9) (a b c))))))])
     x)
   '(1 2 3 4 5 6 7 8 9 a b c))

  (check-equal?
   (for/list ([x (in-lists
                  (stop-before (in-list
                                '((1 2 3) (4 5 6) (7 8 9) (a b c)))

                               (compose1 not integer? car)))])
     x)
   '(1 2 3 4 5 6 7 8 9))

  (check-equal?
   (for/list ([x (in-lists
                  (stop-after (in-list
                               '((1 2 3) (4 5 6) (7 8 9) (a b c)))

                              (λ (x) (= (car x) 7))))])
     x)
   '(1 2 3 4 5 6 7 8 9))

  (check-equal?
   (for/list ([a (in-nested
                  ([(b) (in-list '((0) (1 2 3) (4 5)))])
                  (in-list b))])
     a)
   (range 6))

  (check-equal?
   (for/list ([x (in-nested ([(a) 
                              (stop-after (in-list
                                           '((1 2 3) (4 5 6) (7 8 9) (a b c)))

                                          (λ (x) (= (car x) 7)))])
                            (in-list a))])
     x)
   '(1 2 3 4 5 6 7 8 9))

  (check-equal?
   (for/list ([a (in-nested
                  ([(b) (in-list '((0) (1 2 3) (4 5)))]
                   [(c) (in-range 2)])
                  (in-list b))])
     a)
   '(0 0 1 2 3 1 2 3 4 5 4 5))

  (check-equal?
   (for/list ([x (in-nested
                  ([(a) (in-range 1)])
                  (in-value a))])
     x)
   '(0))

  (check-equal?
   (let ([a 1])
     (for/list ([x (in-nested ([(a) (in-range 1)])
                              (in-value a))])
       a))
   '(1))
  
  (check-equal?
   (let ()
     (define-sequence-syntax *in-list
       (syntax-rules ())
       (lambda (stx)
         (syntax-case stx (list)
           [[(id) (_ lst-expr)]
            (for-clause-syntax-protect
             #'[(id)
                (:do-in
                 ([(lst) lst-expr])
                 (void)
                 ([lst lst])
                 (pair? lst)
                 ([(id) (car lst)]
                  [(rest) (cdr lst)])
                 #t
                 (not (= (caar id) 7))
                 ((begin (cddr rest) rest)))])]
           [_ #f])))
     (for/list ([x (in-nested ([(a) 
                                (*in-list
                                 '(((1 2 3)) ((4 5 6)) ((7 8 9)) ()))])
                              (in-list (car a)))])
       x))
   '(1 2 3 4 5 6 7 8 9))
  
  (define-namespace-anchor ns)
  #;
  (check-not-exn
   (λ ()
     (parameterize ([current-namespace (namespace-anchor->namespace ns)])
       (expand-syntax
        #'(for ([v (in-nested ([(h) (in-list hs)]
                               [(k v) (in-hash h)]
                               [(l) (in-list v)])
                              (in-value l))])
            v))))
   )
  )
