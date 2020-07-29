#lang racket/base
(require "../main.rkt")

(module+ test
  (require rackunit racket/list racket/match)

  (check-equal?
   (for/list ([(a b) (in-filtered <
                                  '(1 3 5 -1)
                                  (in-list '(6 4 2 0)))])
     (cons a b))
   '((1 . 6) (3 . 4) (-1 . 0)))

  (check-equal?
   (for/list ([(a b) (in-mapped (match-lambda [(cons a b) (values a b)]) '((1 . 2) (3 . 4) (4 . 5)))])
     (cons a b))
   '((1 . 2) (3 . 4) (4 . 5)))

  (check-equal?
   (for/list ([a (in-mapped add1 (in-range 3))]
              [b (in-mapped sub1 (in-naturals))])
     (cons a b))
   '((1 . -1) (2 . 0) (3 . 1)))

  (check-equal?
   (for/list ([a (in-filtered odd? (in-range 5))]
              [b (in-filtered even? (in-range 5))])
     (cons a b))
   '((1 . 0) (3 . 2)))

  (check-equal?
   (for/list ([a (in-filtered values (in-mapped even? (in-range 10)))])
     a)
   (filter-map even? (range 10)))

  (let ([l 
         (map cons
              (map cons (range 5) (range 5 10))
              (map cons (range 10 15) (range 15 20)))])
  
    (check-equal?
     (for/list ([a (in-mapped cons
                              (in-mapped cons
                                         (in-range 5)
                                         (in-range 5 10))
                              (in-mapped cons
                                         (in-range 10 15)
                                         (in-range 15 20)))])
       a)
     l)
    (check-equal?
     (for/list ([a (in-mapped (λ (a b c d) (cons (cons a b) (cons c d)))
                              (in-range 5)
                              (in-range 5 10)
                              (in-range 10 15)
                              (in-range 15 20))])
       a)
     l))

  (check-equal?
   (for/list ([a (in-mapped add1
                            (in-filtered odd?
                                         (in-mapped add1
                                                    (in-range 10))))])
     a)
   (map add1 (filter odd?
                     (map add1
                          (range 10)))))

  (check-equal?
   (for/list ([l (in-filtered
                  list?
                  (in-mapped cons
                             '(1 2 3 4 5)
                             '(a () a () a)))])
     l)
   '((2) (4)))

  (check-equal?
   (for/list ([l (in-filtered values
                              (in-filtered
                               list?
                               (in-mapped cons
                                          '(1 2 3 4 5)
                                          '(a () a () a))))])
     l)
   '((2) (4)))
  )