#lang racket/base
(require "../extra.rkt" racket/list)

(module+ main

  (define-syntax-rule (bench name
                             [case form] ...)
    (begin
      (printf "~a:~%" name)
      (begin
        (printf "~a: " case)
        (collect-garbage)
        (collect-garbage)
        (time (for ([_ (in-range 100000)]) form)))
      ...
      (newline)))
  
  (define l (list (list (range 50) (range 50))
                  (list (range 50) (range 50) (range 50))))

  (bench "in-lists"
         ["append"
          (for ([x (in-list (apply append (apply append l)))])
            x)]
         ["for*"
          (for* ([x (in-list l)]
                 [y (in-list x)]
                 [z (in-list y)])
            z)]
         ["nested in-lists"
          (for ([x (in-lists (in-lists (in-list l)))])
            x)]
         ["in-nested"
          (for ([x (in-nested ([(a) (in-list l)]
                               [(b) (in-list a)])
                              (in-list b))])
            x)]
         ["loop"
          (begin
            (local-require "../loop.rkt")
            (loop loop ([x (in-nested ([(a) (in-list l)]
                                       [(b) (in-list a)])
                                      (in-list b))])
                  (loop)))])

  (define hs (list (for/hash ([i (in-range 50)])
                     (values i (range i)))
                   (for/hash ([i (in-range 50)])
                     (values i (range i)))))

  (bench "bench2"
         ["for*"
          (for* ([h (in-list hs)]
                 [(k l) (in-hash h)]
                 [i (in-list l)]
                 [v (in-value i)])
            i)]
         ["in-nested"
          (for ([v (in-nested ([(h) (in-list hs)]
                               [(k l) (in-hash h)]
                               [(i) (in-list l)])
                              (in-value i))])
            v)]
         ["loop"
          (begin
            (local-require "../loop.rkt")
            (loop loop ([v (in-nested ([(h) (in-list hs)]
                                       [(k l) (in-hash h)]
                                       [(i) (in-list l)])
                                      (in-value i))])
                  (loop)))])
  )