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
         ["nested inlists"
          (for ([x (in-lists (in-lists (in-list l)))])
            x)])
  )