#lang racket/base
(require "../extra.rkt")

(module+ main

  (define-syntax-rule (bench name
                             [case form] ...)
    (begin
      (printf "~a:~%" name)
      (begin
        (printf "~a: " case)
        (collect-garbage)
        (collect-garbage)
        (time (for ([_ (in-range 1000000)]) form)))
      ...
      (newline)))
  
  (define l '(((1 2 3) (4 5 6))
              ((7 8 9) (a b c))))

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