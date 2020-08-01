#lang racket/base
(require "../extra.rkt")

(module+ test
  (require rackunit racket/list racket/match)

  (check-equal?
   (for/list ([x (in-lists (in-lists (in-list '(((1 2 3) (4 5 6))
                                                ((7 8 9) (a b c))))))])
     x)
   '(1 2 3 4 5 6 7 8 9 a b c)))
