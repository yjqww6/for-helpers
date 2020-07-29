#lang racket

(module+ main
  (require "../main.rkt")
  (define-syntax-rule (bench form)
    (begin
      (collect-garbage)
      (collect-garbage)
      (time
       (for ([_ (in-range 1000000)])
         form))))

  #;
  (bench
   (map cons
        (map cons (range 50) (range 50 100))
        (map cons (range 100 150) (range 150 200))))
  
  (bench
   (for ([a (in-mapped cons
                       (in-mapped cons
                                  (in-range 50)
                                  (in-range 50 100))
                       (in-mapped cons
                                  (in-range 100 150)
                                  (in-range 150 200)))])
     a))


  (bench
   (for ([a (in-filtered
             values
             (in-mapped
              cons
              (in-mapped cons
                         (in-range 50)
                         (in-range 50 100))
              (in-mapped cons
                         (in-range 100 150)
                         (in-range 150 200))))])
     a))
  )
