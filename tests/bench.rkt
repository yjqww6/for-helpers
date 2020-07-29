#lang racket

(module+ main
  (require "../main.rkt")
  (define-syntax-rule (bench form)
    (begin
      (collect-garbage)
      (collect-garbage)
      (time
       (for ([_ (in-range 100000)])
         form))))

  (bench
   (for ([a (in-mapped cons
                       (in-mapped cons
                                  (in-range 500)
                                  (in-range 500 1000))
                       (in-mapped cons
                                  (in-range 1000 1500)
                                  (in-range 1500 2000)))])
     a))


  (bench
   (for ([a (in-filtered
             values
             (in-mapped
              cons
              (in-mapped cons
                         (in-range 500)
                         (in-range 500 1000))
              (in-mapped cons
                         (in-range 1000 1500)
                         (in-range 1500 2000))))])
     a))

  (bench
   (for ([a
          (in-filtered
           even?
           (in-filtered
            even?
            (in-filtered
             even?
             (in-mapped
              +
              (in-mapped +
                         (in-range 500)
                         (in-range 500 1000))
              (in-mapped +
                         (in-range 1000 1500)
                         (in-range 1500 2000))))))])
     (void)))
  )
