#lang racket

(module+ main
  (require "../private/main.rkt" "../main.rkt")
  
  (define-syntax (bench stx)
    (syntax-case stx ()
      [(_ form rst ...)
       (let ()
         (with-syntax ([(form ...)
                        (list* (parameterize ([current-optimize values])
                                 (local-expand #'form 'expression '()))
                               (local-expand #'form 'expression '())
                               (syntax->list #'(rst ...)))])
           #'(begin
               (begin
                 (collect-garbage)
                 (collect-garbage)
                 (time
                  (for ([_ (in-range 100000)])
                    form)))
               ...
               (newline))))]))

  (bench
   (for ([a (in-mapped cons
                       (in-mapped cons
                                  (in-range 500)
                                  (in-range 500 1000))
                       (in-mapped cons
                                  (in-range 1000 1500)
                                  (in-range 1500 2000)))])
     a)
   (let ()
     (local-require "../loop.rkt")
     (loop next ([a (in-mapped cons
                               (in-mapped cons
                                          (in-range 500)
                                          (in-range 500 1000))
                               (in-mapped cons
                                          (in-range 1000 1500)
                                          (in-range 1500 2000)))])
           (next))))

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
     (void))
   (let ()
     (local-require "../loop.rkt")
     (loop next ([a (in-filtered
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
           (next))))

  (bench
   (for ([a (in-filtered
             values
             (in-mapped
              (λ (x) (and (odd? x) (+ 2 x)))
              (in-filtered
               values
               (in-mapped
                (λ (x) (and (odd? x) (+ 2 x)))
                (in-filtered
                 values
                 (in-mapped
                  (λ (x) (and (odd? x) (+ 2 x)))
                  (in-range 1000)))))))])
     a)
   (let ()
     (local-require "../loop.rkt")
     (loop next ([a (in-filtered
                     values
                     (in-mapped
                      (λ (x) (and (odd? x) (+ 2 x)))
                      (in-filtered
                       values
                       (in-mapped
                        (λ (x) (and (odd? x) (+ 2 x)))
                        (in-filtered
                         values
                         (in-mapped
                          (λ (x) (and (odd? x) (+ 2 x)))
                          (in-range 1000)))))))])
           (next))))

  (bench
   (for ([a (in-mapped
             (λ (x) (* 2 x))
             (in-filtered
              odd?
              (in-mapped
               (λ (x) (+ 2 x))
               (in-filtered
                odd?
                (in-mapped
                 (λ (x) (+ 2 x))
                 (in-filtered odd? (in-range 1000)))))))])
     a)
   (let ()
     (local-require "../loop.rkt")
     (loop next ([a (in-mapped
                     (λ (x) (* 2 x))
                     (in-filtered
                      odd?
                      (in-mapped
                       (λ (x) (+ 2 x))
                       (in-filtered
                        odd?
                        (in-mapped
                         (λ (x) (+ 2 x))
                         (in-filtered odd? (in-range 1000)))))))])
           (next))))

  (bench
   (for ([(a b) (in-filtered <
                             (in-mapped +
                                        (in-mapped add1 (in-range 1000))
                                        (in-mapped add1 (in-range 1000)))
                             (in-mapped *
                                        (in-mapped add1 (in-range 1000))
                                        (in-mapped sub1 (in-range 1000))))])
     (void))
   (let ()
     (local-require "../loop.rkt")
     (loop next ([(a b) (in-filtered <
                                     (in-mapped +
                                                (in-mapped add1 (in-range 1000))
                                                (in-mapped add1 (in-range 1000)))
                                     (in-mapped *
                                                (in-mapped add1 (in-range 1000))
                                                (in-mapped sub1 (in-range 1000))))])
           (next))))
  )
