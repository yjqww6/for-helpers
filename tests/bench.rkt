#lang racket

(module+ main
  (require "../private/main.rkt" "../main.rkt")
  
  (define-syntax (bench stx)
    (syntax-case stx ()
      [(_ form)
       (let ()
         (with-syntax ([(form ...)
                        (list (parameterize ([current-optimize values])
                                (local-expand #'form 'expression '()))
                              (local-expand #'form 'expression '()))])
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

  (bench
   (for ([a (in-filtered
             positive?
             (in-filtered
              even?
              (in-mapped
               (λ (x) (- x 1))
               (in-mapped
                (λ (x) (- x 1))
                (in-filtered
                 positive?
                 (in-filtered
                  even?
                  (in-mapped
                   (λ (x) (- x 1))
                   (in-mapped
                    (λ (x) (- x 1))
                    (in-range 1000)))))))))])
     a))
  )
