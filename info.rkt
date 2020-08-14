#lang info
(define collection "for-helpers")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "sandbox-lib"))
(define compile-omit-paths '("tests"))
(define scribblings '(("scribblings/for-helpers.scrbl" ())))
(define pkg-desc "Helper macros for racket/for")
(define version "0.1")
(define pkg-authors '(yjqww6))
