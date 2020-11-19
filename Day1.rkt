#lang racket

(require math/base)

(define (read-masses)
  (map string->number
       (file->lines "Day1.txt")))

(define (fuel mass)
  (- (quotient mass 3) 2))

(define (fuel-rec mass)
  (if (positive? mass)
      (+ mass
         (fuel-rec (fuel mass)))
      0))

(letrec ((masses (read-masses)))
  (begin (displayln (sum (map fuel masses))))
  (begin (displayln (- (sum (map fuel-rec masses))
                       (sum masses)))))