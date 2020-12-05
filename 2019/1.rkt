#lang racket/base

(require "fuel-input.rkt")

#|

For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
For a mass of 1969, the fuel required is 654.
For a mass of 100756, the fuel required is 33583.

The Fuel Counter-Upper needs to know the total fuel requirement. To find it, individually calculate the fuel needed for the mass of each module (your puzzle input), then add together all the fuel values.

|#


(define (calc n) (- (floor (/ n 3)) 2))
(define (expanded-calc n sum)
  (let ([m (calc n)])
    (if (> m 0)
        (expanded-calc m (+ sum m))
        sum)))


(define total-fuel-a (foldl (λ (n sum) (+ sum (calc n))) 0 fuel-input))
(define total-fuel-b (foldl expanded-calc 0 fuel-input))

(println total-fuel-a)
(println total-fuel-b)
