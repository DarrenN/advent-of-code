#lang racket

(require (only-in racket/list
                  take drop))

(provide group)

(define (group xs ys size)
  (cond [(null? xs) ys]
        [(eq? (length xs) size) (append ys `(,xs))]
        [else (group (drop xs size) (append ys `(,(take xs size))) size)]))
