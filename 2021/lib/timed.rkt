#lang racket/base

(provide timed-apply
         print-timed)

(define (timed-apply fn args)
  (let ([start (current-inexact-monotonic-milliseconds)])
    (define result (apply fn args))
    (values result (- (current-inexact-monotonic-milliseconds) start))))


(define (print-timed fn args)
  (define-values (result ms)
    (timed-apply fn args))

  (printf "Result: ~a~n" result)
  (printf "Duration in ms: ~a~n" ms))


(module+ test
  (define (load)
    (for/fold ([sum 1])
              ([n (in-range 1 1000000)])
      (+ sum n 1.5)))
  (define (load2)
    (for/fold ([sum 1])
              ([n (in-range 1 1000)])
      (+ sum n 1.5)))
  (timed-apply load '())
  (timed-apply load2 '())
  (print-timed load2 '()))
