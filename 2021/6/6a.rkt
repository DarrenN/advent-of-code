#lang racket/base

(require racket/vector
         (only-in racket/file
                  file->lines)
         (only-in racket/string
                  string-split)
         (only-in "../lib/timed.rkt"
                  print-timed))

(define (get-input path)
  (map string->number (string-split (car (file->lines path)) ",")))

(define school (make-vector 9 0))

(define (increment v pos)
  (vector-set! v pos (add1 (vector-ref v pos))))

(define (add-day! v)
  (define o (vector-copy v))
  (vector-set! v 0 (vector-ref o 1))
  (vector-set! v 1 (vector-ref o 2))
  (vector-set! v 2 (vector-ref o 3))
  (vector-set! v 3 (vector-ref o 4))
  (vector-set! v 4 (vector-ref o 5))
  (vector-set! v 5 (vector-ref o 6))
  (vector-set! v 6 (+ (vector-ref o 7) (vector-ref o 0)))
  (vector-set! v 7 (vector-ref o 8))
  (vector-set! v 8 (vector-ref o 0)))

(define (seed-school seed v)
  (for ([s (in-list seed)])
    (case s
      [(0) (increment v 0)]
      [(1) (increment v 1)]
      [(2) (increment v 2)]
      [(3) (increment v 3)]
      [(4) (increment v 4)]
      [(5) (increment v 5)]
      [(6) (increment v 6)])))

(define (grow-fish seed v days)
  (seed-school seed v)
  (for ([_ (in-range days)])
    (add-day! v))
  (for/sum ([n (in-vector v)]) n))

(module+ main
  (define seed (get-input "./input.txt"))
  (define school1 (make-vector 9 0))
  (print-timed grow-fish (list seed school1 80))

  (define school2 (make-vector 9 0))
  (print-timed grow-fish (list seed school2 256)))

(module+ test
  (require racket/vector)

  (define seed '(3 4 3 1 2))
  (define school (make-vector 9 0))
  
  (grow-fish seed school 80)
  school)
