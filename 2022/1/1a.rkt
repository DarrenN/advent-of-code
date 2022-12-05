#lang racket/base

(require
 racket/list
 racket/vector
 (only-in racket/string string-split)
 "../../lib/timed.rkt")

(define (tops ts sum)
  (if (> sum (last ts))
      (take (sort (append ts `(,sum)) >) 3)
      ts))

(define (make-calorie-count lst)
  (define ts '(0 0 0))
  ;(define v (make-vector 500 0)) ;; vector of combined calorie counts
  (define p 0) ;; position
  (define c 0) ;; combined calories
  (for ([i (in-list lst)])
    (cond [(equal? i "")
           (begin0 ;(vector-set! v p c)
             (set! ts (tops ts c))
             (set! c 0)
             (set! p (add1 p)))]
          [else (begin0
                    (let ([n (string->number i)])
                      ;(vector-set! v p (+ c n))
                      (set! c (+ c n))))]))
  (values '() ts))

(define (make-calorie-count2 lst)
  (define (top-sort cals tops)
    (if (> cals (last tops))
        (take (sort (cons cals tops) >) 3)
        tops))

  (for/fold ([tops '(0 0 0)]
             [cals 0]
             #:result (top-sort cals tops))
            ([l (in-list lst)])
    (if (equal? l "")
        (values (top-sort cals tops) 0)
        (values tops (+ cals (string->number l))))))

(define (get-tops2 n ts)
  (for/sum ([i (in-list (take ts n))]) i))

(define (get-tops n cs)
  (for/sum ([i (in-vector (vector-take cs n))]) i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require (only-in racket/file file->lines))
  (define input (string-split "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000" #px"\\s"))

(define-values (result dur)
    (timed-apply (λ (ls) (get-tops2 3 (make-calorie-count2 ls)))
                 (list
                  (file->lines "./input.txt")
                  ;input
                  )))
  (println (format "~a : ~a" result dur))

  )
