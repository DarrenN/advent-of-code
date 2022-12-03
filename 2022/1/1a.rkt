#lang racket/base

(require
 racket/vector
 (only-in racket/string string-split)
 "../../lib/timed.rkt")

(define input "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(define (make-calorie-count lst)
  (define v (make-vector 500 0)) ;; vector of combined calorie counts
  (define p 0) ;; position
  (define c 0) ;; combined calories
  (for ([i (in-list lst)])
    (cond [(equal? i "")
           (begin0 (vector-set! v p c)
             (set! c 0)
             (set! p (add1 p)))]
          [else (begin0
                    (let ([n (string->number i)])
                      (vector-set! v p (+ c n))
                      (set! c (+ c n))))]))
  (vector-sort! v >)
  v)

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
    (timed-apply (λ (ls) (get-tops 3 (make-calorie-count ls)))
                 (list
                  (file->lines "./input.txt")
                  ;input
                  )))
  (println (format "~a : ~a" result dur)))
