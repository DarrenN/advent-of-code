#lang racket/base

(require (only-in racket/list
                  range)
         (only-in racket/string
                  string-split
                  string-trim)
         (only-in racket/file
                  file->lines)
         (only-in "../lib/timed.rkt"
                  print-timed))

(define (get-x pos)
  (vector-ref pos 0))

(define (get-y pos)
  (vector-ref pos 1))

(define (interpolate2 p1 p2)
  (let ([x1 (get-x p1)]
        [x2 (get-x p2)]
        [y1 (get-y p1)]
        [y2 (get-y p2)])
    (cond [(eq? x1 x2)
           (if (< y1 y2)
               (for/list ([y (in-range y1 (add1 y2))])
                 (vector x1 y))
               (for/list ([y (in-list (reverse (range y2 (add1 y1))))])
                 (vector x1 y)))]
          [(eq? y1 y2)
           (if (< x1 x2)
               (for/list ([x (in-range x1 (add1 x2))])
                 (vector x y1))
               (for/list ([x (in-list (reverse (range x2 (add1 x1))))])
                 (vector x y1)))]
          [else
           (for/list
               ([x
                 (if (< x1 x2)
                     (in-range x1 (add1 x2))
                     (in-list (reverse (range x2 (add1 x1)))))]
                [y
                 (if (< y1 y2)
                     (in-range y1 (add1 y2))
                     (in-list (reverse (range y2 (add1 y1)))))])
             (vector x y))])))

(define (process-range s)
  (map (λ (s) (list->vector
               (map (λ (s) (string->number (string-trim s)))
                    (string-split s ","))))
       (string-split s "->")))

(define (fill-grid grid p1 p2)
  (let ([points (interpolate2 p1 p2)])
    (for/fold ([g grid])
              ([p points])
      (hash-update g p add1 0))))

(define (count-intersections grid)
  (for/sum ([v (hash-values grid)])
    (if (> v 1)
        1
        0)))

(define (find-vents ranges)
  (count-intersections
   (for/fold ([g (hash)])
             ([rng ranges])
     (fill-grid g (car rng) (cadr rng)))))

(module+ main
  (define ranges (map process-range (file->lines "./input.txt")))
  (print-timed find-vents (list ranges)))

(module+ test
  (require racket/string
           racket/file)

  (define input #<<GRD
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
GRD
    )
  #|
  (interpolate2 (vector 1 1) (vector 1 3))
  (interpolate2 (vector 1 3) (vector 1 1))
  (interpolate2 (vector 9 7) (vector 7 7))

  (interpolate2 (vector 1 1) (vector 3 3))
  (interpolate2 (vector 9 7) (vector 7 9))
  |#
  (define ranges (map process-range (string-split input "\n")))
  (find-vents ranges)
  )
