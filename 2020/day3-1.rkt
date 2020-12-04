#lang racket/base

(require racket/string
         "./utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare Input

;; 0 is open space, 1 is a tree
(define (process-line s)
  (list->vector (map (λ (c) (if (char=? #\. c) 0 1)) (string->list s))))

(define input (string-split (read-input "input-3.txt")))
(define processed (list->vector (map process-line input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(define WIDTH (vector-length (vector-ref processed 0)))
(define HEIGHT (vector-length processed))
(define RIGHT 3)
(define DOWN 1)
(define TREE 1)
(define OPEN 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution

(let loop ([x 0] [y 0] [trees 0])
  (if (eq? y HEIGHT)
      trees
      (let ([row (vector-ref processed y)]
            [pos (modulo x WIDTH)])
        (loop (+ pos RIGHT) (+ y DOWN) (+ trees (vector-ref row pos))))))
