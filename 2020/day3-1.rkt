#lang racket/base

(require racket/string
         "./utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare Input

;; 0 is open space, 1 is a tree
(define (process-line s)
  (list->vector (map (λ (c) (if (char=? #\. c) 0 1)) (string->list s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution 1

(define (three-one)
  (define input (string-split (read-input "input-3.txt")))
  (define processed (list->vector (map process-line input)))
  (define WIDTH (vector-length (vector-ref processed 0)))
  (define HEIGHT (vector-length processed))
  (define RIGHT 3)
  (define DOWN 1)

  (let loop ([x 0] [y 0] [trees 0])
    (if (eq? y HEIGHT)
        trees
        (let ([row (vector-ref processed y)]
              [pos (modulo x WIDTH)])
          (loop (+ pos RIGHT) (+ y DOWN) (+ trees (vector-ref row pos)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution 2

#|
   We'll store the right/down rules in a struct along with the
   x/y coords to check and a running count of trees

    Right 1, down 1.
    Right 3, down 1. (This is the slope you already checked.)
    Right 5, down 1.
    Right 7, down 1.
    Right 1, down 2.
|#

(struct pos (right down x y trees) #:transparent)

(define positions (list (pos 1 1 0 0 0)
                        (pos 3 1 0 0 0)
                        (pos 5 1 0 0 0)
                        (pos 7 1 0 0 0)
                        (pos 1 2 0 0 0)))

(define (update-pos p x y ts)
  (struct-copy pos p
               [x x]
               [y y]
               [trees ts]))

(define (three-two [input "input-3.txt"])
  ;; process the input into a grid
  (define i (string-split (read-input input)))
  (define processed (list->vector (map process-line i)))
  (define width (vector-length (vector-ref processed 0)))
  (define height (vector-length processed))

  ;; Loop over the grid once, pulling values and accumulating in pos structs
  (for/fold ([acc positions]
             #:result (foldl
                       (λ (n prod) (* (pos-trees n) prod)) 1 acc))
            ([i (in-range height)])
    ;; Process each pos struct
    (for/list ([p acc])
      (if (> (pos-y p) height)
          p
          (let ([row (vector-ref processed (pos-y p))]
                [pos-x (modulo (pos-x p) width)])
            (update-pos p
                        (+ pos-x (pos-right p))
                        (+ (pos-y p) (pos-down p))
                        (+ (pos-trees p)
                           (vector-ref row pos-x))))))))
