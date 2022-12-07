#lang racket/base

(require
 racket/list
 (only-in racket/string string-split)
 "../../lib/timed.rkt")

; Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock

(define table
  (hash
   'A 1 ; rock
   'X 1
   'B 2 ; paper
   'Y 2
   'C 3 ; scissors
   'Z 3))

(define table-wins
  (hash
   'A 2 ; rock
   'X 2
   'C 1 ; scissors
   'Z 1
   'B 0 ; paper
   'Y 0))

(define table-wins-reverse
  (hash
   2 'A ; rock
   2 'X
   1 'C ; scissors
   1 'Z
   0 'B ; paper
   0 'Y))

(define (play input)
  (define (loop ls s)
    (if (null? ls)
        s
        (let* ([p (if (> (length ls) 2) (take ls 2) ls)]
               [rs (if (>= (length ls) 2) (drop ls 2) '())]
               [l (hash-ref table-wins (car p))]
               [r (hash-ref table-wins (cadr p))]
               [points (hash-ref table (cadr p))])
          (cond [(and (> l 0) (zero? r)) (loop rs (+ s points 6))] ; paper/rock win
                [(and (zero? l) (> r 0)) (loop rs (+ s points))] ; paper/rock lose
                [(< l r) (loop rs (+ s points 6))] ; win
                [(> l r) (loop rs (+ s points))] ; lose
                [else (loop rs (+ s points 3))])))) ; draw
  (loop input 0))

(define (play2 input)
  (define (loop ls s)
    (if (null? ls)
        s
        (let* ([p (if (> (length ls) 2) (take ls 2) ls)]
               [rs (if (>= (length ls) 2) (drop ls 2) '())]
               [l (hash-ref table-wins (car p))]
               [r (cadr p)])
          (cond [(eq? r 'X) (cond [(= 2 l) (loop rs (+ s 3))]
                                  [(= 1 l) (loop rs (+ s 2))]
                                  [(= 0 l) (loop rs (+ s 1))])] ; lose
                [(eq? r 'Y) (loop rs (+ s (hash-ref table (car p)) 3))] ; draw
                [else (cond [(= 2 l) (loop rs (+ s 2 6))]
                            [(= 1 l) (loop rs (+ s 1 6))]
                            [(= 0 l) (loop rs (+ s 3 6))])])))) ; win
  (loop input 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require (only-in racket/file file->list))
  (define input '(A Y B X C Z))
  (define input2 '(C Z))

    (define-values (result dur)
    (timed-apply  play2
                 (list
                  (file->list "./input.txt")
                  ;input
                  )))
      (println (format "~a : ~a" result dur)))
