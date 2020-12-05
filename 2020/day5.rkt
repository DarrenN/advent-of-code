#lang racket/base

(require racket/list
         racket/string
         racket/vector
         "./utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare Input

(define (get-boarding-passes input)
  (string-split (read-input input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution 1

#|
    BFFFBBFRRR: row 70, column 7, seat ID 567.
    FFFBBBFRRR: row 14, column 7, seat ID 119.
    BBFFBBFRLL: row 102, column 4, seat ID 820.
|#

(define ROWS (for/vector ([i (in-range 128)]) i))
(define COLS (for/vector ([i (in-range 8)]) i))

(define (b? c) (char=? #\B c)) ; upper
(define (f? c) (char=? #\F c)) ; lower
(define (r? c) (char=? #\R c)) ; upper
(define (l? c) (char=? #\L c)) ; lower

(define t1 "FBFBBFF") ;; -> row 44
(define t2 "RLR") ;; -> col 5
(define t3 "FBFBBFFRLR")

(define (binary rows input)
  (for/fold ([rs rows])
            ([c (string->list input)])
    (define len (vector-length rs))
    (define split-ref (/ len 2))
    (define-values (lower upper)
      (if (= 2 len)
          (values (vector (vector-ref rs 0))
                  (vector (vector-ref rs 1)))
          (vector-split-at rs split-ref)))
    (cond
      [(or (f? c) (l? c)) lower]
      [(or (b? c) (r? c)) upper])))

(define (split-pass pass)
  (let ([cs (string->list pass)])
    (values (apply string (take cs 7))
            (apply string (drop cs 7)))))

(define (day5 [input "input-5.txt"])
  (define passes (get-boarding-passes input))
  (car (sort
        (map (λ (pass)
               (define-values (rs cs) (split-pass pass))
               (define-values (row col)
                 (values (vector-ref (binary ROWS rs) 0)
                         (vector-ref (binary COLS cs) 0)))
               (+ (* row 8) col))
             passes) >)))
