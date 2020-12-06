#lang racket/base

(require racket/string
         "./utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare Input

(define (get-ids input)
  (map string->number (string-split (read-input input))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution 1

;; We use a loop instead of a foldl so we can terminate early
(define (day1-one [input "input-1.txt"])
  (define ids (get-ids input))
  (define lookup (make-hash (map (λ (i) (cons i #t)) ids)))
  (let loop ([product 0] [ls ids])
    (if (null? ls)
        product
        (let* ([id (car ls)]
               [diff (- 2020 id)])
          (if (hash-has-key? lookup diff)
              (* id diff)
              (loop 0 (cdr ls)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution 2

;; We use a loops so we can terminate early, which means we can solve this
;; super efficiently with minimal iterations
(define (day1-two [input "input-1.txt"])
  (define ids (get-ids input))
  (define lookup (make-hash (map (λ (i) (cons i #t)) ids)))
  (let loop ([product 0] [ls ids])
    (if (null? ls)
        product
        (let inner-loop ([inner-prod 0] [js (cdr ls)])
          (if (null? js)
              (loop inner-prod (cdr ls))
              (let* ([i (car ls)]
                     [j (car js)]
                     [k (- 2020 (+ i j))])
                (if (hash-has-key? lookup k)
                    (loop (* i j k) '())
                    (inner-loop 0 (cdr js)))))))))
