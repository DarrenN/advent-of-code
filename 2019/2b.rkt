#lang racket/base

(require racket/format
         racket/vector
         "intcode-1.rkt")

#|
Instructions: (OP RPOS RPOS WPOS) '(1 10 20 30) =
add pos 10 to pos 20 write result to pos 30


1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.

|#

(define OP-ADD 1)
(define OP-MULTIPLY 2)
(define OP-FINISH 99)

(define chunklen 4)

(define (access v pos)
  (let ([max-len (+ pos (- chunklen 1))])
    (if (> (vector-length v) max-len)
        (values (vector-ref v pos)
                (vector-ref v (+ pos 1))
                (vector-ref v (+ pos 2))
                (vector-ref v (+ pos 3)))
        (values (vector-ref v pos) null null null))))

(define (apply-register-fn fn rpos1 rpos2 wpos v)
  (let ([rval1 (vector-ref v rpos1)]
        [rval2 (vector-ref v rpos2)])
    (vector-set! v wpos (fn rval1 rval2)))
  v)

(define (compute v [pos 0])
  (define-values (opcode rpos1 rpos2 wpos) (access v pos))
  (cond [(= opcode OP-ADD)
         (compute (apply-register-fn + rpos1 rpos2 wpos v) (+ pos chunklen))]
        [(= opcode OP-MULTIPLY)
         (compute (apply-register-fn * rpos1 rpos2 wpos v) (+ pos chunklen))]
        [(= opcode OP-FINISH) v])
  v)

;; Copy the vector (because we mutate it) and insert a noun (m) and verb (n)
;; then compute and return the output (position 0)
(define (try-program program m n)
  (let ([p2 (vector-copy program)])
    (vector-set! p2 1 m)
    (vector-set! p2 2 n)
    (vector-ref (compute p2) 0)))

;; Brute force program inputs until we find an output
(for ([m (in-range 99)])
  (for ([n (in-range 99)])
    (let ([output (try-program program0 m n)])
      (when (eq? output 19690720)
        ;; AoC wants (+ (* 100 m) n) as an answer
        (displayln (~a output " " m " " n " " (+ (* 100 m) n)))))))

(module+ test
  (require rackunit)

  (check-equal? (compute (vector 1 0 0 0 99)) (vector 2 0 0 0 99)
                "incorrect compute")

  (check-equal? (compute (vector 2 3 0 3 99)) (vector 2 3 0 6 99)
                "incorrect compute")

  (check-equal? (compute (vector 2 3 3 4 99 0)) (vector 2 3 3 4 16 0)
                "incorrect compute")

  (check-equal? (compute (vector 1 1 1 4 99 5 6 0 99))
                (vector 30 1 1 4 2 5 6 0 99)
                "incorrect compute")

  (check-equal? (compute (vector 2 4 4 5 99 0)) (vector 2 4 4 5 99 9801)
                "incorrect compute")

  (check-equal? (compute (vector 1 9 10 3 2 3 11 0 99 30 40 50))
                (vector 3500 9 10 70 2 3 11 0 99 30 40 50)
                "incorrect compute"))
