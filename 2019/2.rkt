#lang racket/base

(require "intcode-1.rkt")

#|
Instructions: (OP RPOS RPOS WPOS) '(1 10 20 30) = add pos 10 to pos 20 write result to pos 30


1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.

|#

(define OP-ADD 1)
(define OP-MULTIPLY 2)
(define OP-FINISH 99)

(define chunklen 4)
(define *pos* 0)

(define (access v)
  (let ([max-len (+ *pos* (- chunklen 1))])
    (if (> (vector-length v) max-len)
        (values (vector-ref v *pos*)
                (vector-ref v (+ *pos* 1))
                (vector-ref v (+ *pos* 2))
                (vector-ref v (+ *pos* 3)))
        (values (vector-ref v *pos*) null null null))))

(define (add-registers rpos1 rpos2 wpos v)
  (let ([rval1 (vector-ref v rpos1)]
        [rval2 (vector-ref v rpos2)])
    (vector-set! v wpos (+ rval1 rval2)))
  v)

(define (mult-registers rpos1 rpos2 wpos v)
  (let ([rval1 (vector-ref v rpos1)]
        [rval2 (vector-ref v rpos2)])
    (vector-set! v wpos (* rval1 rval2)))
  v)

(define (step v)
  (if (> (vector-length v) (- *pos* 1))
      (begin
        (set! *pos* (+ *pos* chunklen))
        (compute v))
      v))

(define (reset-and-compute v)
  (set! *pos* 0)
  (compute v))

(define (compute v)
  (define-values (opcode rpos1 rpos2 wpos) (access v))
  (cond [(= opcode OP-ADD) (step (add-registers rpos1 rpos2 wpos v))]
        [(= opcode OP-MULTIPLY) (step (mult-registers rpos1 rpos2 wpos v))]
        [(= opcode OP-FINISH) (begin (println 'DONE) v)]
        [else v]))

(define p0 (vector 1 0 0 0 99))
(define p1 (vector 2 3 0 3 99))
(define p2 (vector 2 3 3 4 99 0))
(define p3 (vector 1 1 1 4 99 5 6 0 99))

(map reset-and-compute (list p0 p1 p2 p3))
