#lang racket/base

(require (only-in racket/file
                  file->lines)
         (only-in racket/function
                  identity)
         (only-in racket/vector
                  vector-set*!)
         (only-in "../lib/timed.rkt"
                  print-timed))

(define input (file->lines "./input.txt"))

(define (get-bit bs pos)
  (values (vector-ref bs (* 2 pos)) (vector-ref bs (add1 (* 2 pos)))))

(define (set-bit! bs pos c0 c1)
  (vector-set*! bs (* 2 pos) c0 (add1 (* 2 pos)) c1))

(define (count-significant-bits input bs)
  (define (update-bit! bs pos fn0 fn1)
    (let-values ([(c0 c1) (get-bit bs pos)])
      (set-bit! bs pos (fn0 c0) (fn1 c1))
      (get-bit bs pos)))

  (for ([b (in-list input)])
    (for ([c (in-list (string->list b))]
          [p (in-range 0 (length (string->list b)))])
      (if (eq? #\0 c)
          (update-bit! bs p add1 identity)
          (update-bit! bs p identity add1))))

  bs)

(define (combined-significant-bits bs)
  (let-values ([(ms ls)
                (for/fold ([ms ""]
                           [ls ""])
                          ([p (in-range 0 (/ (vector-length bs) 2))])
                  (let-values ([(c0 c1) (get-bit bs p)])
                    (if (> c0 c1)
                        (values (string-append ms "0") (string-append ls "1"))
                        (values (string-append ms "1") (string-append ls "0")))))])
    (values (string->number ms 2)
            (string->number ls 2))))

(define (significant-bits input)
  (let ([bits
         (count-significant-bits
          input
          (make-vector (* 2 (string-length (car input))) 0))])
    (let-values ([(ms ls) (combined-significant-bits bits)])
      (* ms ls))))

(module+ main
  (print-timed significant-bits `(,input)))

(module+ test
  (require (only-in racket/file
                    file->lines)
           racket/vector
           racket/function)

  (define input '("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"))
  (define input2 '("110011110101"))
  (print-timed significant-bits `(,input)))
