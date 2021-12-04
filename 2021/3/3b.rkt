#lang racket/base

#|
Get significant bits count from input
loop over input again filtering items into most-common-bit and least-common-bit lists based on first value
run two independent loops over the two remaining lists narrowing them down to their last values


Could:

run the first loop, filtering inputs into a list of c0 and c1
list with the greater length is the mcb and lesser is lcb
re-run the same loop as the first, on both mcb and lcb
repeat until a single value in each list

!! will need to pass in which bit to inspect in each loop
|#

(require
 (only-in racket/file
          file->lines)
 (only-in "../lib/timed.rkt"
          print-timed))

(define input (file->lines "./input.txt"))
(define bitput (map (λ (l) (list->vector (string->list l))) input))

(define (filter-bits bs pos)
  (let-values ([(0s 1s)
                (for/fold ([c0 '()]
                           [c1 '()])
                          ([b (in-list bs)])
                  (let ([v (vector-ref b pos)])
                    (if (eq? #\0 v)
                        (values (cons b c0) c1)
                        (values c0 (cons b c1)))))])
    (values (reverse 0s) (reverse 1s))))

(define (search-oxygen input pos)
  (if (eq? 1 (length input))
      input
      (let-values ([(0s 1s) (filter-bits input pos)])
        (if (eq? (length 0s) (length 1s))
            (search-oxygen 1s (add1 pos))
            (if (> (length 0s) (length 1s))
                (search-oxygen 0s (add1 pos))
                (search-oxygen 1s (add1 pos)))))))

(define (search-co input pos)
  (if (eq? 1 (length input))
      input
      (let-values ([(0s 1s) (filter-bits input pos)])
        (if (eq? (length 0s) (length 1s))
            (search-co 0s (add1 pos))
            (if (< (length 0s) (length 1s))
                (search-co 0s (add1 pos))
                (search-co 1s (add1 pos)))))))

(define (bits->number bs)
  (string->number (list->string (vector->list (car bs))) 2))

(define (life-support-rating input)
  (let-values ([(0s 1s) (filter-bits input 0)])
    (if (> (length 0s) (length 1s))
        (* (bits->number (search-oxygen 0s 1))
           (bits->number (search-co 1s 1)))
        (* (bits->number (search-oxygen 1s 1))
           (bits->number (search-co 0s 1))))))


(module+ main
  (print-timed life-support-rating (list bitput)))

(module+ test

  (define input '("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"))
  (define bitput (map (λ (l) (list->vector (string->list l))) input))

  (life-support-rating bitput))
