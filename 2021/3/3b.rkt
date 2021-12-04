#lang racket/base

(require
 (only-in racket/file
          file->lines)
 (only-in "../lib/timed.rkt"
          print-timed))

(define input (file->lines "./input.txt"))

;; Convert binary strings into vectors of #\0 and #\1 chars
(define bitput (map (λ (l) (list->vector (string->list l))) input))

;; Filter input bits into lists of those starting with 0 and 1 respectively
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

;; Recursively filter out oxygen rating bits (most sig bits)
(define (search-oxygen input pos)
  (if (eq? 1 (length input))
      input
      (let-values ([(0s 1s) (filter-bits input pos)])
        (if (eq? (length 0s) (length 1s))
            (search-oxygen 1s (add1 pos))
            (if (> (length 0s) (length 1s))
                (search-oxygen 0s (add1 pos))
                (search-oxygen 1s (add1 pos)))))))

;; Recursively filter out co2 rating bits (least sig bits)
(define (search-co input pos)
  (if (eq? 1 (length input))
      input
      (let-values ([(0s 1s) (filter-bits input pos)])
        (if (eq? (length 0s) (length 1s))
            (search-co 0s (add1 pos))
            (if (< (length 0s) (length 1s))
                (search-co 0s (add1 pos))
                (search-co 1s (add1 pos)))))))

;; Convert vector of chars to a decimal number
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
