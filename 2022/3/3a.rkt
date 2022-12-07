#lang racket/base

(require
 racket/list
 racket/set
 (only-in racket/string string-split)
 "../../lib/timed.rkt")

(define (sack-contents-a input)
  (for/fold ([sum 0])
            ([i (in-list input)])
    ;; Split the string in half, convert to a list of chars, intersect, to int
    (let* ([c (car (apply set-intersect
                          (list (string->list (substring i 0 (/ (string-length i) 2)))
                                (string->list (substring i (/ (string-length i) 2))))))]
           [n (char->integer c)])
      ;; offset the int to get 1 - 26 or 27 - 52
      (cond [(< n 91) (+ sum (- n 38))]
            [else (+ sum (- n 96))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require (only-in racket/file file->lines))

  (define input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

  (define file-input (file->lines "./input.txt"))

  (define-values (result dur)
    (timed-apply  sack-contents-a
                  (list file-input)))

  (println (format "~a : ~a" result dur))

  )
