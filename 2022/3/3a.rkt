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

(define (get-badges input)
  ;; List eater that consumes 3 items on each iteration
  (define (loop sum lst)
    (if (null? lst)
        sum
        (let* ([trio (map string->list (take lst 3))] ; consume 3 items
               [rst (drop lst 3)] ; remainder of list
               [badge (apply set-intersect trio)] ; find badge
               [n (char->integer (car badge))]) ; convert badge to int
          ;; offset the int to get 1 - 26 or 27 - 52, sum and recur w rst
          (cond [(< n 91) (loop (+ sum (- n 38)) rst)]
                [else (loop (+ sum (- n 96)) rst)]))))
  (loop 0 input))

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
    (timed-apply  get-badges
                  (list file-input)))

  (println (format "~a : ~a" result dur)))
