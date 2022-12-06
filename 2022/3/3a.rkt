#lang racket/base

(require
 racket/list
 racket/set
 (only-in racket/string string-split)
 "../../lib/timed.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require (only-in racket/file file->list))

  (define input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

  (map (λ (s)
         (apply set-intersect
                (list (string->list (substring s 0 (/ (string-length s) 2)))
                      (string->list (substring s (/ (string-length s) 2))))))
       (string-split input))
  )
