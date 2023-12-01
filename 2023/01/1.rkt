#lang racket/base

;; https://adventofcode.com/2023/day/1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test

(module+ test
  (require rackunit
           racket/string
           racket/list
           (only-in racket/file file->lines))

  (define test-input "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

  ;(define lines (string-split test-input #px"\\s"))
  (define lines
    (file->lines "./input.txt"))

  (define sumA
    (for/fold ([accum 0])
              ([l (in-list lines)])
      (let ([cs (filter char-numeric? (string->list l))])
        (+ accum (string->number (string (first cs) (last cs)))))))

  (println sumA)
  )
