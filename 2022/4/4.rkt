#lang racket/base

(require
 racket/list
 racket/set
 (only-in racket/string string-split)
 "../../lib/timed.rkt")

;; Fold over input list converting sections to a list of ints
;; Check to see if left (a) or right (b) are contained within one another
(define (find-overlaps input)
  (for/fold ([overlaps 0])
            ([i (in-list input)])
    (let* ([ps (map string->number (string-split i #px"[,-]"))] ; list of ints
           [a (take ps 2)] ; left pair
           [b (drop ps 2)]) ; right pair
      (cond [(and (>= (car b) (car a)) (<= (cadr b) (cadr a))) (add1 overlaps)]
            [(and (<= (car b) (car a)) (>= (cadr b) (cadr a))) (add1 overlaps)]
            [else overlaps]))))

;; Only check if the numbers overlap, not complete containment
(define (find-overlaps2 input)
  (for/fold ([overlaps 0])
            ([i (in-list input)])
    (let* ([ps (map string->number (string-split i #px"[,-]"))] ; list of ints
           [a (take ps 2)] ; left pair
           [b (drop ps 2)]) ; right pair
      (cond [(and (>= (car b) (car a)) (<= (car b) (cadr a))) (add1 overlaps)]
            [(and (<= (car b) (car a)) (>= (cadr b) (car a))) (add1 overlaps)]
            [else overlaps]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require (only-in racket/file file->lines))

  (define input
    (string-split "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"))

  ;;(find-overlaps input)
  
  (define file-input (file->lines "./input.txt"))
  (define-values (result dur)
    (timed-apply  find-overlaps2
                  (list file-input)
                  ;(list input)
                  ))

  (println (format "~a : ~a" result dur))
  )
