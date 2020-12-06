#lang racket/base

(require racket/list
         racket/set
         racket/string
         "./utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare Input

(define (get-input input)
  (map string-split (string-split (read-input input) "\n\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution 1

(define (day6-one [input "input-6.txt"])
  (define groups
    (map (λ (g)
           (apply seteq (flatten (map string->list g))))
         (get-input input)))
  (for/sum ([g (in-list groups)]) (set-count g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution 2

(define (day6-two [input "input-6.txt"])
  (define groups
    (map (λ (g)
           (apply set-intersect
                  (map (λ (g) (apply seteq g)) (map string->list g))))
         (get-input input)))
  (for/sum ([g (in-list groups)]) (set-count g)))
