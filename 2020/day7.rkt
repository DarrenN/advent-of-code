#lang racket/base

(require racket/string
         racket/list
         "./utils.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare Input

(define (erase-bags s)
  (string-trim (string-trim (string-replace  s #px"bag(.*)" ""))))

(define (parse-bag s)
  (define st (string-trim s))
  (define ct (if (string=? st "no other bags.")
                 0
                 (string->number (car (regexp-match #px"^[\\d]*" st)))))
  (define desc (erase-bags (string-replace st #px"^[\\d]*" "")))
  (list ct desc '()))

(define (parse-rules rule)
  (define frags (string-split rule "contain"))
  (list (erase-bags (car frags))
        (map parse-bag (string-split (cadr frags) ","))))

(define (get-rules input)
  (map parse-rules (string-split (read-input input) "\n")))

(define (lookup-table rules)
  (for/hash ([rule rules])
    (values (car rule) (cadr rule))))

(define (descend-children rules label)
  (define entries (hash-ref rules label '()))
  (if (null? entries)
      entries
      (for/list ([entry entries])
        (let* ([ct (car entry)]
               [l (cadr entry)]
               [cs (caddr entry)])
          (if (hash-has-key? rules l)
              (list ct l (descend-children rules l))
              (list ct l cs))))))

(define (link-rules rule-table)
  (define keys (hash-keys rule-table))
  (for/fold ([rules rule-table])
            ([key keys])
    (if (not (hash-has-key? rules key))
        rules
        (hash-set rules key (descend-children rules key)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution 1

#|
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.



shiny gold bag

A bright white bag, which can hold your shiny gold bag directly.
A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.

So, in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4.

(for/list ([entry entries])
  (let ([label (cadr entry)])
    (println label)
    (if (string=? label "shiny gold")
        (loop (append hits (list #t)) (cdr keys))
        (loop hits (cdr keys)))))

|#

(define rules (link-rules (lookup-table (get-rules "input-7.txt"))))
(define rule-labels (hash-keys rules))

(define (day7-1)
  ;; flatten the entry tree for each color and do a members check on shiny gold
  (for/fold ([sum 0])
            ([rule rule-labels])
    (let ([ms (member "shiny gold" (flatten (hash-ref rules rule)))])
      (if ms (add1 sum) sum))))

(define (day7-2))
