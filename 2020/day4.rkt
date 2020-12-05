#lang racket/base

(require racket/string
         "./utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare Input

(define (get-passports input)
  (map string-split
       (string-split (read-input input) "\n\n")))

(define required
  (hash "byr" #t ; Birth
        "iyr" #t ; Issue Year
        "eyr" #t ; Expiration Year
        "hgt" #t ; Height
        "hcl" #t ; Hair Color
        "ecl" #t ; Eye Color
        "pid" #t ; Passport ID
        "cid" #t ; Country ID [optional]
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution 1

;; A valid passport must have the first seven fields (cid is optional)
(define (day4-one [input "input-4.txt"])
  (for/fold ([valid 0])
            ([p (get-passports input)])
    (define pfs
      (for/fold ([fields (hash)])
                ([fld p])
        (define f (car (string-split fld ":")))
        (if (hash-has-key? required f)
            (hash-set fields f #t)
            fields)))
    (define len (length (hash-keys pfs)))
    (cond
      [(= 8 len) (add1 valid)]
      [(and (= 7 len) (not (hash-has-key? pfs "cid"))) (add1 valid)]
      [else valid])))
