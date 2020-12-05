#lang racket/base

(require racket/list
         racket/string
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution 2

#|
    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.
|#

(define eye-colors
  (make-hash
   (map (λ (s) (cons s #t))
        '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))

(define (between? mn mx n)
  (and (<= mn n) (>= mx n)))

(define ((valid-year? mn mx) year)
  (and (= 4 (string-length year))
       (between? mn mx (string->number year))))

(define (valid-height? hgt)
  (define c (string->list hgt))
  (define-values (val unit)
    (values (string->number (apply string (filter char-numeric? c)))
            (apply string (filter char-alphabetic? c))))
  (cond
    [(string=? unit "in") (between? 59 76 val)]
    [(string=? unit "cm") (between? 150 193 val)]
    [else #f]))

(define (valid-hex-color? hcl)
  (and (= 7 (string-length hcl))
       (for/and ([i (map char->integer (drop (string->list hcl) 1))])
         (or (between? 48 57 i) (between? 97 102 i)))))

(define (valid-eye-color? ecl)
  (hash-has-key? eye-colors ecl))

(define (valid-passport-id? id)
  (and (= 9 (string-length id))
       (= 9 (length (filter char-numeric? (string->list id))))))

(define (fails x) #f)
(define (passes x) #t)

(define validation
  (hash "byr" (valid-year? 1920 2002) ; Birth
        "iyr" (valid-year? 2010 2020) ; Issue Year
        "eyr" (valid-year? 2020 2030) ; Expiration Year
        "hgt" valid-height? ; Height
        "hcl" valid-hex-color? ; Hair Color
        "ecl" valid-eye-color? ; Eye Color
        "pid" valid-passport-id? ; Passport ID
        "cid" passes ; Country ID [optional]
        ))

;; A valid passport must have the first seven valid fields (cid is optional)
(define (day4-two [input "input-4-test-valid.txt"])
  (for/fold ([valid 0])
            ([p (get-passports input)])
    (define pfs
      (for/fold ([fields (hash)])
                ([fld p])
        (define kv (string-split fld ":"))
        (define-values (f v) (values (car kv) (cadr kv)))
        (if ((hash-ref validation f fails) v)
            (hash-set fields f v)
            fields)))
    (define len (length (hash-keys pfs)))
    (cond
      [(= 8 len) (add1 valid)]
      [(and (= 7 len) (not (hash-has-key? pfs "cid"))) (add1 valid)]
      [else valid])))
