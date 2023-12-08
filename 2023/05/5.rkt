#lang racket/base

(require racket/hash
         racket/list
         racket/string
         threading)

#|

Advent of Code Day 5 https://adventofcode.com/2023/day/5

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2023 4a

(define (->strnum l)
  (string->number (apply string l)))

;; Linear interpreration between start & stop
;; amt is 0.0 -> 1.0
(define (lerp start stop amt)
  (+ start (* amt (- stop start))))

;; Convert from one range to another
(define (remap value start1 stop1 start2 stop2)
  (lerp start2 stop2 (/ (- value start1) (- stop1 start1))))

(struct smap (dstart sstart srange) #:transparent)

(define (make-map ds ss r)
  (smap (string->number ds)
        (string->number ss)
        (string->number r)))

;; Scan input line by line and load into a map-table hash
;; #hash(("water-to-light-map" . '((smap ...) (smap ...))))
(define (scan-input ls)
  (for/fold ([map-table (hash)]
             [current-key #f]
             #:result map-table)
            ([l (in-list ls)])
    (cond
      [(regexp-match #px"seeds:" l)
       (values
        (hash-set map-table "seeds"
                  (map string->number (string-split (substring l 6))))
        current-key)]
      [(regexp-match #px"([\\w-]+) map:" l)
       (values map-table (cadr (regexp-match #px"([\\w-]+) map:" l)))]
      [(regexp-match #px"([\\d]+) ([\\d]+) ([\\d]+)" l)
       (let ([ms (regexp-match #px"([\\d]+) ([\\d]+) ([\\d]+)" l)])
         (values
          (if (hash-has-key? map-table current-key)
              (hash-set map-table current-key
                        (append (hash-ref map-table current-key)
                                `(,(make-map (cadr ms) (caddr ms) (cadddr ms)))))
              (hash-set map-table current-key
                        `(,(make-map (cadr ms) (caddr ms) (cadddr ms)))))
          current-key))]
      [else (values map-table current-key)])))

(define (map-seeds table key s)
  (define maps (hash-ref table key))
  (define mapped
    (filter
     (λ (x) x)
     (map
      (λ (m)
        (if (and (>= s (smap-sstart m))
                 (<= s (+ (smap-sstart m) (sub1 (smap-srange m)))))
            (remap s (smap-sstart m) (+ (smap-sstart m) (smap-srange m))
                   (smap-dstart m) (+ (smap-dstart m) (smap-srange m)))
            #f))
      maps)))
  (if (null? mapped) s (car mapped)))

(define (map-to-locations t)
  (for/list ([s (hash-ref t "seeds" '())])
    (~>> s
       (map-seeds t "seed-to-soil")
       (map-seeds t "soil-to-fertilizer")
       (map-seeds t "fertilizer-to-water")
       (map-seeds t "water-to-light")
       (map-seeds t "light-to-temperature")
       (map-seeds t "temperature-to-humidity")
       (map-seeds t "humidity-to-location"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2023 4b

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test

  (require racket/string
           racket/list
           (only-in racket/file file->lines))

  (define (prep-test-input s)
    (string-split s #px"\n"))

  (define test-input "
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

  ;; 5a
  ;(apply min (map-to-locations (scan-input (prep-test-input test-input))))
  (apply min (map-to-locations (scan-input (file->lines "./input.txt"))))

  ;(map-seeds t "seed-to-soil" 53)
  ;;(find-winners (file->lines "./input.txt"))


  ;; 5b
  )
