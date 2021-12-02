#lang racket/base

(require
 (only-in racket/string string-split)
 "../lib/read-input.rkt")

#|

Day 1a:
=======

count the number of times a depth measurement increases from the previous measurement.
(There is no measurement before the first measurement.)
In the example above, the changes are as follows:

199 (N/A - no previous measurement)
200 (increased)
208 (increased)
210 (increased)
200 (decreased)
207 (increased)
240 (increased)
269 (increased)
260 (decreased)
263 (increased)

In this example, there are 7 measurements that are larger than the previous
measurement.

|#

(define (process-input input-file)
  (map string->number (string-split (read-input input-file))))

(define full-input (process-input "./input-1a.txt"))

;; test input
(define depths '(199
                 200
                 208
                 210
                 200
                 207
                 240
                 269
                 260
                 263))

(define (count-depths depths)
  (define-values (_ increased-count)
    (for/fold ([seen 0]
               [ct 0])
              ([d depths])
      (if (zero? seen)
          (values d ct)
          (if (> d seen)
              (values d (add1 ct))
              (values d ct)))))
  increased-count)

(define-values (result cpu real gc)
  (time-apply count-depths (list full-input)))

(printf "Number of increases: ~a~n" (car result))
(printf "[Time] cpu: ~ams | real: ~ams | gc: ~ams~n" cpu real gc)


