#lang racket/base

(require racket/format
         racket/function
         racket/list
         racket/vector
         2htdp/image
         "wires-input.rkt")

(define GRID-SIZE 100)
(define EMPTY 0)
(define VISITED 1)
(define INTERSECTION 2)

(define (build-grid [size GRID-SIZE])
  (build-vector size  (λ (n) (make-vector size EMPTY))))

(define (update-grid grid x y)
  (let* ([row (vector-ref grid y)]
         [val (vector-ref row x)])
    (cond [(eq? val EMPTY) (vector-set! row x VISITED)]
          [(eq? val VISITED) (vector-set! row x INTERSECTION)])
    (vector-set! grid y row)
    grid))

(define (draw-path grid x y dir dist)
  (if (> dist 0)
      (let ([newdist (- dist 1)])
        (cond [(eq? dir 'R)
               (draw-path (update-grid grid (+ x 1) y) (+ x 1) y dir newdist)]
              [(eq? dir 'L)
               (draw-path (update-grid grid (- x 1) y) (- x 1) y dir newdist)]
              [(eq? dir 'U)
               (draw-path (update-grid grid x (- y 1)) x (- y 1) dir newdist)]
              [(eq? dir 'D)
               (draw-path (update-grid grid x (+ y 1)) x (+ y 1) dir newdist)]))
      (values grid x y)))

(define (render-path path [grid (build-grid 10)])
  (define start-pos (floor (/ (vector-length grid) 2)))
  (for/fold ([state (vector grid start-pos start-pos)])
            ([p path])
    (let ([grid (vector-ref state 0)]
          [x (vector-ref state 1)]
          [y (vector-ref state 2)]
          [dir (car p)]
          [dist (cadr p)])
      (define-values (newgrid new-x new-y) (draw-path grid x y dir dist))
      (vector-set! state 0 newgrid)
      (vector-set! state 1 new-x)
      (vector-set! state 2 new-y)
      state)))

;; Returns completed grid for all paths
(define (render-paths paths [size 100])
  (for/fold ([grid (build-grid size)])
            ([path paths])
    (vector-ref (render-path (parse-wire-path path) grid) 0)))


(define (find-intersections grid)
  (flatten
   (for/list ([y (vector-length grid)])
     (for/fold ([acc (list)])
               ([x (vector-length (vector-ref grid y))])
       (let ([val (vector-ref (vector-ref grid y) x)])
         (if (eq? val INTERSECTION)
             (cons (vector x y) acc)
             acc))))))

(define (calculate-distance start-pos pos)
  (let ([x1 (vector-ref start-pos 0)]
        [y1 (vector-ref start-pos 1)]
        [x2 (vector-ref pos 0)]
        [y2 (vector-ref pos 1)])
    (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(define (display-grid grid [size 100])
  (for/fold ([image (rectangle size size "solid" "white")]
             [y 0])
            ([row (in-vector grid)])
    (let ([image-row (vector->list
                      (vector-map
                       (λ (v) (rectangle 1 1 "solid"
                                         (cond [(eq? v 0) "white"]
                                               [(eq? v 1) "black"]
                                               [(eq? v 2) "red"])))
                       row))])
      (values
       (place-image (apply beside image-row) (floor (/ size 2)) y image)
       (add1 y)))))

(module+ test
  (require rackunit)

  #|

  R75,D30,R83,U83,L12,D49,R71,U7,L72
  U62,R66,U55,R34,D71,R55,D58,R83 = distance 159

  R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
  U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135

  |#
  (define s0 "R8,U5,L5,D3")
  (define s1 "U7,R6,D4,L4")

  (define b (render-paths (list s0 s1) 25))
  ;(define-values (img1 _) (display-grid b 25))
  ;(scale 10 img1)

  (define start-pos1 (floor (/ 25 2)))
  (apply min
         (map (λ (p)
                (calculate-distance (vector start-pos1 start-pos1) p))
              (find-intersections b)))

  #|
  (hash "x0y0": 0
  "x1y0": 0
  |#

  #|
  (define g0 (render-path (parse-wire-path s0) (build-grid 10)))
  (define g1 (render-path (parse-wire-path s1) (vector-ref g0 0)))
  (display-grid (vector-ref g1 0))
  (display (map (λ (p) (calculate-distance (vector 0 9) p))
  (find-intersections (vector-ref g1 0))))
  |#

  ;(display-grid (vector-ref (render-path (parse-wire-path s0) (build-grid 20)) 0))

  (define t0 "R75,D30,R83,U83,L12,D49,R71,U7,L72")
  (define t1 "U62,R66,U55,R34,D71,R55,D58,R83")

  (define a (render-paths (list t0 t1) 500))
  ;(define-values (img __) (display-grid a 500))
  ;(scale 3 img)

  (define start-pos (floor (/ 500 2)))
  (apply min (map (λ (p) (calculate-distance (vector start-pos start-pos) p))
                  (find-intersections a)))

  (define u0 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
  (define u1 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

  (define size1 500)
  (define c (render-paths (list u0 u1) size1))
  (define start-pos2 (floor (/ size1 2)))
  (apply min (map (λ (p) (calculate-distance (vector start-pos2 start-pos2) p))
                  (find-intersections c)))

  (define size2 25000)
  (define d (render-paths (list w0 w1) size2))
  (define start-pos3 (floor (/ size1 2)))
  (apply min (map (λ (p) (calculate-distance (vector start-pos3 start-pos3) p))
                  (find-intersections d)))

  ;(define-values (img __) (display-grid c size1))
  ;(scale 2 img)

  #|
  (display (map (λ (p) (calculate-distance (floor (/ 500 2)) p))
  (find-intersections a)))
  |#

  #|
  (define z0 (render-path (parse-wire-path t0) (build-grid 500)))
  (define z1 (render-path (parse-wire-path t1) (vector-ref z0 0)))
  (display (map (λ (p) (calculate-distance (vector 0 499) p))
  (find-intersections (vector-ref z1 0))))


  (define u0 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
  (define u1 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")


  (define h0 (render-path (parse-wire-path u0) (build-grid 200)))
  (define h1 (render-path (parse-wire-path u1) (vector-ref h0 0)))
  (display (map (λ (p) (calculate-distance (vector 0 199) p))
  (find-intersections (vector-ref h1 0))))

  |#

  )
