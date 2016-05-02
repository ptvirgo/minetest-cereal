#lang racket
(require test-engine/racket-tests)
(provide pos-to-point point-to-pos)

;; #### Constants ####

;; The algorithm used to convert block 'pos' values into positions in
;; X,Y,Z coordinates uses these values.  I do not know why it was done
;; this way.

(define POS_MAX_SIGN 2048)
(define POS_MODULO 4096)
(define POS_BIGINT 16777216)

;; #### Data Definitions ####

;; # Coordinate is Number[-30912 30927]
;; Interpretation: a single number representing an x, y, or z value
;; of a location in a Minetest world

;; The origin coordinate of either the x, y, or z axis.
(define COORDINATE-01 0)

;; The minimum an x, y, or z can be.
(define COORDINATE-02 -30912)

;; 3/4 of the way up of the full coordinate range
(define COORDINATE-03 15456)

#;
(define (fn-for-coordinate c)
  (... c))

;; # Pos is Integer

;; Interpretation:  The 'pos' field from a Minetest world database is
;; run through a math algorithm to determine the x, y, and z coordinates.
(define POS-01 -16756737) ; Translates to coordinate (-1 5 -1)
(define POS-02 -16752641) ; Translates to coordinate (-1 6 -1)
(define POS-03 20479) ; Translates to coordinate (-1 5 0)

;; Template
#;
(define (fn-for-pos pos)
  (if (not (number? pos))
      (error "position must be a number")
      (... pos)))

;; # Point is (list x y z)

;; Where x, y, and z are the numeric coordinates for the point in their
;; respective axis.  This could have been a struct (and I'll be
;; replicating basic accessors), but the alorithm for the Pos is
;; better geared toward recursion and list output.

; Origin point - single-player games usually start near here
(define POINT-0 (list  0 0 0))

; Due West and slightly above the origin point
(define POINT-1 (list -200 5 0))

; Slightly East, far North, and below the origin point.
(define POINT-2 (list 25 -30 500))

;; The default template for Point looks odd, but this is a data definition
;; for a list of exactly three elements, not one of arbitrary length.

#;
(define (fn-for-point p)
  (... (first p)                   ; x coordinate
       (first (rest p))            ; y coordinate
       (first (rest (rest p)))))   ; z coordinate

;; Using functions from those defined in the function section below allows
;; for a struct-like template:
#;
(define (fn-for-point p)
  (... (point-x p)   ; x coordinate
       (point-y p)   ; y coordinate
       (point-z p))) ; z coordinate

;; # Minetest-Block is (minetest-block pos data)

;; Interpretation:  This duplicates a single block entry from a Minetest
;; map.sqlite (or compatible) database.
;; - pos is the identifying string, which can be converted to the
;;   x,y,z coordinates of the block
;; - data is the chunk of bits that the Minetest game engine translates
;;   into actual information about the block.

(struct minetest-block (pos data))

;; As of this writing, I do not know how to describe the examples.
;; They were records pulled  from one of my single-player Minetest worlds.
;; As written the data may be corrupt, having been converted to a string.

(define mblock-01 (minetest-block -16756737 "\31\0\2\2x\234\355\330\1\r\0\0\b\3\2407\260\177[S8\247\203\30$\0\0\0\0\0\0\0\300u\5\0\0\0\274\267\375\17\0\0\0\300\274\6\t\236\360\1x\234c\0\0\0\1\0\1\0\0\0\377\377\377\377\0\0\1\0\0\0\3air\n\0\0"))
(define mblock-02 (minetest-block 20479 "\31\0\2\2x\234\355\330\1\r\0\0\b\3\2407\260\177[S8\247\203\30$\0\0\0\0\0\0\0\300u\5\0\0\0\274\267\375\17\0\0\0\300\274\6\t\236\360\1x\234c\0\0\0\1\0\1\0\0\0\377\377\377\377\0\0\1\0\0\0\3air\n\0\0"))

#;
(define (fn-for-minetest-block block)
  (... (minetest-block-pos block)
       (minetest-block-data block)))

;; Function Definitions

;; Pos -> Point
;; Convert a Pos formatted position identifier into a Point.

;; We're treating Pos as a self-referencing recursive number
;; for the purposes of Minetests position algorithm, so
;; this function is structured differently from the normal data
;; template.

;; By HTDP standards, the recursion is jacked-up, because the algorithm
;; is a jacked up, becuase the data definition for Pos is jacked up,
;; which is the whole reason I'm writing a way to convert it in the first
;; place.

(check-expect (pos-to-point -16756737 empty) (list -1 5 -1))
(check-expect (pos-to-point -16752641 empty) (list -1 6 -1))
(check-expect (pos-to-point 20479 empty) (list -1 5 0))

(define (pos-to-point pos build-point)
  (cond [(not (number? pos)) (error "position must be a number")]
        [(> (length build-point) 3) (error "build-point too long")]
        [(= (length build-point) 3) (reverse build-point)]
        [else
         (let* ([coordinate (pos-to-coordinate pos)])
           (append (pos-to-point (splice-pos coordinate pos)
                                 (cons coordinate build-point))))]))


;; Pos -> Coordinate
;; Take a Pos, calculate a single coordinate

(check-expect (pos-to-coordinate -16756737) -1)
(check-expect (pos-to-coordinate -4091) 5)
(check-expect (pos-to-coordinate -4090) 6)
(check-expect (pos-to-coordinate 20479) -1)

(define (pos-to-coordinate pos)
  (below-max (modulo pos POS_MODULO)))

(define (below-max number)
  (if (< number POS_MAX_SIGN)
      number
      (- number (* 2 POS_MAX_SIGN))))

;; Point -> Coordinate
;; Retrieve the x value of a Point.  Just a convenience function that makes
;; reading the code a bit easier.

(check-expect (point-x (list 1 2 3)) 1)
(check-expect (point-x (list 90 80 70)) 90)
(define (point-x p) (first p))

;; Point -> Coordinate
;; Retrieve the y value of a Point.  Just a convenience function that makes
;; reading the code a bit easier.

(check-expect (point-y (list 1 2 3)) 2)
(check-expect (point-y (list 90 80 70)) 80)
(define (point-y p) (first (rest p)))

;; Point -> Coordinate
;; Retrieve the z value of a Point.  Just a convenience function that makes
;; reading the code a bit easier.

(check-expect (point-z (list 1 2 3)) 3)
(check-expect (point-z (list 90 80 70)) 70)
(define (point-z p) (first (rest (rest p))))

;; Coordinate Pos -> Pos
;; Takes a previously calculated coordinate and Pos number, return a new
;; Pos value with the previously calculated coordinate removed.

(check-expect (splice-pos -1 -16756737) -4091)
(check-expect (splice-pos 5 -4091) -1)
(check-expect (splice-pos 5 0) 0)

(define (splice-pos coordinate pos)
  (round (/ (- pos coordinate) POS_MODULO)))

;; Point -> Pos
;; Convert a Point into a Minetest block Pos

(check-expect (point-to-pos (list -1 5 -1)) -16756737)
(check-expect (point-to-pos (list -1 6 -1)) -16752641)
(check-expect (point-to-pos (list -1 5 0)) 20479)

(define (point-to-pos point)
  (int64 (+ (point-x point)
            (* (point-y point) POS_MODULO)
            (* (point-z point) POS_BIGINT))))

;; Number -> Number
;; Presumably this forces a 64 bit integer.  Again cheating a little
;; by simply stealing from the sample code without independent tests.

(define (int64 u)
  (cond [(and (<= u (expt 2 63)) (>= u (* -1 (expt 2 63)))) u]
        [(>= u (expt 2 63)) (int64 (- u (expt 2 63)))]
        [else (int64 (+ u (expt 2 63)))]))

(test)