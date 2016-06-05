#lang racket
(require test-engine/racket-tests)
(provide read-U16 read-U32)

#|
The Minetest World format stores block data as a byte stream.  The byte
stream uses different bit length encodings and different meanings throughout.

Borrowing from the minetestmapper.py, this library attempts to translate
the byte stream into something humane.

See also: https://en.wikipedia.org/wiki/Integer_%28computer_science%29
|#

;; Data Definitions

;; U8 is Integer [0 - 255]
;; The Minetest code refers to a single byte as a "U8."  It's a standard
;; 8-bit byte, used as an unsigned (positive) integer [0 - 255].  We don't
;; alter Racket's representation of Integers here.

(define U8-0 0)     ; Smallest U8
(define U8-128 128) ; About middle of possible range
(define U8-255 255) ; Maximum U8

;; Template for U8

#;
(define (fn-for-U8 x)
  (... x))

;; U16 is Integer [0 - 65,535]

;; Minetest's U16 is a 16 bit (2 bytes of 8 bits each), unsigned integer,
;; derived by reading two bytes.
;; We don't alter Racket's representation of Integers here.

(define U16-0 0) ;; lowest U16
(define U16-255 255) ;; The first byte is 0, and the second is 255
(define U16-256 256) ;; The first byte is 1, and the second byte is 0
(define U16-MAX (+ (* 256 255) 255)) ;; Every byte is 255

;; Template for U16
#;
(define (fn-for-U16 x)
  (... x))

;; U32 is Integer [0 - 4,294,967,295]
;; Minetest's U32 is a 32 bit (4 bytes, 8 bits each) unsigned integer, derived
;; from reading 4 bytes.
;; We don't alter Racket's representation of Integers here, either.

(define U32-0 0) ;; lowest U32
(define U32-1 (+ (* 256 256 256 1) (* 256 256 1) (* 256 1) 1)) ;; Every byte is 1
(define U32-MAX (+ (* 256 256 256 255) (* 256 256 255) (* 256 255) 255)) ;; Every byte is 255

;; Functions

;; InputStream -> U16

;; Reads 2 bytes (16 bits total) from the provided input stream, converting them
;; to the appropriate integer.

(check-expect (read-U16 (open-input-bytes (bytes 0 0))) 0)
(check-expect (read-U16 (open-input-bytes (bytes 0 1))) 1)
(check-expect (read-U16 (open-input-bytes (bytes 0 255))) 255)
(check-expect (read-U16 (open-input-bytes (bytes 1 0))) 256)
(check-expect (read-U16 (open-input-bytes (bytes 255 255))) 65535)

(define (read-U16 stream)
  (+ (* 256 (read-byte stream)) (read-byte stream)))


;; InputStream -> U32

;; Read 4 bytes (32 bits total) from the provided input stream, converting them
;; to the appropriate integer.

(check-expect (read-U32 (open-input-bytes (bytes 0 0 0 0))) 0)
(check-expect (read-U32 (open-input-bytes (bytes 1 1 1 1))) (+ (* 256 256 256 1) (* 256 256 1) (* 256 1) 1))
(check-expect (read-U32 (open-input-bytes (bytes 255 255 255 255))) 4294967295)

(define (read-U32 stream)
  (+ (* 256 256 256 (read-byte stream)) (* 256 256 (read-byte stream)) (* 256 (read-byte stream)) (read-byte stream)))

(test)