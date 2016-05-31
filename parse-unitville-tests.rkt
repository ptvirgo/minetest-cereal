#! /usr/bin/env racket
#lang racket
(require db file/gunzip test-engine/racket-tests "world/blocks.rkt" "world/data.rkt")

;; Reading Functions

(define (listOf reader stream count)
  (cond [(zero? count) empty]
        [else (cons (reader stream) (listOf reader stream (sub1 count)))]))


;; List -> String
;; Show how many times each number repeats in a list

(check-expect (repeats empty) "")
(check-expect (repeats (list 1 2 2 2  3 3 3 3 3)) "1:1 2:3 3:5")

(define (repeats list)
  (local [(define (reps list last count)
            (cond [(empty? list) (string-append (number->string last) ":" (number->string count))]
                  [(= last (first list)) (reps (rest list) (first list) (+ 1 count))]
                  [else
                   (string-append
                    (number->string last) ":" (number->string count) " "
                    (reps (rest list) (first list) 1))]))]
    (if (empty? list)
        ""
        (reps (rest list) (first list) 1))))
        
;; (test)

(define unitville (sqlite3-connect #:database "test_data/unitville.mtc"))

(define DATA0 (query-value unitville "select data from blocks where x=0 and y=0 and z=0 limit 1")); Minetest Bytes
(define block-data (open-input-bytes DATA0))
(define decompress (open-output-bytes))

(define map-version (read-byte block-data))
(define flags (read-byte block-data))
(define content-width (read-byte block-data))
(define params-width (read-byte block-data))
(define metadata-count (read-U16 block-data))

(inflate block-data decompress)
(define decompressed-nodes (get-output-bytes decompress))
; (define nodes (open-input-bytes decompressed-nodes))
(define params (bytes->list decompressed-nodes))
(repeats params)