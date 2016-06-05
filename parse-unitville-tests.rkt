#! /usr/bin/env racket
#lang racket
(require db file/gunzip test-engine/racket-tests "world/blocks.rkt" "world/data.rkt")

;; Reading Functions

(define (listOf reader stream count)
  (cond [(zero? count) empty]
        [else (cons (reader stream) (listOf reader stream (sub1 count)))]))

(define (readTo endpoint stream)
  (let ([byte (read-byte stream)])
    (cond [(eof-object? byte) #f]
          [(eq? byte endpoint) #t]
          [else (readTo endpoint stream)])))


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

(define map-version (read-byte block-data))
(define flags (read-byte block-data))
(define content-width (read-byte block-data))
(define params-width (read-byte block-data))

;; Decompress node parameters: "zlib-compressed node data:"
;; (define node-data-version (read-U16 block-data))
(read-byte block-data)
(read-byte block-data)

(define decompress-node-output (open-output-bytes))
(inflate block-data decompress-node-output)
(define decompressed-nodes (get-output-bytes decompress-node-output))
(define nodes (open-input-bytes decompressed-nodes))
(define param0 (listOf read-U16 nodes 4096))
(define param1 (listOf read-byte nodes 4096))
(define param2 (listOf read-byte nodes 4096))

;; #"\t\236\360\1x\234c\0\0\0\1\0\1\0\0\0\0\0\2\266\0\0\1\0\0\0\3air\n\0\0"

;; Decompress node metadata: "zlib-compressed node metadata list"

;; (define remains (port->bytes block-data))
;; (bytes-length remains)
;; remains

;; (listOf read-byte block-data 6)


(define remains (port->bytes block-data))
remains ; #"\0\1\0\1\0\0\0\0\0\2\266\0\0\1\0\0\0\3air\n\0\0"

;; (readTo 1 block-data)
;; (define node-metadata-count (read-U16 block-data))

;; (define decompress-node-metadata-output (open-output-bytes))
;; (inflate block-data decompress-node-metadata-output)
;; (get-output-bytes decompress-node-metadata-output)