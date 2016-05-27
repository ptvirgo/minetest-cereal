#! /usr/bin/env racket
#lang racket
(require db "world/blocks.rkt")

(define unitville (sqlite3-connect #:database "test_data/unitville.mtc"))
(if (not (connected? unitville))
    (raise "Unable to connect to unitville database")
    (display "Good to go\n"))

(define DATA0 (query-value unitville "select data from blocks where x=0 and y=0 and z=0")); Minetest Bytes
(define block-data (open-input-bytes DATA0))
(display (read-byte block-data))


