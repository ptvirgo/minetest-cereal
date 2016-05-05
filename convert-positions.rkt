#! /usr/bin/env racket
#lang racket
(require db racket/cmdline "world/blocks.rkt")

;; #### Functions

;; String -> Database connection
;; Take the file-name of a Minetest world as a string, return a SQL
;; database connection

(define (open-minetest-world world)
  (let ([minetest-world (sqlite3-connect #:database world)])
    (if (connected? minetest-world)
        minetest-world
        (raise (string-append "Could not open " world)))))

;; Command line -> Database Connection
;; Parse command line options, use a helper funtion to return a database
;; connection or error-out as appropriate.

(define minetest-world-db
  (command-line
   #:program "Convert Positions"
   #:args (world)
   (open-minetest-world world)))