#! /usr/bin/env racket
#lang racket
(require db racket/cmdline "world/blocks.rkt")

;; # Constants
#! /usr/bin/env racket
(define BATCH-SIZE 1000)

;; # Functions
;; Functions that rely on database state have been incorporated into
;; the command line tool instead of a library.

;; Mintest database row -> Mintest block
;; Convert a single mintest world database row into a minetest-block

(define (minetest-row->minetest-block row)
  (minetest-block (vector-ref row 0)   ;; the pos field
                  (vector-ref row 1))) ;; data field

;; Database Pos -> ListOfBlocks
;; Accept a database connection and a start (low) pos.  Return
;; a list of blocks, up to a maximum of BATCH-SIZE.

(define (retrieve-blocks source start)
  (map minetest-row->minetest-block (query-rows source
                                                "select pos, data from blocks where pos > ? order by pos asc limit ?"
                                                start
                                                BATCH-SIZE)))

;; Database connection -> Function
;; Prepare an insert function to add blocks to the provided table.
;; Insert SQL works faster if it is compiled only once.  The created
;; function has signature:
;;
;; MintestBlock -> ID
;; Insert a block record into the destination database.  Returns
;; the pos identifier.

(define (prepare-insert destination)
  (let ([insert
         (prepare destination "insert into blocks(x,y,z,data) values (?,?,?,?)")])
    (lambda (block)
      (let* ([position (pos->point (minetest-block-pos block) empty)]
             [x (point-x position)]
             [y (point-y position)]
             [z (point-z position)]
             [data (minetest-block-data block)])
        (query-exec destination insert x y z data)
        (minetest-block-pos block)))))


;; String -> Database connection
;; Take a source database file name.  Connect to it, or raise an
;; exception.  The exception is the only addition.

(define (open-database name)
  (let ([source (sqlite3-connect #:database name)])
    (cond [(connected? source) source]
          [else (raise (string-append "Unable to open: " name))])))


;; String -> Database connection
;; Takes the name of a destination file. Creates the appropriate database
;; table and returns the SQLite connection.

(define (create-database name)
  (let ([new-db (sqlite3-connect #:database name
                                 #:mode 'create)])
    (cond [(connected? new-db)
           (query-exec new-db
                       "create table blocks (id INTEGER PRIMARY KEY, x INTEGER, y INTEGER, z INTEGER, data BLOB, UNIQUE (x,y,z))")
           new-db]
          [else (raise (string-append "Could not create " name))])))

;; Database -> Integer
;; From a world database, determine the lowest pos

(define (first-record db)
  (query-value db "select min(pos) from blocks"))

;; Database -> Integer
;; From a world database, determine highest pos
(define (last-record db)
  (query-value db "select max(pos) from blocks"))

;; Database Database Function Pos -> Pos
;; Take a database source connection, destination, insert function and starting pos
;; Migrates all records from the source to the destination, returns the last record.

(define (migrate-all src dest insert start)
  (cond [(>= start (last-record src)) start]
        [else
         (let ([blocks (retrieve-blocks src start)])
           (start-transaction dest)
           (map insert blocks)
           (commit-transaction dest)
           (migrate-all src dest insert (minetest-block-pos (last blocks))))]))

;; Database Database Function Pos -> Pos
;; Take a database source connection, destination, insert function and starting pos
;; Migrates all records from the source to the destination, returns the last record.

(define (validate-and-start source destination)
  (cond [(not (file-exists? source))
         (raise (string-append "Source file not found: " source))]
        [(file-exists? destination)
         (raise (string-append "Can not over-write existing database: " destination))]
        [else
         (let* ([src-db (open-database source)]
                [dest-db (create-database destination)]
                [insert (prepare-insert dest-db)])
           (migrate-all src-db dest-db insert (sub1 (first-record src-db))))]))


(define source (make-parameter ""))
(define destination (make-parameter ""))

(define main (command-line
              #:program "Translate Minetest Positions"
              #:args (source destination)
              (validate-and-start source destination)))