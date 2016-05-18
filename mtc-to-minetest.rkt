#!/usr/bin/env racket
#lang racket
(require db racket/cmdline "world/blocks.rkt")

;; # Constants

(define BATCH-SIZE 1000)

;; # Functions
;; Functions that rely on database state have been incorporated into
;; the command line tool instead of a library.

;; Mtc database row -> Mintest block
;; Convert a single mintest world database row into a minetest-block

(define (mtc-row->minetest-block row)
  (minetest-block (point->pos (list (vector-ref row 1)
                                    (vector-ref row 2)
                                    (vector-ref row 3)))
                  (vector-ref row 4)))

;; Database RowID -> ListOfRows
;; Accept a start (low) record id and maximum number of records.  Returns
;; a batch of rows from the database, up to a total of BATCH-SIZE.

(define (retrieve-rows source start)
  (query-rows source
              "select id, x, y, z, data from blocks where id > ? order by id asc limit ?"
              start
              BATCH-SIZE))

;; Database connection -> Function
;; Prepare an insert function to add blocks to the destination table.
;; Insert SQL works faster if it is compiled only once.  The created
;; function has signature:
;;
;; Mtc row -> Pos
;; Insert a block record into the destination database.  Returns
;; the pos identifier.

(define (prepare-insert destination)
  (let ([insert
         (prepare destination "insert into blocks(pos,data) values (?,?)")])
    (lambda (row)
      (let ([block (mtc-row->minetest-block row)])
        (query-exec destination insert (minetest-block-pos block) (minetest-block-data block))))))

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
                       "create table blocks (pos INTEGER PRIMARY KEY, data BLOB)")
           new-db]
          [else (raise (string-append "Could not create " name))])))

;; Database -> Integer
;; From an mtc database, determine the lowest record id

(define (first-record db)
  (query-value db "select min(id) from blocks"))

;; Database -> Integer
;; From a world database, determine highest pos
(define (last-record db)
  (query-value db "select max(id) from blocks"))

;; Database Database Function Pos -> Pos
;; Take a database source connection, destination, insert function and starting pos
;; Migrates all records from the source to the destination, returns the last record.

(define (migrate-all src dest insert start)
  (cond [(>= start (last-record src)) start]
        [else
         (let ([rows (retrieve-rows src start)])
           (start-transaction dest)
           (map insert rows)
           (commit-transaction dest)
           (migrate-all src dest insert (vector-ref (last rows) 0)))]))

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