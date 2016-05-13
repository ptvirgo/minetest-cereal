#! /usr/bin/env racket
#lang racket
(require db racket/cmdline "world/blocks.rkt")

;; #### Constants

(define BATCH-SIZE 1000)

;; #### Functions

;; Functions that rely on a database being in a particular state have been
;; incorporated here rather than in the library, because unit tests.

;; Database -> Pos
;; Given a Minetest world database, return the minimum Pos, by integer
;; value.

(define (min-pos wdb)
  (query-value wdb "select min(pos) from blocks"))

;; Database -> Pos
;; Given a Minetest world database, return the maximum Pos, by integer
;; value.

(define (max-pos wdb)
  (query-value wdb "select max(pos) from blocks"))

;; Database Pos Pos -> ListOfBlock
;; Given a database, low, and maximum count, return a list of minetest-blocks
;; within the provided range from the database.

(define (get-blocks wdb start count)
  (rows->blocks (query-rows wdb
                            "select pos, data from blocks where pos > $1 limit $2"
                            start count)))

;; Database rows -> ListOfBlocks
;; Convert database rows into a list of blocks.

(define (rows->blocks rows)
  (cond [(empty? rows) rows]
        [else
         (cons (row->block (first rows)) (rows->blocks (rest rows)))]))

;; Database row -> Mintest-Block
;; Convert a single Minetest world database row into a minetest-block

(define (row->block row)
  (minetest-block (vector-ref row 0) (vector-ref row 1)))

;; String -> Database connection
;; Take the file-name of a Minetest world as a string, return a SQL
;; database connection

(define (open-minetest-world world)
  (let ([minetest-world (sqlite3-connect #:database world)])
    (if (connected? minetest-world)
        minetest-world
        (raise (string-append "Could not open " world)))))

;; String -> New database connection
;; Take a file-name and create a new database for the human-readable format.

(define (new-legible-db name)
  (let ([legible-db (sqlite3-connect #:database name
                                     #:mode 'create)])
    (cond [(connected? legible-db) (create-legible-table legible-db)]
          [else (raise (string-append "Could not create " name))])))

;; Database connection -> Database connections
;; Create a new, legible table in the provided database
(define (create-legible-table ldb)
  (query-exec ldb "create table blocks (x INTEGER, y INTEGER, z INTEGER, data BLOB, PRIMARY KEY (x, y, z))")
  ldb)

;; Database connection -> Prepared SQL statement
;; Prepare the insert statement to add blocks to the legible database table.  The code should run
;; faster with this compiled only once.
(define (prepare-legible-insert ldb)
  (prepare ldb "insert into blocks (x, y, z, data) values(?,?,?,?)"))

;; Database connection x y z -> boolean
;; Return true if the position provided already exists in the legible database,
;; false otherwise

(define (block-exists? ldb x y z)
  (if (false? (query-maybe-row ldb "select * from blocks where x = ? and y = ? and z = ?" x y z))
      #f
      #t))

;; Database Boolean -> Boolean
;; Returns true.  As a side effect, starts a transaction if the supplied
;; boolean is false.  Creating side effects in a functional language is odd,
;; but the idea is "necessary? true" -> starts a transaction, "necessary? false"
;; does not.

(define (start-transaction-if-necessary c necessary?)
  (cond [necessary? (start-transaction c) #t]
        [else #t]))

;; Database Database Statement Pos Boolean -> Integer
;; Migrate records from the Minetest world to the Legible database,
;; using the prepared statement. Returns last block inserted.
;; Transaction boolean should indicate whether or not the batch is already
;; contained within a transaction.  A new transaction will be initiated
;; if it is not.

(define (migrate-batch mtw ldb insert start transaction)
  (cond [(>= start (max-pos mtw)) (commit-transaction ldb) start]
        [else
         (migrate-batch
          mtw
          ldb
          insert
          (migrate-blocks (get-blocks mtw start BATCH-SIZE) ldb insert)
          (start-transaction-if-necessary ldb (not transaction)))]))

;; ListOfBlock Database Insert -> Pos
;; Insert a provided list of blocks into the new legible database.  Return the Pos
;; of the last block inserted.

(define (migrate-blocks blocks ldb insert)
  (cond [(empty? blocks) 0] ; Should never happen unless we have an empty database.
        [(empty? (rest blocks)) (migrate-block (first blocks) ldb insert)]
        [else (migrate-block (first blocks) ldb insert) ;; Value is ignored.  Insert as side-effect.
              (migrate-blocks (rest blocks) ldb insert)]))

;; Minetest-Block Database Insert
;; Insert the block into the database with the provided statement.  Return the Pos
;; value of the block.

(define (migrate-block block ldb insert)
  (let* ([position (pos->point (minetest-block-pos block) empty)]
         [x (point-x position)]
         [y (point-y position)]
         [z (point-z position)]
         [data (minetest-block-data block)]
         [ok (not (block-exists? ldb x y z))]
         [id (string-append "block x:" (number->string x)
                            " y:" (number->string y)
                            " z:" (number->string z)
                            " pos:" (number->string (minetest-block-pos block)))])
    (cond [ok (query-exec ldb insert x y z data)]
          [else (display (string-append "Duplicate " id ": skipping\n"))])
    (minetest-block-pos block)))


;; Command line -> Program execution
;; Parse command line options, use a helper funtion to return a database
;; connection or error-out as appropriate.
(define world (make-parameter ""))
(define output-db (make-parameter ""))

(define convert-minetest-world-db
  (command-line
   #:program "Convert Positions"
   #:args (world output)
   (cond [(file-exists? output) (raise (string-append output " already exists, cannot create"))]
         [(not (file-exists? world)) (raise (string-append world " does not exist, cannot open"))]
         [else
          (let* ([ldb (new-legible-db output)]
                 [mtw (open-minetest-world world)]
                 [insert (prepare-legible-insert ldb)]
                 [start (- (min-pos mtw) 1)])
            (migrate-batch mtw ldb insert start (start-transaction-if-necessary ldb #t)))])))
