#!/usr/bin/env racket
#lang racket

(require db)

(define db-connect
    (sqlite3-connect  #:database "sample.db" #:mode 'create))

(define get-table
    (query db-connect "select name from sqlite_master where type='table' and name='sample'"))

(define (create-table)
    (query db-connect "create table sample('test' text)"))

;check and exit
(define (closing)
  ((cond 
     ((connected? db-connect) (disconnect db-connect)))
   (exit)))

(cond 
  ((null? (rows-result-rows get-table)) (create-table)))

(query db-connect "insert into sample values(?)" "test")

(print (list? (rows-result-rows (query db-connect "select * from 'sample'"))))

;; (Closing)
