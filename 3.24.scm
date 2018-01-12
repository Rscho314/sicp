#lang r6rs

(import (rnrs base(6))
        (rnrs lists(6))
        (rnrs mutable-pairs(6))
         (rnrs io simple(6)))

(define (my-make-table compare-proc)
  (define (my-assoc key table)
    (define (rec k t)
      (if (null? t)
          #f
          (if (compare-proc k (caar t))
              (car t)
              (rec k (cdr t)))))
    (rec key table))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (display local-table)
      (newline)
      (let ((subtable (my-assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (my-assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                    #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (display local-table)
      (newline)
      (let ((subtable (my-assoc key-1 (cdr local-table))))
        (display subtable)
        (newline)
        (if subtable
            (let ((record (my-assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value))
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define my-operation-table (my-make-table eqv?))
(define my-get (my-operation-table 'lookup-proc))
(define my-put (my-operation-table 'insert-proc!))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))