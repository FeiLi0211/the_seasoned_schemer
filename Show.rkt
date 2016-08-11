#lang racket

(require "help_proc.rkt")

;;;; 11. Welcome Back to the Show

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a)
                (member? a (cdr lat)))])))

(member? 'sardines
         '(Italian sardines spaghetti parsley))


#|
(define two-in-a-row?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (or (is-first? (car lat) (cdr lat))
                (two-in-a-row? (cdr lat)))])))

(define is-first?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (eq? a (car lat))])))
|#

#|
(define two-in-a-row?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (is-first? (car lat) (cdr lat))])))

(define is-first?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a)
                (two-in-a-row? lat))])))
|#

(define two-in-a-row?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (two-in-a-row-b? (car lat) (cdr lat))])))

(define two-in-a-row-b?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a)
                (two-in-a-row-b? (car lat) (cdr lat)))])))

(two-in-a-row? '(Italian sardines paghetti parsley))

(two-in-a-row? '(Italian sardines sardines spaghetti parsley))

(two-in-a-row? '(Italian sardines more sardines spaghetti))


(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      [(null? tup) '()]
      [else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b (+ sonssf (car tup))
                                     (cdr tup)))])))

(sum-of-prefixes '(2 1 9 17 0))

(sum-of-prefixes '(1 1 1 1 1))


(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(define scramble-b
  (lambda (tup rv)
    (cond
      [(null? tup) '()]
      [else
       (cons (pick (car tup) (cons (car tup) rv))
             (scramble-b (cdr tup) (cons (car tup) rv)))])))

(scramble '(1 1 1 3 4 2 1 1 9 2))

(scramble '(1 2 3 4 5 6 7 8 9))

(scramble '(1 2 3 1 2 3 4 1 8 2 10))