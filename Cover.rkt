#lang racket

(require "help_proc.rkt")

;;;; 12. Take Cover

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a)
                (member? a (cdr lat)))])))

(member? 'sardines
         '(Italian sardines spaghetti parsley))


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
      [else (eq? (car lat) a)])))

(two-in-a-row? '(Italian sardines sardines spaghetti parsley))

(two-in-a-row? '(Italian sardines more sardines spaghetti))
               
  