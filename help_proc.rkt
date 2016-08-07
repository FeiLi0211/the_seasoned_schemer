#lang racket

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define one?
  (lambda (n)
    (= n 1)))

(define pick
  (lambda (n lat)
    (if (one? n)
        (car lat)
        (pick (sub1 n) (cdr lat)))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define member?
  (lambda (a lat)
    (letrec
        [(M? (lambda (lat)
               (cond
                 [(null? lat) #f]
                 [else (or (eq? (car lat) a)
                           (M? (cdr lat)))])))]
      (M? lat))))

(define-syntax letcc
  (syntax-rules ()
    ((_ var body ...)
     (call-with-current-continuation
      (lambda (var) body ...)))))

(define-syntax try
  (syntax-rules ()
    ((_ var a . b)
     (letcc success
       (letcc var (success a)) . b))))

(provide atom?)
(provide one?)
(provide pick)
(provide Y)
(provide member?)
(provide letcc)
(provide try)