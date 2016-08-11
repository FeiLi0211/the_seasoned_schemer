#lang racket

(require "help_proc.rkt")

;;;; 14. Let There Be Names

#|
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
|#

#|
(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (cond
              ((atom? (leftmost (car l)))
               (leftmost (car l)))
              (else (leftmost (cdr l))))))))
|#

#|
(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (let ((a (leftmost (car l))))
              (cond
                ((atom? a) a)
                (else (leftmost (cdr l)))))))))
|#

#|
(define leftmost
  (lambda (l)
    (letcc skip
      (lm l skip))))

(define lm
  (lambda (l out)
    (cond
      ((null? l) '())
      ((atom? (car l)) (out (car l)))
      (else
       (begin (lm (car l) out)
              (lm (cdr l) out))))))
|#

#|
(define leftmost
  (letrec
      ((lm (lambda (l out)
             (cond
               ((null? l) '())
               ((atom? (car l)) (out (car l)))
               (else
                (begin (lm (car l) out)
                       (lm (cdr l) out)))))))
    (lambda (l)
      (letcc skip
        (lm l skip)))))
|#

#|
(define leftmost
  (lambda (l)
    (letcc skip
      (letrec
          ((lm (lambda (l out)
                 (cond
                   ((null? l) '())
                   ((atom? (car l)) (out (car l)))
                   (else
                    (begin (lm (car l) out)
                           (lm (cdr l) out)))))))
        (lm l skip)))))
|#

(define leftmost
  (lambda (l)
    (letcc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) '())
                   ((atom? (car l)) (skip (car l)))
                   (else
                    (begin (lm (car l))
                           (lm (cdr l))))))))
        (lm l)))))

(leftmost '(((a) b) (c d)))

(leftmost '(((a) ()) () (e)))

(leftmost '(((() a) ())))


#|
(define rember1*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (cdr l))
         (else (cons (car l) (rember1* a (cdr l))))))
      (else (cond
              ((equal? (rember1* a (car l)) (car l))
               (cons (car l) (rember1* a (cdr l))))
              (else (cons (rember1* a (car l)) (cdr l))))))))
|#

#|
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else (cond
                        ((equal? (R (car l)) (car l))
                         (cons (car l) (R (cdr l))))
                        (else (cons (R (car l)) (cdr l)))))))))
      (R l))))
|#

#|
(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh #f))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh))))
      (else
       (if (atom? (letcc oh
                    (rm a (car l) oh)))
           (cons (car l)
                 (rm a (cdr l) oh))
           (cons (rm a (car l) 0) (cdr l)))))))
|#

#|
(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh #f))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh))))
      (else
       (let ((new-car (letcc oh (rm a (car l) oh))))
         (if (atom? new-car)
             (cons (car l)
                   (rm a (cdr l) oh))
             (cons new-car (cdr l))))))))
|#

(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh #f))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh))))
      (else
       (try oh2
            (cons (rm a (car l) oh2) (cdr l))
            (cons (car l) (rm a (cdr l) oh)))))))

(letcc Say (rm 'noodles '((food) more (food)) Say))

#|
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else (let ((av (R (car l))))
                        (cond
                          ((equal? av (car l))
                           (cons (car l) (R (cdr l))))
                          (else (cons av (cdr l))))))))))
      (R l))))
|#

#|
(define rember1*
  (lambda (a l)
    (if (atom? (letcc oh (rm a l oh)))
        l
        (rm a l '()))))
|#

#|
(define rember1*
  (lambda (a l)
    (let ((new-l (letcc oh (rm a l oh))))
      (if (atom? new-l)
          l
          new-l))))
|#

(define rember1*
  (lambda (a l)
    (try oh (rm a l oh) l)))

#|
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else (cond
              ((> (depth* (cdr l))
                  (add1 (depth* (car l))))
               (depth* (cdr l)))
              (else (add1 (depth* (car l)))))))))
|#

#|
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (let ((a (add1 (depth* (car l))))
             (d (depth* (cdr l))))
         (cond
           ((> d a) d)
           (else a)))))))
|#

#|
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      (else
       (let ((d (depth* (cdr l))))
         (cond
           ((atom? (car l)) d)
           (else
            (let ((a (add1 (depth* (car l)))))
              (cond
                ((> d a) d)
                (else a))))))))))
|#

#|
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (let ((a (add1 (depth* (car l))))
             (d (depth* (cdr l))))
         (if (> d a) d a))))))
|#

#|
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (let ((a (add1 (depth* (car l))))
             (d (depth* (cdr l))))
         (max a d))))))
|#

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (max (add1 (depth* (car l)))
            (depth* (cdr l)))))))
         

(depth* '((pickled) pepers (peppers pickled)))
                        
(depth* '(margarine
          ((bitter butter)
           (makes)
           (batter
            (bitter)))
          butter))


(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
                ((null? tup) '())
                (else
                 (let ((rp (cons (car tup) rp)))
                   (cons (pick (car tup) rp)
                         (P (cdr tup) rp))))))))
      (P tup '()))))
