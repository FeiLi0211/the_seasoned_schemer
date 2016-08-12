#lang racket

(require "help_proc.rkt")

;;;; 16. Ready, Set, Bang!

(define sweet-tooth
  (lambda (food)
    (cons food
          (cons 'cake '()))))

(define last 'angelfood)

(sweet-tooth 'chocolate)

last

(sweet-tooth 'fruit)

last


(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons 'cake '()))))

(sweet-toothL 'chocolate)

last

(sweet-toothL 'fruit)

last

(sweet-toothL 'cheese)

last

(sweet-toothL 'carrot)

last


(define ingredients '())

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food
          (cons 'cake '()))))

(sweet-toothR 'chocolate)

ingredients

(sweet-toothR 'fruit)

ingredients

(sweet-toothR 'cheese)

ingredients

(sweet-toothR 'carrot)

ingredients


(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (cons (deep (sub1 m)) '())))))

(deep 3)


#|
(define Ns '())

(define deepR
  (lambda (n)
    (set! Ns (cons n Ns))
    (deep n)))
|#

(define Ns '())
(define Rs '())

#|
(define deepR
  (lambda (n)
    (set! Rs (cons (deep n) Rs))
    (set! Ns (cons n Ns))
    (deep n)))
|#

(define deepR
  (lambda (n)
    (let ((result (deep n)))
      (set! Rs (cons result Rs))
      (set! Ns (cons n Ns))
      result)))

(deepR 3)

(deepR 5)

(deepR 3)


#|
(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
                ((= (car ns) n) (car rs))
                (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))
|#

(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
                ((null? ns) #f)
                ((= (car ns) n) (car rs))
                (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

#|
(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (deepR n))))
|#

#|
(set! deep
      (lambda (m)
        (cond
          ((zero? m) 'pizza)
          (else (cons (deepM (sub1 m)) '())))))

(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))

(set! Ns (cdr Ns))
(set! Rs (cdr Rs))

Ns

Rs

(deepM 6)

Ns

Rs

(deepM 9)

Ns

Rs
|#

#|
(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (if (member? n Ns)
          (find n Ns Rs)
          (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))
|#

#|
(define deepM
  (let ((Rs '()) (Ns '()))
    (lambda (n)
      (if (atom? (find n Ns Rs))
          (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          (find n Ns Rs)))))
|#

(define deepM
  (let ((Rs '()) (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (deep n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(deepM 16)


#|
(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
|#

#|
(define length
  (lambda (l)
    0))

(set! length
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
|#

#|
(define length
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (h (cdr l)))))))
    h))
|#


(define L
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

#|
(define length
  (let ((h (lambda (l) 0)))
    (set! h (L (lambda (arg) (h arg))))
    h))
|#

(define Y!
  (lambda (L)
    (let ((h (lambda (l) '())))
      (set! h (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec
        ((h (f (lambda (arg) (h arg)))))
      h)))

(define length (Y! L))


(define D
  (lambda (depth*)
    (lambda (s)
      (cond
        ((null? s) 1)
        ((atom? (car s))
         (depth* (cdr s)))
        (else
         (max (add1 (depth* (car s)))
              (depth* (cdr s))))))))

(define depth* (Y! D))


(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))

((Y biz) 5)

((Y! biz) 5)
           