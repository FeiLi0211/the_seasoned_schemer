#lang racket

(require "help_proc.rkt")

;;;; 17. We Change, Therefore We Are!

(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (cons (deep (sub1 m)) '()))))

(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (Ns Rs)
              (cond
                ((null? Ns) #f)
                ((= (car Ns) n) (car Rs))
                (else (A (cdr Ns) (cdr Rs)))))))
      (A Ns Rs))))

#|
(define deepM
  (let ((Ns '()) (Rs '()))
    (letrec
        ((D (lambda (m)
              (if (zero? m)
                  'pizza
                  (cons (D (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))
|#

#|
(define deepM
  (let ((Ns '()) (Rs '()))
    (letrec
        ((D (lambda (m)
              (if (zero? m)
                  'pizza
                  (cons (deepM (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))
|#

#|
(define deepM
  (let ((Ns '()) (Rs '()))
    (let
        ((D (lambda (m)
              (if (zero? m)
                  'pizza
                  (cons (deepM (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))

|#

#|
(define deepM
  (let ((Ns '())
        (Rs '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM (sub1 m)) '())))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (D n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
|#

#|
(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result ((lambda (m)
                             (if (zero? m)
                                 'pizza
                                 (cons (deepM (sub1 m)) '()))) n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
|#

#|
(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (let ((m n))
                            (if (zero? m)
                                'pizza
                                (cons (deepM (sub1 m)) '())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
|#

(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (cons (deepM (sub1 n)) '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
         