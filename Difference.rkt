#lang racket

;;;; 15. The Difference Between Men and Boys ...

(define x (cons 'chicage (cons 'pizza '())))

(set! x 'gone)

(set! x 'skins)


(define gourmet
  (lambda (food)
    (cons food
          (cons x '()))))

(gourmet 'onion)


(set! x 'rings)

(gourmet 'onion)


(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x '()))))

(gourmand 'potato)

(gourmand 'rice)


(define diner
  (lambda (food)
    (cons 'milkshake
          (cons food '()))))


(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake
          (cons food '()))))

(dinerR 'onion)

(dinerR 'pecanpie)

(gourmand 'onion)


(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x '())))))

(omnivore 'bouillabaisse)


(define gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x '())))))

(gobbler 'gumbo)


(define nibbler
  (lambda (food)
    (let ((x 'donut))
      (set! x food)
      (cons food
            (cons x '())))))

(nibbler 'cheerio)


(define food 'none)

(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more
          (cons x
                (cons 'more
                      (cons x '()))))))

(glutton 'garlic)


#|
(define chez-nous
  (lambda ()
    (set! food x)
    (set! x food)))
|#

(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))

(glutton 'garlic)

(gourmand 'potato)

x

food

(chez-nous)

x

food