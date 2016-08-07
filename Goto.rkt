#lang racket

(require "help_proc.rkt")

;;;; 13. Hop, Skip, and Jump

#|
(define intersect
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))
|#

#|
(define intersect
  (lambda (set1 set2)
    (letrec
        [(I (lambda (set)
              (cond
                [(null? set) '()]
                [(member? (car set) set2)
                 (cons (car set) (I (cdr set)))]
                [else (I (cdr set))])))]
      (I set1))))
|#

(define intersect
  (lambda (set1 set2)
    (letrec
        [(I (lambda (set)
              (cond
                [(null? set) '()]
                [(member? (car set) set2)
                 (cons (car set) (I (cdr set)))]
                [else (I (cdr set))])))]
      (cond
        [(null? set2) '()]
        [else (I set1)]))))


#|
(define intersectall
  (lambda (lset)
    (cond
      [(null? lset) '()]
      [(null? (cdr lset)) (car lset)]
      [else (intersect (car lset)
                       (intersectall (cdr lset)))])))
|#

#|
(define intersectall
  (lambda (lset)
    (letrec
        [(A
          (lambda (lset)
            (cond
              [(null? (cdr lset)) (car lset)]
              [else (intersect (car lset)
                               (A (cdr lset)))])))]
      (cond
        [(null? lset) '()]
        [else (A lset)]))))
|#

#|
(define intersectall
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           [(A (lambda (lset)
                 (cond
                   [(null? (car lset)) (hop '())]
                   [(null? (cdr lset)) (car lset)]
                   [else (intersect (car lset)
                                    (A (cdr lset)))])))]
         (cond
           [(null? lset) '()]
           [else (A lset)]))))))
|#

#|
(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
          [(A (lambda (lset)
                (cond
                  [(null? (car lset)) (hop '())]
                  [(null? (cdr lset)) (car lset)]
                  [else (intersect (car lset)
                                   (A (cdr lset)))])))]
        (cond
          [(null? lset) '()]
          [else (A lset)])))))
|#

(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
          [(A (lambda (lset)
                (cond
                  [(null? (car lset)) (hop '())]
                  [(null? (cdr lset)) (car lset)]
                  [else (I (car lset) (A (cdr lset)))])))
           (I (lambda (s1 s2)
                (letrec
                    [(J (lambda (s1)
                          (cond
                            [(null? s1) '()]
                            [(member? (car s1) s2)
                             (cons (car s1) (J (cdr s1)))]
                            [else (J (cdr s1))])))]
                  (cond
                    [(null? s2) (hop '())]
                    [else (J s1)]))))]
        (cond
          [(null? lset) '()]
          [else (A lset)])))))

(intersectall '((3 mangos and)
                (3 kiwis and)
                (3 hamburgers)))

(intersectall '((3 steaks and)
                (no food and)
                (three baked potatoes)
                (3 diet hamburgers)))

(intersectall '((3 mangoes and)
                ()
                (3 diet hamburgers)))


(define rember
  (lambda (a lat)
    (letrec
        [(R (lambda (lat)
              (cond
                [(null? lat) '()]
                [(eq? (car lat) a) (cdr lat)]
                [else (cons (car lat)
                            (R (cdr lat)))])))]
      (R lat))))

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        [(R (lambda (lat)
              (cond
                [(null? lat) '()]
                [(eq? (car lat) a) '()]
                [else (cons (car lat)
                            (R (cdr lat)))])))]
      (R lat))))

(rember-beyond-first 'roots
                     '(noodles
                       spaghetti spatzle bean-thread
                       roots
                       potatoes yam
                       others
                       rice))

(rember-beyond-first 'others
                     '(noodles
                       spaghetti spatzle bean-thread
                       roots
                       potatoes yam
                       others
                       rice))

(rember-beyond-first 'sweetthing
                     '(noodles
                       spaghetti spatzle bean-thread
                       roots
                       potatoes yam
                       others
                       rice))

(rember-beyond-first 'desserts
                     '(cookies
                       chocolate mints
                       caramel delight ginger snaps
                       desserts
                       chocolate mousse
                       vanilla ice cream
                       German chocolate cake
                       more desserts
                       gingerbreadman chocolate
                       chip brownies))


(define rember-upto-last
  (lambda (a lat)
    (letcc skip
      (letrec
          [(R (lambda (lat)
                (cond
                  [(null? lat) '()]
                  [(eq? (car lat) a)
                   (skip (R (cdr lat)))]
                  [else (cons (car lat)
                              (R (cdr lat)))])))]
        (R lat)))))

(rember-upto-last 'roots
                  '(noodles
                       spaghetti spatzle bean-thread
                       roots
                       potatoes yam
                       others
                       rice))

(rember-upto-last 'sweetthing
                  '(noodles
                       spaghetti spatzle bean-thread
                       roots
                       potatoes yam
                       others
                       rice))

(rember-upto-last 'cookies
                  '(cookies
                    chocolate mints
                    caramel delight ginger snaps
                    desserts
                    chocolate mousse
                    vanilla ice cream
                    German chocolate cake
                    more cookies
                    gingerbreadman chocolate
                    chip brownies))