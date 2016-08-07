#lang racket

(require "help_proc.rkt")

;;;; 12. Take Cover

#| version 1.0
(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a)
       (multirember a (cdr lat))]
      [else
       (cons (car lat)
             (multirember a (cdr lat)))])))
|#

#| version 2.0
(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
              [(null? lat) '()]
              [(eq? (car lat) a)
               (mr (cdr lat))]
              [else
               (cons (car lat)
                     (mr (cdr lat)))]))))
     lat)))
|#

#| version 3.0
(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                  [(null? lat) '()]
                  [(eq? (car lat) a) (mr (cdr lat))]
                  [else (cons (car lat) (mr (cdr lat)))]))))
       mr)
     lat)))
|#

(define multirember
  (lambda (a lat)
    (letrec
        [(mr (lambda (lat)
               (cond
                 [(null? lat) '()]
                 [(eq? (car lat) a) (mr (cdr lat))]
                 [else (cons (car lat) (mr (cdr lat)))])))]
      (mr lat))))
                  
(multirember 'tuna
             '(shrimp salad tuna salad and tuna))

(multirember 'pie
             '(apple custard pie linzer pie torte))


#|
(define length
  ((lambda (le)
     ((lambda (f) (f f))
      (lambda (f)
        (le (lambda (x) ((f f) x))))))
   (lambda (length)
     (lambda (l)
       (cond
         [(null? l) 0]
         [else (add1 (length (cdr l)))])))))
|#

(define length
  (Y (lambda (length)
       (lambda (l)
         (cond
           [(null? l) 0]
           [else (add1 (length (cdr l)))])))))

(length '(a b (c d) (d (d (d))) d))



(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) '()]
        [(test? (car l) a) (cdr l)]
        [else (cons (car l)
                    ((rember-f test?) a (cdr l)))]))))

#|
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) '()]
        [(test? (car lat) a)
         ((multirember-f test?) a (cdr lat))]
        [else (cons (car lat)
                    ((multirember-f test?) a (cdr lat)))]))))
|#

(define multirember-f
  (lambda (test?)
    (letrec
        [(m-f (lambda (a lat)
                (cond
                  [(null? lat) '()]
                  [(test? (car lat) a)
                   (m-f a (cdr lat))]
                  [else (cons (car lat)
                              (m-f a (cdr lat)))])))]
      m-f)))


#|
(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a)
                (member? a (cdr lat)))])))
|#

(define member?
  (lambda (a lat)
    (letrec
        [(M? (lambda (l)
               (cond
                 [(null? l) #f]
                 [else (or (eq? (car l) a)
                           (M? (cdr l)))])))]
      (M? lat))))

(member? 'ice
         '(salad greens with pears brie cheese frozen yogurt))


#|
(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2)
       (union (cdr set1) set2)]
      [else (cons (car set1)
                  (union (cdr set1) set2))])))
|#

#|
(define union
  (lambda (set1 set2)
    (letrec
        [(U (lambda (set1)
              (cond
                [(null? set1) set2]
                [(member? (car set1) set2)
                 (U (cdr set1))]
                [else (cons (car set1)
                            (U (cdr set1)))])))]
      (U set1))))
|#

(define union
  (lambda (set1 set2)
    (letrec
        [(U (lambda (set)
              (cond
                [(null? set) set2]
                [(member? (car set) set2)
                 (U (cdr set))]
                [else (cons (car set)
                            (U (cdr set)))])))
         (member? (lambda (a lat)
                    (letrec
                        [(M? (lambda (lat)
                               (if (null? lat)
                                   #f
                                   (or (eq? (car lat) a)
                                       (M? (cdr lat))))))]
                      (M? lat))))]
      (U set1))))

(union '(tomatoes and macaroni casserole)
       '(macaroni and cheese))


;; two-in-a-row?

(define two-in-a-row?
  (lambda (lat)
    (letrec
        [(W (lambda (a lat)
              (cond
                [(null? lat) #f]
                [else (or (eq? (car lat) a)
                          (W (car lat) (cdr lat)))])))]
      (cond
       [(null? lat) #f]
       [else (W (car lat) (cdr lat))]))))

(two-in-a-row? '(a b c d e e f))


;; sum-of-prefixes

(define sum-of-prefixes
  (lambda (tup)
    (letrec
        [(S (lambda (sss tup)
              (cond
                [(null? tup) '()]
                [else (cons (+ sss (car tup))
                            (S (+ sss (car tup)) (cdr tup)))])))]
      (S 0 tup))))

(sum-of-prefixes '(1 1 1 1 1))


;; scramble

(define scramble
  (lambda (tup)
    (letrec
        [(P (lambda (tup rp)
              (cond
                [(null? tup) '()]
                [else (cons (pick (car tup) (cons (car tup) rp))
                            (P (cdr tup) (cons (car tup) rp)))])))]
      (P tup '()))))

(scramble '(1 2 3 4 5 6 7 8 9))