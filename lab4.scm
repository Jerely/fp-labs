#lang racket

;; Задание 1
(define (fac n)
  (let loop ([n n]
             [prod 1])
    (if (<= n 1)
        prod
        (loop (- n 1)
              (* prod n)))))

;; Задание 2

(define (simplify f)
  (let ([szero? (lambda (x)
                      (if (not (number? x))
                          #f
                          (zero? x)))])
    (match f
           [`(+ ,a ,b) #:when (and (number? a) (number? b)) (+ a b)]
           [`(- ,a ,b) #:when (and (number? a) (number? b)) (- a b)]
           [`(* ,a ,b) #:when (and (number? a) (number? b)) (* a b)]
           [`(/ ,a ,b) #:when (and (number? a) (number? b)) (/ a b)]
           [`(* ,a 0) 0]
           [`(* 0 ,a) 0]
           [`(+ 0 ,a) a]
           [`(+ ,a 0) a]
           [`(* ,a ,b ,c) #:when (or (szero? a) (szero? b) (szero? c)) 0]
           [`(* 1 ,a) a]
           [`(* ,a 1) a]
           [`(- ,a) #:when (number? a) (- a)]
           [`(,o ,a ,b) #:when (or (pair? a) (pair? b))  (let ([sa (simplify a)]
                                                               [sb (simplify b)])
                                                               (if (and (equal? sa a)
                                                                        (equal? sb b))
                                                                   `(,o ,sa ,sb)
                                                                   (simplify `(,o ,sa ,sb))))]
           [`(,o ,a ,b ,c) `(,o ,(simplify a) ,(simplify b) ,(simplify c))]
           [_ f]
           )))

(define (deriv f x)
  (let ([constant? (lambda (y)
                     (not (or (list? y)
                              (equal? x y))))]
        [x? (curry equal? x)])
    (match f
           [(? constant?) 0]
           [(? x?) 1]
           [`(- ,U) `(- ,(deriv U x))]
           [`(+ ,U ,V) `(+ ,(deriv U x) ,(deriv V x))]
           [`(- ,U ,V) `(- ,(deriv U x) ,(deriv V x))]
           [`(* ,c ,U) #:when (constant? c) `(* ,c ,(deriv U x))]
           [`(* ,U ,c) #:when (constant? c) `(* ,c ,(deriv U x))]
           [`(* ,U ,V) `(+ (* ,U ,(deriv V x)) (* ,V ,(deriv U x)))]
           [`(/ ,U ,V) (deriv `(* ,U (expt ,V -1)) x)]
           [`(expt ,U ,c) #:when (constant? c) `(* ,c (expt ,U (- ,c 1)) ,(deriv U x))]
           [`(log ,U) `(* (expt ,U -1) ,(deriv U x))]
           [`(,U) (deriv U x)]
           )))

(define-syntax-rule (test expr)
  (begin (quote expr)
         (simplify expr)))

(test (deriv 'y 'x))
(test (deriv 'x 'x))
(test (deriv '(- x) 'x))
(test (deriv '(+ x y) 'x))
(test (deriv '(- y x) 'x))
(test (deriv '(* c x) 'x))
(test (deriv '(* x c) 'x))
(test (deriv '(* (* x x) y) 'x))
(test (deriv '(/ x y) 'x))
(test (deriv '(/ y x) 'x))
(test (deriv '(expt x 3) 'x))
(test (deriv '(log x) 'x))
(test (deriv '(+ (* x x) (* 2 x)) 'x))
