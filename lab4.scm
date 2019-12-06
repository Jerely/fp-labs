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


;; Задание 3
(define (lab arg)
  (map
   (lambda (l)
     (if (not (list? l))
         l
         (foldl
          (lambda (arg acc)
            (let add ([arg arg]
                      [acc acc])
              (match arg
                     [(? number?) (+ arg acc)]
                     [(? pair?) (foldl add acc arg)]
                     [_ acc])))
          0
          l)))
   arg))

(define (add arg
             acc)
  (match arg
         [(? number?) (+ arg acc)]
         [(? pair?) (foldl add acc arg)]
         [_ acc]))
                
(lab '('(1 2 3 4 5) a '(1.2 3.4 5 6) 1 '(1 a 2 b 3 c) '() '(1 a 2.3 b '(4 c 5.6 d 7) e '(8.9 f 10)) 1.0 '(1 -2 3 -4)))

;; Задание 4
(define (my-eval xpr)
  (if (pair? xpr)
      (let ([operator (car xpr)]
            [operands (cdr xpr)])
        (match operator
               ['+ (my-apply + operands)]
               ['- (my-apply - operands)]
               ['* (my-apply * operands)]
               ['/ (my-apply / operands)]
               ['car (car (my-eval (car operands)))]
               ['cdr (cdr (my-eval (car operands)))]
               ['cons (cons (my-eval (car operands))
                            (my-eval (cadr operands)))]
               ['union (union (my-eval (car operands))
                              (my-eval (cadr operands)))]
               [_ (cadr xpr)]))
      xpr))

(define (my-apply operation operands)
  (let loop ([acc (my-eval (car operands))]
             [operands (cdr operands)])
    (if (null? operands)
        acc
        (loop (operation acc
                         (my-eval (car operands)))
              (cdr operands)))))

;; Задание 5
(define (union set1 set2)
  (if (null? set1)
      set2
      (cons (car set1)
            (union (cdr set1)
                   set2))))
