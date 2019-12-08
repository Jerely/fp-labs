#lang racket
"Задание 1"
((lambda (x y z)
  (if (and (pair? x)
           (pair? y)
           (pair? z))
    (list (car x)
          (car y)
          (car z))
    '())) 
  '(D F G H J K) '(1 2 3 4 5 6 (4 5) 4) '(ER RT TY 5 6 6 5))

;Задание 2
(define (get532 x y z)
  (if (or (< (len x) 5)
          (< (len y) 3)
          (< (len z) 2))
    '()
    (list (get x 5)
          (get y 3)
          (get z 2))))

;получить i-ый элемент списка
(define (get x i)
  (if (= i 1)
    (car x)
    (get (cdr x)
         (- i 1))))

;вычисление длины списка с помощью хвостовой рекурсии
(define (len lst)
  (let loop ((l lst)
             (i 0))
    (if (not (pair? l))
      i
      (loop (cdr l)
            (+ i 1)))))

"Задание 2"
(get532 
  '(D F G H J K) '(1 2 3 4 5 6 (4 5) 4) '(ER RT TY 5 6 6 5))

;Задание 3
(define (var18 l)
  (if (pair? l)
    (if (or (pair? (car l))
            (pair? (get l (len l))))
      (if (< (len l) 3)
        #f
        (list? (get l 3)))
      (list (car l)
            (get l (len l))))
    #f))

"Задание 3"

"'()"
(var18 '())

"'(1)"
(var18 '(1))

"'(1 2)"
(var18 '(1 2))

"'('(1 2) 3)"
(var18 '('(1 2) 3))

"'('(1 2) (3 4))"
(var18 '('(1 2) (3 4)))

"'(1 '(2 3))"
(var18 '(1 '(2 3)))

"'(1 2 3 4)"
(var18 '(1 2 3 4))

"'(1 2 (3 4) 5)"
(var18 '(1 2 (3 4) 5))

"'(1 2 (4 5) (6 7))"
(var18 '(1 2 (4 5) (6 7)))

"'((1 2) 3 4 5 6)"
(var18 '((1 2) 3 4 5 6))
