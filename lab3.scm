#lang racket

;; Задание 1 

;получить iтый элемент
(define (get a i)
  (if (= i 0)
      (car a)
      (get (cdr a)
           (- i 1))))

;изменить iтый элемент
(define (set a i v)
  (if (= i 0)
      (cons v (cdr a))
      (cons (car a)
            (set (cdr a)
                 (- i 1)
                 v))))

;получить длину списка
(define (lengf lst)
  (let while ([i 0]
              [a lst])
    (if (not (pair? a))
        i
        (while (+ i 1)
               (cdr a)))))


;сортировка Шелла
(define (shell lst)

  ;вычислить длину списка
  (let ([len (lengf lst)])

    ;получить шаг, который будет как можно больше, но меньше длины списка
    (define (init-gap)
      (let while ([gap 1])
        (if (>= gap len)
            gap
            ;следующий шаг получить по формуле x[k+1] = x[k]*2 + 1
            (while (+ 1 (* 2 gap))))))

    ;сама сортировка
    (let while ([gap (init-gap)]
                [lt lst])
      ;если шаги закончились, возвращаем отсортированный список
      (if (<= gap 0)
          lt
          ;получить следующий шаг
          (while (quotient gap 2)
                 ;проходимся по списку с заданным шагом
                 (let while2 ([i gap]
                              [l lt])
                   ;если если проход завершен, возвращаем список
                   (if (>= i len)
                       l
                       ;temp -- элемент, с которым мы сравниваем все остальные
                       (let ([temp (get l i)])
                         (while2 (+ i 1)
                                 (let while3 ([j i]
                                              [a l])
                                   ;сравниваем
                                   (if (or (< j gap)
                                           (>= temp (get a [- j gap])))
                                       (set a j temp)
                                       ;если a[j-gap] больше temp, делаем перестановку
                                       (while3 (- j gap)
                                               (set a j (get a [- j gap]))))))))))))))


(shell '(20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))

;; Задание 2 
(define (split-by l p k)
  (let loop ([low '()]
             [high '()]
             [l l])
    (cond [(null? l)
           (k low high)]
          [(p (car l))
           (loop low
                 (cons (car l) high)
                 (cdr l))]
          [else
           (loop (cons (car l) low)
                 high
                 (cdr l))])))
 
(define (quicksort l)
  (if (null? l)
      '()
      (split-by (cdr l) 
                (lambda (x) (> x (car l)))
                (lambda (low high)
                  (append (quicksort low)
                          (list (car l))
                          (quicksort high))))))
 
(quicksort '(1 3 5 7 9 8 6 4 2))
(quicksort '(9 8 7 6 5 4 3 2 1))

;; Задание 3
(define (merge a b)
  (if (not (pair? a))
      b
      (if (not (pair? b))
          a
          (if (> (car a)
                 (car b))
              (cons (car b)
                    (merge a
                           (cdr b)))
              (cons (car a)
                    (merge b
                           (cdr a)))))))

(merge '(1 3 5 7 9) '(2 4 6 8 10))

;; Задание 4
(define (rem lst)
  (if (not (pair? lst))
      lst
      (if (pair? (car lst))
          (cons (rem (car lst))
                (rem (cdr lst)))
          (if (integer? (car lst))
              (cons (remainder (car lst) 2)
                    (rem (cdr lst)))
              (cons (car lst)
                    (rem (cdr lst)))))))

(rem '((1 2 (4 5) b 3) a 1 2 3 4 5 6 7 8 9 10))
