#lang racket
(define (lab l)
  (if (null? l)
      l
      (cons (if (pair? (car l))
                (subs (car l))
                (car l))
            (lab (cdr l)))))

(define (subs lst)
  (let loop ((l lst)
             (sum 0))
    (cond ((number? l) (+ sum l))
          ((pair? l) (loop (cdr l)
                           (+ sum (subs (car l)))))
          (else sum))))

(subs '(1 2 3 4 5))
(subs '(1.2 3.4 5 6))
(subs '(1 a 2 b 3 c))
(subs '(1 a 2.3 b (4 c 5.6 d 7) e (8.9 f 10)))
(subs '(1 -2 3 -4))

(lab '('(1 2 3 4 5) a '(1.2 3.4 5 6) 1 '(1 a 2 b 3 c) '() '(1 a 2.3 b (4 c 5.6 d 7) e (8.9 f 10)) 1.0 '(1 -2 3 -4)))
