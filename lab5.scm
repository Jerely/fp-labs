#lang racket

;; Задание 1
(define (process-text text old-word new-word)
  (map (lambda (sentence)
         (map (lambda (word)
                (if (equal? word old-word)
                    new-word
                    word))
              sentence))
       text))

(define test-text
'(
  (one two three)
  (five six one)
  (seven one eight)
  (nine ten eleven)))

test-text
(process-text test-text 'one 'two)

;; Задание 2

(define (vowel? char)
  (if (regexp-match #rx"[ауоыиэяюёеАУОЫИЭЯЮЁЕ]"
                    (string char))
      #t
      #f))

(define (consonant? char)
  (if (regexp-match #rx"[бвгджзйклмнпрстфхцчшщБВГДЖЗЙКЛМНПРСТФХЦЧШЩ]"
                    (string char))
      #t
      #f))

(define (delimiter? char)
  (if (regexp-match #rx"[ьъЬЪ]"
                    (string char))
      #t
      #f))

(define (sonorous? char)
  (if (regexp-match #rx"[лмнрйЛМНРЙ]"
                    (string char))
      #t
      #f))

(define (skip-syllables chars)
  (let loop ([chars chars]
             [syllable null])
    (if (null? chars)
        (cons chars syllable)
        (if (not (consonant? (car chars)))
            (cons chars syllable)
            (loop (cdr chars)
                  (cons (car chars) syllable))))))

(define (word->syllables word)
  (reverse
   (map
    (compose list->string reverse)
    (let loop ([chars (string->list word)]
               [syllables null])
      (let* ([chars-syl (skip-syllables chars)]
             [chars (car chars-syl)]
             [syllable (cdr chars-syl)])
        (match chars
               [`(,x ,y . ,z) #:when (and (delimiter? x)
                                          (vowel? y)
                                          (list? z))
                (loop (cdr chars)
                      (cons (append (cons x syllable)
                                    (car syllables))
                            (cdr syllables)))]
               [`(,x ,y ,z . ,w) #:when (and (vowel? x)
                                             (sonorous? y)
                                             (consonant? z)
                                             (list? w))
                (loop (cddr chars)
                      (cons `(,y ,x . ,syllable)
                            syllables))]
               [`(,x ,y ,z . ,w) #:when (and (vowel? x)
                                             (consonant? y)
                                             (delimiter? z)
                                             (list? w))
                (loop (cdddr chars)
                      (cons `(,z ,y ,x . ,syllable)
                            syllables))]
               [(cons x y) #:when (and (vowel? x)
                                       (list? y))
                (loop (cdr chars)
                      (cons (cons x syllable)
                            syllables))]
               [null
                (if (not (pair? syllables))
                    null
                    (cons (append syllable
                                  (car syllables))
                          (cdr syllables)))]
               [_
                syllables]))))))

(define (sentence->syllables sentence)
  (map
    word->syllables
    (string-split sentence)))

(sentence->syllables "Католическая философия в том смысле в котором я буду применять этот термин это философское направление господствовавшее в европейской мысли со времен Августина до эпохи возрождения")

;; Задание 3
(define (gossip keyword sentence)
  (substring 
   (foldl (lambda (str1 str2)
            (string-append str2 " " str1))
          ""
          (map glue-phrase
               (let ([keyword (word->syllables keyword)]
                     [words (filter pair? (sentence->syllables sentence))])
                 (map (curry encode-word keyword)
                      words))))
   1))
         
(define (encode-word keyword word)
      (cons (cons (car keyword) (cdr word))
            (list (cons (car word) (cdr keyword)))))

(define (glue-phrase phrase)
  (string-append
   (apply string-append
          (car phrase))
   " "
   (apply string-append
          (cadr phrase))))


"Католическая философия в том смысле в котором я буду применять этот термин это философское направление господствовавшее в европейской мысли со времен Августина до эпохи возрождения"
(gossip "груша" "Католическая философия в том смысле в котором я буду применять этот термин это философское направление господствовавшее в европейской мысли со времен Августина до эпохи возрождения")


;; Задание 4

(define (gypsy keyword sentence)
  (let ([result
         (foldl (lambda (str1 str2)
                  (string-append str1 " " str2))
                ""
                (map glue-phrase
                     (let ([words (filter pair? (sentence->syllables sentence))]
                           [keyword (word->syllables keyword)])
                       (let loop ([words words]
                                  [processed null])
                         (match words
                                [`(,x ,y . ,z)
                                 (loop (cdr words)
                                       (cons (encode-word y x) processed))]
                                [`(,x . ,z) #:when (null? z)
                                 (cons (encode-word keyword x) processed)]
                                [_
                                 processed])))))])
    (substring result 0 (- (string-length result) 1))))

"Католическая философия в том смысле в котором я буду применять этот термин это философское направление господствовавшее в европейской мысли со времен Августина до эпохи возрождения"
 (gypsy "груша" "Католическая философия в том смысле в котором я буду применять этот термин это философское направление господствовавшее в европейской мысли со времен Августина до эпохи возрождения")
