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
               ;; [(cons x (cons y z)) #:when (and (delimiter? x)
               [`(,x ,y . ,z) #:when (and (delimiter? x)
                                          (vowel? y)
                                          (list? z))
                (loop (cdr chars)
                      (cons (append (cons x syllable) (car syllables)) (cdr syllables)))]
               [(cons x (cons y (cons z w))) #:when (and (vowel? x)
                                                         (sonorous? y)
                                                         (consonant? z)
                                                         (list? w))
                (loop (cddr chars)
                      (cons (cons y (cons x syllable)) syllables))]
               [(cons x (cons y (cons z w))) #:when (and (vowel? x)
                                                         (consonant? y)
                                                         (delimiter? z)
                                                         (list? w))
                (loop (cdddr chars)
                      (cons (cons z (cons y (cons x syllable))) syllables))]
               [(cons x y) #:when (and (vowel? x)
                                       (list? y))
                (loop (cdr chars)
                      (cons (cons x syllable) syllables))]
               [(? null?)
                (if (not (pair? syllables))
                    null
                    (cons (append syllable
                                  (car syllables))
                          (cdr syllables)))]
               [_
                syllables]))))))

(define (split-sentence sentence)
  (reverse
   (map
    (compose list->string reverse)
    (let loop ([chars (string->list sentence)]
               [words '(())])
      (if (null? chars)
          words
          (if (equal? (car chars) #\ )
              (loop (cdr chars)
                    (cons '() words))
              (loop (cdr chars)
                    (cons (cons (car chars) (car words)) (cdr words)))))))))

(define (sentence->syllables sentence)
  (map
    word->syllables
    (split-sentence sentence)))

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


 (gossip "груша" "Католическая философия в том смысле в котором я буду применять этот термин это философское направление господствовавшее в европейской мысли со времен Августина до эпохи возрождения")