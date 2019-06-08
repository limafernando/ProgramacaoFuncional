#lang racket
(require rackunit rackunit/text-ui)

;Leap - Given a year, report if it is a leap year.

(define (leap ano)
  (if (zero? (remainder ano 400))
      #t
      (if (and (zero? (remainder ano 4)) (not (zero? (remainder ano 100))))
          #t
          #f))
)

(define-test-suite testes-leap
  (test-equal? "ano bissexto divisível por 400"  (leap 2000)    #t)
  (test-equal? "ano bissexto divisível por 4 e não por 100"  (leap 2004)    #t)
  (test-equal? "ano bissexto divisível por 4 e não por 100"  (leap 2008)    #t)
  (test-equal? "não ano bissexto"  (leap 1900)    #f)
  (test-equal? "não ano bissexto"  (leap 1700)    #f))

;Word Count - Given a phrase, count the occurrences of each word in that phrase.

(define (word-count palavra frase acc)

  (if (empty? (string-split frase))
      acc
      (if (equal? (first (string-split frase)) palavra)
          (word-count palavra (string-join (rest (string-split frase))) (add1 acc))
          (word-count palavra (string-join (rest (string-split frase))) acc)))
  
)

(define-test-suite testes-word-count
  (test-equal? "conta palavras"  (word-count "um" "um um um" 0)    3)
  (test-equal? "conta palavras"  (word-count "maria" "eu amei maria e maria me amou" 0)    2))

;Grains - Calculate the number of grains of wheat on a chessboard given that the number on each square doubles.

(define (grains num_graos qtd_no_quadrado acc)
  (if (equal? acc 64)
      num_graos
      (grains (+ num_graos qtd_no_quadrado) (* qtd_no_quadrado 2) (add1 acc))) 
)

(define-test-suite testes-grains
  (test-equal? "quantidade de grãos"  (grains 0 1 0)    18446744073709551615))

;Two Fer - Create a sentence of the form "One for X, one for me."

(define (two-fer nome quantidade acc-lista)
  (if (zero? quantidade)
      acc-lista
      (if (odd? quantidade)
          (two-fer nome (sub1 quantidade) (cons (string-append "One for " nome) acc-lista))
          (two-fer nome (sub1 quantidade) (cons "One for me" acc-lista))))
)

(define-test-suite testes-two-fer
  (test-equal? "Dividindo 1"  (two-fer "Rogerinho" 1 '())  '("One for Rogerinho"))
  (test-equal? "Dividindo 2"  (two-fer "Rogerinho" 2 '())  '("One for Rogerinho" "One for me"))
  (test-equal? "Dividindo 3"  (two-fer "Rogerinho" 3 '())  '("One for Rogerinho" "One for me" "One for Rogerinho")))

;Nucleotide Count - Given a DNA string, compute how many times each nucleotide occurs in the string.

(define (nucleotide-count dna A_count C_count G_count T_count)
  (if (empty? (string->list dna))
      (cons A_count(cons C_count(cons G_count(cons T_count '()))))
      (if (equal? #\A (first (string->list dna)))
          (nucleotide-count (list->string (rest (string->list dna))) (add1 A_count) C_count G_count T_count)
          (if (equal? #\C (first (string->list dna)))
              (nucleotide-count (list->string (rest (string->list dna))) A_count (add1 C_count) G_count T_count)
              (if (equal? #\G (first (string->list dna)))
                  (nucleotide-count (list->string (rest (string->list dna))) A_count C_count (add1 G_count) T_count)
                  (if (equal? #\T (first (string->list dna)))
                      (nucleotide-count (list->string (rest (string->list dna))) A_count C_count G_count (add1 T_count))
                      (nucleotide-count (list->string (rest (string->list dna))) A_count C_count G_count T_count)))))))

(define-test-suite testes-nucleotide-count
  (test-equal? "conta nucleotides 1"  (nucleotide-count "ACGT" 0 0 0 0) '(1 1 1 1))
  (test-equal? "conta nucleotides 2"  (nucleotide-count "AAACAACTTCGTAAGTATA" 0 0 0 0) '(9 3 2 5)))

(run-tests
 (test-suite "todos os testes"
             testes-leap
             testes-word-count
             testes-grains
             testes-two-fer
             testes-nucleotide-count))
