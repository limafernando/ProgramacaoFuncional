#lang racket

(require rackunit rackunit/text-ui)

;; --- Exercício 2 Questão 1  ---------------------

(define (mult-acc m n acc)
  (if (zero? m)
  acc
  (mult-acc n (sub1 m) (+ n acc)))
)

(define-test-suite testes-mult-acc
  (test-equal? "3 * 4"  (mult-acc 3 4 0)    12)
  (test-equal? "5 * 0"  (mult-acc 5 0 0)    0)
  (test-equal? "0 * 5"  (mult-acc 0 5 0)    0)
  (test-equal? "13 * 1" (mult-acc 13 1 0)   13)
  (test-equal? "1 * 13" (mult-acc 1 13 0)   13))


;; --- Exercício 2 Questão 2 ---------------------
(define (sub-acc m n acc)
  (if (zero? n)
      acc
      (sub-acc m (sub1 n) (sub1 acc))) ;;acc iniciado com o primeiro valor
)


(define-test-suite testes-sub-acc
  (test-equal? "42 - 0"  (sub-acc 42 0 42)   42)
  (test-equal? "32 - 16" (sub-acc 32 16 32)  16)
  (test-equal? "42 - 42" (sub-acc 42 42 42)  0)
  (test-equal? "11 - 10" (sub-acc 11 10 11)  1))

;; --- Exercício 2 Questão 3 ---------------------

;;As funções par e impar já são tail recursive

(define (par n)
  (if (zero? n)
      #t
      (if (< n 0) #f (par (sub1(sub1 n)))))
)

(define (impar n)
  (if (par n) #f #t) 
)

;; --- Exercício 2 Questão 4 ---------------------

;; Não se aplica

;; --- Exercício 2 Questão 5 ---------------------

;; Não se aplica

;; --- Exercício 2 Questão 6 ---------------------

;;Não se aplica

;; --- Exercício 2 Questão 7 ---------------------

;;Não se aplica

;; --- Exercício 2 Questão 8 ---------------------

(define (soma-lista-acc lst acc)
    (if (empty? lst)
        acc
        (soma-lista-acc (rest lst) (+ (first lst) acc))))

(define-test-suite testes-soma-lista-acc
  (test-equal? "soma da lista vazia"                (soma-lista-acc '() 0)                  0)
  (test-equal? "soma de um número apenas"           (soma-lista-acc '(13) 0)                13)
  (test-equal? "soma de vários números"             (soma-lista-acc (list 5 4 3 2 1) 0)     15)
  (test-equal? "soma de números em ordem diferente" (soma-lista-acc (list 1 2 3 4 5) 0)     15)
  (test-equal? "soma de lista com zero"             (soma-lista-acc (list 1 0 2 0 13 0) 0)  16))

;; --- Exercício 2 Questão 9 ---------------------

(define (mult-lista-acc l acc)
  (if (empty? l)
      acc
      (mult-lista-acc (rest l) (* (first l) acc)))
)

(define-test-suite testes-mult-lista-acc
  (test-equal? "produto da lista vazia"            (mult-lista-acc '() 1)                  1)
  (test-equal? "produto de lista com zero"         (mult-lista-acc (list 1 0 2 0 13 0) 1)  0)
  (test-equal? "produto de um número"              (mult-lista-acc '(55) 1)                55)
  (test-equal? "produto de vários números"         (mult-lista-acc (list 1 2 3 4 5) 1)     120)
  (test-equal? "produto de números em outra ordem" (mult-lista-acc (list 2 5 1 4 3) 1)     120))

;; --- Exercício 2 Questão 10 ---------------------

(define (max-lista-acc l acc)
  (if (empty? l)
      acc
      (max-lista-acc (rest l) (if (>= (first l) acc) (first l) acc))))
                                  

(define-test-suite testes-max-lista-acc
  (test-equal? "maximo da lista vazia"       (max-lista-acc '() 0)                     0)
  (test-equal? "maximo de lista unitaria"    (max-lista-acc '(22) 0)                   22)
  (test-equal? "maximo de lista com numeros" (max-lista-acc (list 8 55 13 24 45) 0)    55)
  (test-equal? "maximo não muda com ordem"   (max-lista-acc (list 45 13 8 55 24) 0)    55)
  (test-equal? "maximo de lista com zeros"   (max-lista-acc (list 1 0 13 0 356 0) 0)   356))

;; --- Exercício 2 Questão 11 ---------------------

;;Já é tail recursive

;; --- Exercício 2 Questão 12 ---------------------

(define (quadrado-lista-acc l acc)
  (if (empty? l)
      (reverse acc)
      (quadrado-lista-acc (rest l) (cons (* (first l) (first l)) acc))))


(define-test-suite testes-quadrado-lista-acc
  (test-equal? "quadrado da lista vazia"  (quadrado-lista-acc '() '())        '())
  (test-equal? "quadrado de um número"    (quadrado-lista-acc '(5) '())       '(25))
  (test-equal? "quadrado de números"
               (quadrado-lista-acc (list 2 5 12 25) '())
               (list 4 25 144 625)))

;; --- Exercício 2 Questão 13 ---------------------

(define (filtra-par-acc l acc)
  (cond [(empty? l) (reverse acc)]
        [(par (first l)) (filtra-par-acc (rest l) (cons (first l) acc))]
        [else (filtra-par-acc (rest l) acc)]))

(define-test-suite testes-filtra-par-acc
  (test-equal? "filtragem da lista vazia"     (filtra-par-acc '() '())                  '())
  (test-equal? "filtragem de lista sem pares" (filtra-par-acc (list 1 3 5 7 9) '())     '())
  (test-equal? "filtragem de lista com pares" (filtra-par-acc (list 1 2 3 4 5) '())     (list 2 4))
  (test-equal? "filtragem com todos os itens pares"
               (filtra-par-acc (list 2 4 22 144) '())
               (list 2 4 22 144)))

;; --- Exercício 3 Questão 1 ---------------------

(define (remove-primeiro-acc x l acc)
  (cond [(empty? l) (reverse acc)]
        [(equal? (first l) x) (append (reverse acc) (rest l))]
        [else (remove-primeiro-acc x (rest l) (cons (first l) acc))]))


(define-test-suite test-remove-primeiro-acc
  (test-equal? "lista vazia"
               (remove-primeiro-acc 5 '() '())              '())
  
  (test-equal? "uma ocorrência"
               (remove-primeiro-acc 5 '(1 3 5 7) '())       '(1 3 7))
  
  (test-equal? "múltiplas ocorrências"
               (remove-primeiro-acc 5 '(1 3 5 7 5 9) '())   '(1 3 7 5 9))
  
  (test-equal? "nenhuma ocorrência"
               (remove-primeiro-acc 3 '(11 7 23 55 42) '()) '(11 7 23 55 42)))

;; --- Exercício 3 Questão 2 ---------------------

(define (remove-todos-acc x l acc)
  (cond [(empty? l) (reverse acc)]
        [(equal? (first l) x) (remove-todos-acc x (rest l) acc)]
        [else (remove-todos-acc x (rest l) (cons (first l) acc))]))

(define-test-suite test-remove-todos-acc
  (test-equal? "lista vazia"           (remove-todos-acc 5 '() '())              '())
  (test-equal? "uma ocorrência"        (remove-todos-acc 5 '(1 3 5 7) '())       '(1 3 7))
  (test-equal? "múltiplas ocorrências" (remove-todos-acc 5 '(1 3 5 7 5 9) '())   '(1 3 7 9))
  (test-equal? "nenhuma ocorrência"    (remove-todos-acc 3 '(11 7 23 55 42) '()) '(11 7 23 55 42)))

;; --- Exercício 3 Questão 3 ---------------------

;;Não se aplica

;; --- Exercício 3 Questão 4 ---------------------

;;A função "pertence?" já é tail recursive

(define (pertence? x lst)
  (if (empty? lst)
      #f
      (if (equal? (first lst) x)
          #t
          (pertence? x (rest lst))))

)


;; --- Exercício 3 Questão 5 ---------------------

(define (combine-acc l1 l2 acc)
  (cond [(empty? l1) (reverse acc)]
        [(empty? l2) (reverse acc)]
        [else (combine-acc (rest l1) (rest l2) (cons (list (first l1) (first l2)) acc))]))


(define-test-suite test-combine-acc
  (test-equal? "listas de mesmo tamanho"
               (combine-acc '(1 2 3) '(10 20 30) '())  '((1 10) (2 20) (3 30)))
  
  (test-equal? "listas de tamanho diferente"
               (combine-acc '(1)     '(55 33 11) '())  '((1 55)))
  
  (test-equal? "primeira lista vazia"
               (combine-acc '()      '(1 2 3) '())     '())
  
  (test-equal? "segunda lista vazia"
               (combine-acc '(1 2 3) '() '())          '())
  
  (test-equal? "segunda lista menor"
               (combine-acc '(4 5 6) '(22 33) '())     '((4 22) (5 33))))

;; --- Exercício 3 Questão 6 ---------------------

;;A função conjunto e subconjunto já são tail recursive

(define (subconjunto c1 c2)
  (if (empty? c1)
      #t
      (if (pertence? (first c1) c2)
          (subconjunto (rest c1) c2)
          #f)
  )
)

(define (conjunto=? c1 c2)
 (and (subconjunto c1 c2) (subconjunto c2 c1))
)

;; --- Exercício 3 Questão 7 ---------------------

(define (remove-duplicatas-acc l acc)
  (if (empty? l)
      (reverse acc)
      (remove-duplicatas-acc (remove-todos-acc (first l) (rest l) '()) (cons (first l) acc))))


;; Note que usamos conjunto=? nos testes, caso contrário funções que retornassem
;; elementos em ordens diferentes não passariam
(define-test-suite test-remove-duplicatas-acc
  (test-true "sem duplicatas"
             (conjunto=? (remove-duplicatas-acc '(1 2 3 4 5) '()) '(1 2 3 4 5)))
  
  (test-true "lista vazia"
             (conjunto=? (remove-duplicatas-acc '() '()) '()))
  
  (test-true "várias duplicatas"
             (conjunto=? (remove-duplicatas-acc '(1 2 3 2 3 5) '()) '(1 2 3 5)))
  
  (test-true "apenas um elemento"
             (conjunto=? (remove-duplicatas-acc   '(5 5 5 5 5 5) '()) '(5)))
  
  (test-true "mais repetições"
             (conjunto=? (remove-duplicatas-acc '(1 2 3 1 2 3 1 2 3 1 2 3) '()) '(1 2 3))))

;; --- Exercício 3 Questão 8 ---------------------

(define (uniao-acc c1 c2 acc) ;;o acumulador tem que ser iniciado com a c2
  (cond [(empty? c1) (reverse acc)]
        [(empty? c2) c1]
        [(uniao-acc (rest c1) (remove-todos-acc (first c1) c2 '()) (cons (first c1) acc))]))


(define-test-suite test-uniao-acc
  (test-true "Vazio é elemento neutro 1"
             (conjunto=? (uniao-acc '() '(1 2 3) '(1 2 3))  '(1 2 3)))
  
  (test-true "Vazio é elemento neutro 2"
             (conjunto=? (uniao-acc '(4 5 6) '() '())  '(4 5 6)))
  
  (test-true "União de vazios"
             (conjunto=? (uniao-acc '() '() '())  '()))
  
  (test-true "Sem elementos em comum"
             (conjunto=? (uniao-acc '(1 2 3) '(4 5 6) '(4 5 6))  '(1 2 3 4 5 6)))
  
  (test-true "Com elementos em comum"
             (conjunto=? (uniao-acc '(1 4 5) '(4 5 6) '(4 5 6))  '(1 4 5 6))))

;; --- Exercício 3 Questão 9 ---------------------

(define (interseccao-acc c1 c2 acc)
  (cond [(empty? c1) (reverse acc)]
        [(empty? c2) (reverse acc)]
        [(pertence? (first c1) c2) (interseccao-acc (rest c1) c2 (cons (first c1) acc))]
        [else (interseccao-acc (rest c1) c2 acc)]))


(define-test-suite test-interseccao-acc
  (test-equal? "Conjuntos vazios"        (interseccao-acc '()      '() '())      '())
  (test-equal? "Intersecção com vazio 1" (interseccao-acc '(1 2 3) '() '())      '())
  (test-equal? "Intersecção com vazio 2" (interseccao-acc '()      '(11 22) '()) '())
  (test-equal? "Sem elementos comuns"    (interseccao-acc '(1 2 3) '(11 22) '()) '())

  (test-true "Um elemento em comum"
             (conjunto=? (interseccao-acc '(1 2 3) '(11 1 121) '())  '(1)))

  (test-true "Vários elementos em comum"
             (conjunto=? (interseccao-acc '(1 3 5 7 9 11)
                                      '(11 3 1 13 17) '())
                         '(1 3 11)))

  (test-true "Mesmo conjunto"
             (conjunto=? (interseccao-acc '(1 2 3 4 5) '(5 4 3 2 1) '())
                         '(1 2 3 4 5))))

;; --- Exercício 3 Questão 10 ---------------------

(define (diferenca-acc c1 c2 acc)
  (cond [(empty? c1) (reverse acc)]
        [(empty? c2) c1]
        [(not (pertence? (first c1) c2)) (diferenca-acc (rest c1) c2 (cons (first c1) acc))]
        [else (diferenca-acc (rest c1) c2 acc)]))


(define-test-suite test-diferenca-acc
  (test-equal? "Conjuntos vazios"        (diferenca-acc '()      '() '())      '())
  (test-equal? "Diferença com vazio 1" (diferenca-acc '(1 2 3) '() '())      '(1 2 3))
  (test-equal? "Diferença com vazio 2" (diferenca-acc '()      '(11 22) '()) '())
  (test-equal? "Sem elementos comuns"    (diferenca-acc '(1 2 3) '(11 22) '()) '(1 2 3))

  (test-true "Um elemento em comum"
             (conjunto=? (diferenca-acc '(1 2 3) '(11 1 121) '())  '(2 3)))

  (test-true "Vários elementos em comum"
             (conjunto=? (diferenca-acc '(1 3 5 7 9 11)
                                      '(11 3 1 13 17) '())
                         '(5 7 9)))

  (test-true "Mesmo conjunto"
             (conjunto=? (diferenca-acc '(1 2 3 4 5) '(5 4 3 2 1) '())
                         '())))

;; --- Executa todos os testes ---------
(run-tests
 (test-suite "todos os testes"
             testes-mult-acc
             testes-sub-acc
             testes-soma-lista-acc
             testes-mult-lista-acc
             testes-max-lista-acc
             testes-quadrado-lista-acc
             testes-filtra-par-acc
             test-remove-primeiro-acc
             test-remove-todos-acc
             test-combine-acc
             test-remove-duplicatas-acc
             test-uniao-acc
             test-interseccao-acc
             test-diferenca-acc))