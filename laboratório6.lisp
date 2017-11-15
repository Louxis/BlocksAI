;;;; laboratorio6.lisp
;;;; Disciplina de IA - 2017 / 2018
;;;; Ficha de Laboratório nº6 - Apoio ao 1º projeto
;;;; Autor: 


;;; Tabuleiros

(defun tabuleiro-vazio (&optional (dimensao 14))
  "Retorna um tabuleiro 14x14 (default) com as casas vazias"
	(make-list dimensao :initial-element (make-list dimensao :initial-element '0))
)

(defun tabuleiro-teste ()
  "Retorna um tabuleiro de teste 14x14 com 4 quadrados 1x1, 1 quadrado 2x2 e 1 cruz"
	'(
	(0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	(0 0 0 0 0 0 0 0 0 0 1 0 0 0)
	(0 0 0 0 0 0 0 0 0 1 1 1 0 0)
	(0 0 0 0 0 0 0 0 0 0 1 0 0 0)
	(0 0 0 0 0 0 0 0 1 1 0 0 0 0)
	(0 0 0 0 0 0 0 0 1 1 0 1 0 0)
	(0 0 0 0 0 0 0 0 0 0 1 0 1 0)
	(0 0 0 0 0 0 0 0 0 0 0 0 0 1)
	)
)


;;; Exercicios
(defun linha (indice tabuleiro)
  (nth indice tabuleiro)
)

(defun coluna (indice tabuleiro)
  (mapcar #'(lambda (linha &aux (n-coluna (nth indice linha))) n-coluna) tabuleiro)
)

(defun celula (l c tabuleiro)
  (nth c (linha l tabuleiro))
)

(defun casa-vaziap (l c tabuleiro)
  (cond ((not (= (celula l c tabuleiro) 0)) nil)
        (t t))
)

(defun verifica-casas-vazias (tabuleiro posicoes)
  (mapcar #' (lambda (casa) (casa-vaziap (first casa) (second casa) tabuleiro)) posicoes)
)

(defun substituir-posicao (indice lista &optional (valor 1))
  (cond ((or(null lista) (not (numberp indice))) nil)
        ((or (< indice 0) (> indice (length lista))) nil)
        ((= indice 0) (cons valor (cdr lista)))
        (t (cons (car lista) (substituir-posicao (- indice 1) (cdr lista) valor)))
   )
)

(defun substituir (l c tabuleiro &optional (valor 1))
  (substituir-posicao l tabuleiro (substituir-posicao c (linha l tabuleiro) valor))
)
