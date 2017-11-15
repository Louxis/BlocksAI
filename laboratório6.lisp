;;;; laboratorio6.lisp
;;;; Disciplina de IA - 2017 / 2018
;;;; Ficha de Laborat�rio n�6 - Apoio ao 1� projeto
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

(defun peca-casas-ocupadas (x y tipo)
  (cond ((eq tipo 'quadrado-1x1) (list (list x y)))
        ((eq tipo 'quadrado-2x2) (list (list x y) (list x (+ y 1))(list (+ 1 x) y) (list (+ x 1) (+ y 1))))
        ((eq tipo 'cruz) (list (list x (+ y 1)) (list (+ x 1) (+ y 1)) (list (+ x 2) (+ y 1))(list (+ x 1) y) (list (+ x 1)(+ y 2))))
        (t nil))
)

(defun quadrado-1x1 (x y tabuleiro)
  (cond ((verifica-casas-vazias tabuleiro (peca-casas-ocupadas x y 'quadrado-1x1))  
         (substituir x y tabuleiro))
        (t nil))
)



(defun quadrado-2x2zeisgood (x y tabuleiro)
(cond ((verifica-casas-vazias tabuleiro (peca-casas-ocupadas x y 'quadrado-2x2))
         (labels ((quadrado-aux (x y tabuleiro casas) 
                  (if (null casas) (quadrado-1x1 x y tabuleiro) 
                    (quadrado-aux (first (first casas)) (second (first casas)) (quadrado-1x1 x y tabuleiro) (cdr casas))))) (quadrado-aux x y tabuleiro (peca-casas-ocupadas x y 'quadrado-2x2))))))


