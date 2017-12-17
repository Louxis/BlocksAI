# Manual Técnico

![Cover](cover.png)

**Realizado por**

- José Pereira nº 150221044

- Lyudmyla Todoriko nº 150221059

## 1. Introdução

No âmbito da cadeira de Inteligência Artificial foi nos proposto a criação de toda a lógica do jogo Blokus e de algoritmos para a sua resolução.
Uma vez que estamos a trabalhar numa versão limitada do LispWorks e em contexto académico foi nos solicitado utilizar uma versão mais pequena do Blokus apelidada de Blokus Uno, contendo menos peças, um tabuleiro mais pequeno e apenas para um jogador.
Os algoritmos que foram utilizados e estudados foram o BFS, DFS, A* e IDA*, com duas heurísticas distintas com objetivos diferentes.

## 2. Lógica do Puzzle

### 2.1 Tabuleiro

Uma vez que vamos trabalhar sobre um jogo de tabuleiro será preciso ter funções básicas para manipular o mesmo, seja para selecionar uma linha ou para alterar uma célula.

Para facilitar a obtenção de valores existentes no tabuleiro utilizamos funções para obter linhas, colunas e células baseadas no índice introduzido.

```lisp

(defun line (index board)
  (cond ((not (numberp index)) nil)
        ((< index 0) nil)
        (t (nth index board))))

(defun column (index board)
  (cond ((not (numberp index)) nil)
        ((< index 0) nil)
        (t (mapcar #'(lambda (line &aux (n-column (nth index line))) n-column) board))))

(defun board-cell (x y board)
  (cond ((or (not (numberp x)) (not (numberp y))) nil)
        ((or (< x 0) (< y 0)) nil)
        (t (nth x (line y board)))))

(defun empty-cellp (x y board)
  "Verifies if the x,y cell is empty (contains value 0)"
  (cond ((or (not (numberp x)) (not (numberp y))) nil)
        ((or (< x 0) (< y 0)) nil)
        ((not (eq (board-cell x y board) 0)) nil)
        (t t)))
```

Para podermos visualizar o problema e ser mais fácil de testar criamos uma função que imprime o tabuleiro por linhas, facilitando a sua leitura e mais tarde escrita.

```lisp
(defun board-print (board)
  "Prints board in a easier way to read, in a grid with the board size"
  (cond ((null board) nil)
        ((eq (length board) 1) (format t "~d~%~%" (car board)))
        (t (format t "~d~%" (car board)) (board-print (cdr board)))))
```

Para podermos trocar os valores que existem no tabuleiro e recebermos de volta o tabuleiro alterado foi necessário algumas funções extra.

```lisp
(defun replace-position (index board &optional (value 1))
  "Replace a line in the index of a board"
  (cond ((or(null board) (not (numberp index))) nil)
        ((or (< index 0) (> index (length board))) nil)
        ((= index 0) (cons value (cdr board)))
        (t (cons (car board) (replace-position (- index 1) (cdr board) value)))))

(defun replace-board (x y board &optional (value 1))
  (replace-position y board (replace-position x (line y board) value)))
```

### 2.2 Nós

Para ser mais fácil a gestão do problema e respetivos cálculos criamos um formato para os nós do problema que consistem num estado, o nó pai, o custo do nó e quando necessário o valor de h e o da função f. Adicionalmente o estado do nosso nó vai conter dois elementos, o tabuleiro e uma lista com as peças restantes no formato de (square-1x1 square-2x2 cross).

Tal como no caso do tabuleiro, foi necessário criar funções básicas de manipulação do nó para receber cada elemento do mesmo e a sua criação.

```lisp
(defun node-create (state parent d g h f)
  (list state parent d g h f))

(defun node-state (node)
  (car node))

(defun node-board (node-state)
  (car node-state))

(defun node-pieces (node-state)
  (second node-state))

(defun node-parent (node)
  (cadr node))

(defun node-depth (node)
  (caddr node))

(defun node-cost (node)
  (cadddr node))

(defun node-h (node)
  (nth 4 node))

(defun node-f (node)
  (nth 5 node))
```

Para visualizar o nó e facilitar o trabalho de leitura e escrita foi criada também uma função para imprimir o nó e a sua informação necessária

```lisp
(defun node-print (node)
  "Prints node board, pieces remaining, price, h falue and f value"
  (cond ((null node) nil)
        (t (format t "Original:~%")
           (board-print (node-board (node-state (node-original node)))) 
           (format t "Final:~%")
           (board-print (node-board (node-state node))) 
           (format t "~%Pieces: ~d~%Depth:~d~%Cost:~d~%F=~d~%H=~d~%" (node-pieces (node-state node)) (node-depth node) (node-cost node) (node-f node) (node-h node)))))
```

E para podermos estudar a solução obtida é util saber qual foi o nó de origem e o tamanho da solução existente.

```lisp
(defun node-solution-size (node)
  "Calculates the total solution size of a node (without counting with the original root)"
    (cond ((null (node-parent node)) 0)
          (t (+ 1 (node-solution-size (node-parent node))))))

(defun node-original (node)
  "Gets the node original root"
    (cond ((null (node-parent node)) node)
          (t (node-original (node-parent node)))))
```

### 2.3 Operadores

Tal como foi lecionado na cadeira, partimos o problema nos seus diversos operadores, que na versão pedida é a colocação de um dos três tipos de peças disponíveis (quadrado-1x1, quadrado-2x2 e cruz) numa determinada posição do tabuleiro.

```lisp
(defun operators ()
  "Possible operators to use on Blokus"
  '(CROSS SQUARE-2X2 SQUARE-1X1))

(defun place-square (x y board)
  "Places a 1x1 square on the given x and y of a board"
  (cond ((verify-empty-cells board (block-occupied-cells x y 'square-1x1))  
         (replace-board x y board))
        (t nil)))

(defun update-pieces (pieces type)
  "Subtracts one from the type of piece in the list, returning nil if it wasn't possible"
  (cond ((eq type 'square-1x1) (list (1- (first pieces)) (second pieces) (third pieces)))
        ((eq type 'square-2x2) (list (first pieces) (1- (second pieces)) (third pieces)))
        ((eq type 'cross) (list (first pieces) (second pieces) (1- (third pieces))))
        (t (print "SOMETHING WENT WRONG") nil)))

(defun square-1x1 (x y node)
  "Places a 1x1 square on the board if it is possible and updating the existing pieces on the node"
  (let ((pieces (node-pieces (node-state node))))
    (cond ((eq (first pieces) 0) nil)
          (t (list (place-square x y (node-board (node-state node))) 
                   (update-pieces pieces 'square-1x1))))))

(defun square-2x2(x y node)
  "Places a 2x2 square on the board if it is possible and updating the existing pieces on the node"
         (labels ((square-aux (x y board cells) 
                  (if (null cells) (place-square x y board) 
                    (square-aux (first (first cells)) (second (first cells)) (place-square x y board) (cdr cells)))))
           (let ((pieces (node-pieces (node-state node))))
             (cond ((eq (second pieces) 0) nil)
                   (t (list (square-aux x y (node-board (node-state node)) (block-occupied-cells x y 'square-2x2))
                            (update-pieces pieces 'square-2x2)))))))

(defun cross (x y node)
  "Places a cross (+) on the board if it is possible and updating the existing pieces on the node"
         (labels ((cross-aux (x y board cells) 
                  (if (null cells) (place-square x y board) 
                    (cross-aux (first (first cells)) (second (first cells)) (place-square x y board) (cdr cells)))))
           (let ((pieces (node-pieces (node-state node))))
             (cond ((eq (third pieces) 0) nil)
                   (t (list (cross-aux (1+ x) (1+ y) (node-board (node-state node)) (block-occupied-cells x y 'cross))
                            (update-pieces pieces 'cross)))))))
```

No entanto é preciso ter em contas algumas regras:

1. As peças só podem ser colocadas diagonalmente entre si;

1. As peças nunca podem ser colocadas imediatamente adjacente a outra peça do jogador (mas podem estar adjacentes a uma peça já existente);

1. Caso não exista mais nenhuma peça em campo do jogador, a peça apenas pode ser colocada num dos cantos disponíveis; 

1. Uma peça não pode ser colocada por cima de outra.

Para validar todos estes casos foi preciso uma longa lista de condições, garantindo o bom funcionamento dos nossos operadores.

```lisp
(defun verify-empty-cells (board positions)
  "Verifies if all the given positions are empty"
  (mapcar #' (lambda (board-cell) (empty-cellp (first board-cell) (second board-cell) board)) positions))

(defun empty-positions (board positions) 
  "Returns the positions that are empty from the given position list"
  (apply #'append 
  (mapcar #' (lambda (board-cell &aux (empty (empty-cellp (first board-cell) (second board-cell) board)))               
               (if (and empty (cell-inbounds (first board-cell) (second board-cell) board)) (list board-cell))) positions)))

(defun block-occupied-cells (x y block-type)
  "Calculates the coordinates (x y) that will be occupied by a given block type on a give x and y"
  (cond ((eq block-type 'square-1x1) (list (list x y)))
        ((eq block-type 'square-2x2) (list (list x y) (list x (+ y 1))(list (+ 1 x) y) (list (+ x 1) (+ y 1))))
        ((eq block-type 'cross) (list (list x (+ y 1)) (list (+ x 1) (+ y 1)) (list (+ x 2) (+ y 1))(list (+ x 1) y) (list (+ x 1)(+ y 2))))
        (t nil)))

(defun cell-inbounds (x y board)
  "Verifies if a cell is in a valid board position (between 0 and maximum line length"
  (cond ((and (and (>= y 0) (<= y (1- (length (line 0 board))))) (and (>= x 0) (<= x (1- (length (column 0 board)))))) t)
        (t nil)))

(defun possible-diagonals (x y board block-type)
  "Calculates all the possible diagonals from a specific block on a given x and y"
  (cond ((eq block-type 'square-1x1) (empty-positions board (list (list (1- x) (1- y)) (list (1- x) (1+ y)) 
                                                                  (list (1+ x) (1- y)) (list (1+ x) (1+ y)))))
        ((eq block-type 'square-2x2) (empty-positions board (list (list (- x 2) (- y 2)) (list (+ x 1) (- y 2)) 
                                                                  (list (- x 2) (+ y 1)) (list (+ x 1) (+ y 1)))))
        ((eq block-type 'cross) (empty-positions board (list (list (- x 3) (- y 2)) (list x (- y 3)) (list (+ x 1) y) 
                                                             (list (- x 2) (+ y 1)) (list (- x 3) y) 
                                                             (list x (+ y 1)) (list x (+ y 1)) (list (+ x 1) (- y 2)) 
                                                             (list (- x 2) (- y 3)))))))

(defun not-adjacent-pos (x y board block-type)
  "Verifices if there is any adjacent player piece on a given x and y"
  (cond ((and (eq block-type 'square-1x1) 
              (not (eq (board-cell x (1- y) board) 1)) 
              (not (eq (board-cell x (1+ y) board) 1)) 
              (not (eq (board-cell (1- x) y board) 1)) 
              (not (eq (board-cell (1+ x) y board) 1))) t)
        ((and (eq block-type 'square-2x2) 
              (not (eq (board-cell x (- y 1) board) 1)) 
              (not (eq (board-cell (+ x 1) (- y 1)board) 1)) ;; 
              (not (eq (board-cell (+ x 2) y board) 1)) ;;
              (not (eq (board-cell (+ x 2) (+ y 1) board)1)) 
              (not (eq (board-cell (+ x 1) (+ y 2) board) 1)) 
              (not (eq (board-cell x (+ y 2) board) 1)) 
              (not (eq (board-cell (- x 1) (+ y 1) board) 1)) ;; 
              (not (eq (board-cell (- x 1) y board) 1)) 
              (eq (length (empty-positions board (block-occupied-cells x y block-type))) 
                  (length (block-occupied-cells x y block-type)))) t)
        ((and (eq block-type 'cross) 
              (not (eq (board-cell x y board) 1)) 
              (not (eq (board-cell (+ x 1) (- y 1) board) 1)) 
              (not (eq (board-cell (+ x 2) y board) 1)) 
              (not (eq (board-cell (- x 1) (+ y 1) board) 1)) 
              (not (eq (board-cell (+ x 3) (+ y 1) board) 1)) 
              (not (eq (board-cell x (+ y 2) board) 1)) 
              (not (eq (board-cell (+ x 2) (+ y 2) board) 1)) 
              (not (eq (board-cell (+ x 1) (+ y 3) board) 1)) 
              (eq (length (empty-positions board (block-occupied-cells x y block-type))) 
                  (length (block-occupied-cells x y block-type)))) t)))

(defun valid-diagonals (diagonal-positions board block-type)
  "Returns the valid diagonals"
  (apply #'append(mapcar #'(lambda (position) (cond ((not-adjacent-pos (first position) (second position) board block-type) (list position)))) diagonal-positions)))


(defun possible-block-positions (board block-type)
  "Returns all the possible position to place a block type on the board, following the game rules"
  (labels ((possible-pos-aux (x y board) 
             (cond ((= x 14) (possible-pos-aux 0 (1+ y) board))
                   ((= y 14) nil)
                   ((eq (board-cell x y board) 1) (append (list (valid-diagonals (possible-diagonals x y board block-type) board block-type)) (possible-pos-aux (1+ x) y board)))
                   (t (possible-pos-aux (1+ x) y board)))))  
    (cond ((empty-boardp board) (valid-corner board block-type))
          (t (remove-duplicates (apply #'append (possible-pos-aux 0 0 board)) :test #'equal-coords)))))

(defun valid-corner (board block-type)
  (cond ((eq block-type 'square-1x1) (empty-positions board '((0 0) (0 13) (13 0) (13 13)))) 
        ((eq block-type 'square-2x2) (apply #'append (mapcar #'(lambda (pos &aux (x (first pos)) (y (second pos))) 
                                                            (if (= (length (empty-positions board (block-occupied-cells x y 'square-2x2))) 4) (list (list x y))))
                                             '((0 0) (0 12) (12 0) (12 12)))))
        (t nil)))

(defun equal-coords (coorda coordb)
  (and (= (car coorda) (car coordb)) (= (cadr coorda) (cadr coordb))))

;;Checks if there is any "1" piece on the board
(defun empty-boardp (board)
  "Verifies that the board is empty"
  (labels ((possible-pos-aux (x y board) 
             (cond ((= x 14) (possible-pos-aux 0 (1+ y) board))
                   ((= y 14) t)                   
                   ((eq (board-cell x y board) 1) nil )
                   (t (possible-pos-aux (1+ x) y board)))))    
    (possible-pos-aux 0 0 board)))
```


bfs

|            | A   | B   |C    |D    |E  |F  |
| ----------:|:---:|:---:|:---:|:---:|:-:|:-:|
| Gerados    |6    |16   |47   |85   |N/A|N/A|
| Explorados |1    |2    |9    |13   |N/A|N/A|
| Penetrância|0.17 |0.13 |0.06 |0.04 |N/A|N/A|
| Ramificação|6    |3.531|3.221|4.017|N/A|N/A|
| Tempo(ms)  |2    |4    |18   |48   |N/A|N/A|

