;;;; laboratorio6.lisp
;;;; Disciplina de IA - 2017 / 2018
;;;; Ficha de Laboratório nº6 - Apoio ao 1º projeto
;;;; Autor: 


;;; Tabuleiros

(defun empty-board (&optional (board-size 14))
  "Retorna um tabuleiro 14x14 (default) com as casas vazias"
	(make-list board-size :initial-element (make-list board-size :initial-element '0))
)

(defun test-board ()
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
(defun line (index board)
  (cond
   ((not (numberp index)) nil)
   ((< index 0) nil)
   (t (nth index board))
  )
)

(defun column (index board)
(cond
  ((not (numberp index)) nil)
  ((< index 0) nil)
  (t (mapcar #'(lambda (line &aux (n-column (nth index line))) n-column) board))))

(defun board-cell (x y board)
  (cond
   ((or (not (numberp x)) (not (numberp y))) nil)
   ((or (< x 0) (< y 0)) nil)
   (t (nth x (line y board)))))

(defun empty-cellp (x y board)
  (cond ((or (not (numberp x)) (not (numberp y))) nil)
        ((or (< x 0) (< y 0)) nil)
        ((not (eq (board-cell x y board) 0)) nil)
        ;;((and (= (length (column 0 board)) x) (= (length (line 0 board)) y)) nil)
        (t t))
)

(defun verify-empty-cells (board positions)
  (mapcar #' (lambda (board-cell) (empty-cellp (first board-cell) (second board-cell) board)) positions)
)

(defun empty-positions (board positions)  
  (apply #'append 
  (mapcar #' (lambda (board-cell &aux (empty (empty-cellp (first board-cell) (second board-cell) board)))               
               (if (and empty (cell-inbounds (first board-cell) (second board-cell) board)) (list board-cell))) positions)))

(defun replace-position (index board-list &optional (value 1))
  (cond ((or(null board-list) (not (numberp index))) nil)
        ((or (< index 0) (> index (length board-list))) nil)
        ((= index 0) (cons value (cdr board-list)))
        (t (cons (car board-list) (replace-position (- index 1) (cdr board-list) value)))
   )
)

(defun replace-board (x y board &optional (value 1))
  (replace-position x board (replace-position y (line x board) value))
)

(defun block-occupied-cells (x y block-type)
  (cond ((eq block-type 'quadrado-1x1) (list (list x y)))
        ((eq block-type 'quadrado-2x2) (list (list x y) (list x (+ y 1))(list (+ 1 x) y) (list (+ x 1) (+ y 1))))
        ((eq block-type 'cruz) (list (list x (+ y 1)) (list (+ x 1) (+ y 1)) (list (+ x 2) (+ y 1))(list (+ x 1) y) (list (+ x 1)(+ y 2))))
        (t nil))
)

(defun square-1x1 (x y board)
  (cond ((verify-empty-cells board (block-occupied-cells x y 'quadrado-1x1))  
         (replace-board x y board))
        (t nil))
)

(defun square-2x2(x y board)
         (labels ((square-aux (x y board cells) 
                  (if (null cells) (square-1x1 x y board) 
                    (square-aux (first (first cells)) (second (first cells)) (square-1x1 x y board) (cdr cells))))) (square-aux x y board (block-occupied-cells x y 'quadrado-2x2))))

;;senpai might want to clean this...thing
(defun cross (x y board)
         (labels ((cross-aux (x y board cells) 
                  (if (null cells) (square-1x1 x y board) 
                    (cross-aux (first (first cells)) (second (first cells)) (square-1x1 x y board) (cdr cells)))))
           (cross-aux (1+ x) (1+ y) board (block-occupied-cells x y 'cruz))))

(defun cell-inbounds (x y board)
  (cond ((and (and (>= y 0) (<= y (1- (length (line 0 board))))) (and (>= x 0) (<= x (1- (length (column 0 board)))))) t)
        (t nil)))

(defun possible-diagonals (x y board block-type)
  (cond ((eq block-type 'quadrado-1x1) (empty-positions board (list (list (1- x) (1- y)) (list (1- x) (1+ y)) (list (1+ x) (1- y)) (list (1+ x) (1+ y)))))
        ((eq block-type 'quadrado-2x2) (empty-positions board (list (list (- x 2) (- y 2)) (list (+ x 1) (- y 2)) (list (- x 2) (+ y 1)) (list (+ x 1) (+ y 1))))))
)

(defun not-adjacent-pos (x y board block-type)
  (cond ((and (eq block-type 'quadrado-1x1) (and (not (eq (board-cell x (1- y) board) 1)) (not (eq (board-cell x (1+ y) board) 1)) (not(eq (board-cell (1- x) y board) 1)) (not(eq (board-cell (1+ x) y board) 1)))) t)
        ((and (eq block-type 'quadrado-2x2) (and (not (eq (board-cell x (- y 1) board) 1)) (not (eq (board-cell (+ x 1) (- y 1)board) 1)) (not (eq (board-cell (+ x 2) y board) 1)) (not (eq (board-cell (+ x 2) (+ y 1) board)1)) (not (eq (board-cell (+ x 1) (+ y 2) board) 1)) (not (eq (board-cell x (+ y 2) board) 1)) (not (eq (board-cell (- x 1) (+ y 1) board) 1)) (not (eq (board-cell (- x 1) y board) 1)) (eq (length (empty-positions board (block-occupied-cells x y block-type))) (length (block-occupied-cells x y block-type))) )) t))
)

(defun valid-diagonals (diagonal-positions board block-type)
  (apply #'append(mapcar #'(lambda (position) (cond ((not-adjacent-pos (first position) (second position) board block-type) (list position)))) diagonal-positions))
)


(defun possible-block-positions (board block-type)
  (labels ((possible-pos-aux (x y board) 
             (cond ((= x 14) (possible-pos-aux 0 (1+ y) board))
                   ((= y 14) nil)
                   ((eq (board-cell x y board) 1) (append (list (valid-diagonals (possible-diagonals x y board block-type) board block-type)) (possible-pos-aux (1+ x) y board)))
                   (t (possible-pos-aux (1+ x) y board)))))
    
    (remove-duplicates (apply #'append (possible-pos-aux 0 0 board)) :test #'equal-coords)))

(defun equal-coords (coorda coordb)
  (and (= (car coorda) (car coordb)) (= (cadr coorda) (cadr coordb))))
