;;;;Puzzle logic
;;;;Made by José Pereira and Lyudmyla Todoriko

(defvar *player1* 1)
(defvar *player2* 2)
;;;Board 
(defun board-print (board)
  "Prints board in a easier way to read, in a grid with the board size"
  (cond ((null board) nil)
        ((eq (length board) 1) (format t "~d~%~%" (car board)))
        (t (format t "~d~%" (car board)) (board-print (cdr board)))))

(defun replace-position (index board &optional (value 1))
  "Replace a line in the index of a board"
  (cond ((or(null board) (not (numberp index))) nil)
        ((or (< index 0) (> index (length board))) nil)
        ((= index 0) (cons value (cdr board)))
        (t (cons (car board) (replace-position (- index 1) (cdr board) value)))))

(defun replace-board (x y board &optional (value 1))
  (replace-position y board (replace-position x (line y board) value)))

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


(defun max-x (board) (length (car board)))

(defun max-y (board) (length board))

;;;End of board 

;;;Node 
(defun node-print (node)
  "Prints node board, pieces remaining, price, h falue and f value"
  (cond ((null node) nil)
        (t ;(format t "Original:~%")
           ;(board-print (node-board (node-state (node-original node)))) 
           ;(format t "Final:~%")
         (format t "~%~%")
           (board-print (node-board (node-state node))) 
           (format t "~%Pieces 1: ~d~%Pieces 2: ~d~%Player 1 Score: ~d~%Player 2 Score: ~d~%" 
                   (node-pieces (node-state node) *player1*) (node-pieces (node-state node) *player2*) 
                   (player-score node *player1*) (player-score node *player2*)))))

(defun player-score (node player)
  (let ((pieces (node-pieces (node-state node) player)))
    (+ (* 1 (first pieces)) (* 4 (second pieces)) (* 5 (third pieces)))))

(defun assert-winner (node)
  (cond ((= (player-score node *player1*) (player-score node *player2*)) 0)
        ((< (player-score node *player1*) (player-score node *player2*)) *player1*)
        (t *player2*)))

(defun node-create (state parent d)
  (list state parent d))

(defun node-state (node)
  (car node))

(defun node-board (node-state)
  (car node-state))

(defun node-pieces (node-state &optional (player 1))
  (cond ((= player 1) (first(second node-state)))
        (t (second (second node-state)))))

(defun node-parent (node)
  (cadr node))

(defun node-depth (node)
  (caddr node))

(defun node-solution-size (node)
  "Calculates the total solution size of a node (without counting with the original root)"
    (cond ((null (node-parent node)) 0)
          (t (+ 1 (node-solution-size (node-parent node))))))

(defun node-original (node)
  "Gets the node original root"
    (cond ((null (node-parent node)) node)
          (t (node-original (node-parent node)))))

(defun node-steps (node)
  "Gets, by order, all the steps taken by the node to the current state"
  (labels ((steps-aux (node)
             (cond ((null node) nil)
                   ((null (node-parent node)) nil)
                   (t (cons (calculate-made-step (node-pieces (node-state node)) (node-pieces (node-state (node-parent node)))) (steps-aux (node-parent node)))))))
    (reverse (steps-aux node))))

(defun calculate-made-play (current-node parent-node &optional (player 1))
  (labels ((find-played-coord (current-moves parent-moves)
             (cond ((null parent-moves) nil)
                   ((not (member (car parent-moves) current-moves :test #'equal)) (car parent-moves))
                   (t (find-played-coord current-moves (cdr parent-moves))))))             
  (let* ((current-board (node-board (node-state current-node))) (parent-board (node-board (node-state parent-node)))
         (operation (calculate-made-step (node-pieces (node-state current-node) player) (node-pieces (node-state parent-node) player)))
         (current-moves (possible-block-positions current-board operation player))
         (parent-moves (possible-block-positions parent-board operation player)))
    (list operation (find-played-coord current-moves parent-moves)))))

(defun calculate-made-step (current-position parent-position)
  "Calculates step taken between states"
  (cond ((null parent-position))
        ((< (nth 0 current-position) (nth 0 parent-position)) 'SQUARE-1X1)
        ((< (nth 1 current-position) (nth 1 parent-position)) 'SQUARE-2X2)
        ((< (nth 2 current-position) (nth 2 parent-position)) 'CROSS)))
;;;End of node 

;;;Operations

(defun operators ()
  "Possible operators to use on Blokus"
  '(CROSS SQUARE-2X2 SQUARE-1X1))

(defun place-square (x y board &optional (player 1))
  "Places a 1x1 square on the given x and y of a board"
  (cond ((verify-empty-cells board (block-occupied-cells x y 'square-1x1))  
         (replace-board x y board player))
        (t nil)))

(defun update-pieces (pieces type)
  "Subtracts one from the type of piece in the list, returning nil if it wasn't possible"
  (cond ((eq type 'square-1x1) (list (1- (first pieces)) (second pieces) (third pieces)))
        ((eq type 'square-2x2) (list (first pieces) (1- (second pieces)) (third pieces)))
        ((eq type 'cross) (list (first pieces) (second pieces) (1- (third pieces))))
        (t (print "SOMETHING WENT WRONG") nil)))

(defun square-1x1 (x y node &optional (player 1))
  "Places a 1x1 square on the board if it is possible and updating the existing pieces on the node"
  (let ((pieces (node-pieces (node-state node) *player1*))
        (other-pieces (node-pieces (node-state node) *player2*))) 
    (cond ((eq (first (node-pieces (node-state node) player)) 0) nil)
          (t (list (place-square x y (node-board (node-state node)) player) 
                   (if (= player *player1*) (list (update-pieces pieces 'square-1x1) other-pieces)
                     (list pieces (update-pieces other-pieces 'square-1x1))))))))

(defun square-2x2(x y node &optional (player 1))
  "Places a 2x2 square on the board if it is possible and updating the existing pieces on the node"
         (labels ((square-aux (x y board cells) 
                  (if (null cells) (place-square x y board player) 
                    (square-aux (first (first cells)) (second (first cells)) (place-square x y board player) (cdr cells)))))
           (let ((pieces (node-pieces (node-state node) *player1*))
                 (other-pieces (node-pieces (node-state node) *player2*)))
             (cond ((eq (second (node-pieces (node-state node) player)) 0) nil)
                   (t (list (square-aux x y (node-board (node-state node)) (block-occupied-cells x y 'square-2x2))
                            (if (= player *player1*) (list (update-pieces pieces 'square-2x2) other-pieces)
                              (list pieces (update-pieces other-pieces 'square-2x2)))))))))


(defun cross (x y node &optional (player 1))
  "Places a cross (+) on the board if it is possible and updating the existing pieces on the node"
         (labels ((cross-aux (x y board cells) 
                  (if (null cells) (place-square x y board player) 
                    (cross-aux (first (first cells)) (second (first cells)) (place-square x y board player) (cdr cells)))))
           (let ((pieces (node-pieces (node-state node) *player1*))
                 (other-pieces (node-pieces (node-state node) *player2*)))
             (cond ((eq (third (node-pieces (node-state node) player)) 0) nil)
                   (t (list (cross-aux (1+ x) (1+ y) (node-board (node-state node)) (block-occupied-cells x y 'cross))
                            (if (= player *player1*) (list (update-pieces pieces 'cross) other-pieces)
                              (list pieces (update-pieces other-pieces 'cross)))))))))

;;;End of operations

;;;Expand

(defun solution-nodep (node &optional (player 1)) 
  (cond ((equal (node-pieces node player) '(0 0 0)) t)
        ((null (node-expand node (operators) player)) t)
        (t nil)))

(defun node-expand (node operators &optional (player 1))
  (labels ((place-nodes (node operation positions player) 
             (cond ((null positions) nil)
                   ;Check if there was any problem or if out of pieces
                   ((null (funcall operation (first (car positions)) (second (car positions)) node player))
                    (place-nodes node operation (cdr positions) player))
                   (t (cons (node-create 
                             (funcall operation (first (car positions)) (second (car positions)) node player)
                             node (1+ (node-depth node)))
                            (place-nodes node operation (cdr positions) player))))))             
    (flet ((expand-node (node operation player)             
             (place-nodes node operation (possible-block-positions (node-board (node-state node)) operation player) player)))
      (apply #'append (mapcar #'(lambda(operation) (expand-node node operation player)) operators)))))

(defun node-expandp (node &optional (player 1))     
  "Faster way to confirm is a node can expand, verifying if there is no possible position to go to"
  (labels ((place-nodes (node operation positions player) 
             (cond ((null positions) nil)
                   ((null (funcall operation (first (car positions)) (second (car positions)) node player))
                    (place-nodes node operation (cdr positions) player))
                   (t '(t)))))             
    (flet ((expand-node (node operation player)             
             (place-nodes node operation (possible-block-positions (node-board (node-state node)) operation) player)
             ))
      (apply #'append (mapcar #'(lambda(operation) (expand-node node operation player)) '(square-1x1 square-2x2 cross))))))

;;;End Expand

;;;Expand aux
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

(defun not-adjacent-pos (x y board block-type &optional (player 1))
  "Verifices if there is any adjacent player piece on a given x and y"
  (cond ((and (eq block-type 'square-1x1) 
              (not (eq (board-cell x (1- y) board) player)) 
              (not (eq (board-cell x (1+ y) board) player)) 
              (not (eq (board-cell (1- x) y board) player)) 
              (not (eq (board-cell (1+ x) y board) player))) t)
        ((and (eq block-type 'square-2x2) 
              (not (eq (board-cell x (- y 1) board) player)) 
              (not (eq (board-cell (+ x 1) (- y 1)board) player)) 
              (not (eq (board-cell (+ x 2) y board) player))
              (not (eq (board-cell (+ x 2) (+ y 1) board) player)) 
              (not (eq (board-cell (+ x 1) (+ y 2) board) player)) 
              (not (eq (board-cell x (+ y 2) board) player)) 
              (not (eq (board-cell (- x 1) (+ y 1) board) player))
              (not (eq (board-cell (- x 1) y board) player)) 
              (eq (length (empty-positions board (block-occupied-cells x y block-type))) 
                  (length (block-occupied-cells x y block-type)))) t)
        ((and (eq block-type 'cross) 
              (not (eq (board-cell x y board) player)) 
              (not (eq (board-cell (+ x 1) (- y 1) board) player)) 
              (not (eq (board-cell (+ x 2) y board) player)) 
              (not (eq (board-cell (- x 1) (+ y 1) board) player)) 
              (not (eq (board-cell (+ x 3) (+ y 1) board) player)) 
              (not (eq (board-cell x (+ y 2) board) player)) 
              (not (eq (board-cell (+ x 2) (+ y 2) board) player)) 
              (not (eq (board-cell (+ x 1) (+ y 3) board) player)) 
              (eq (length (empty-positions board (block-occupied-cells x y block-type))) 
                  (length (block-occupied-cells x y block-type)))) t)))

(defun valid-diagonals (diagonal-positions board block-type &optional (player 1))
  "Returns the valid diagonals"
  (apply #'append(mapcar #'(lambda (position) (cond ((not-adjacent-pos (first position) (second position) board block-type player) (list position)))) diagonal-positions)))


(defun possible-block-positions (board block-type &optional (player 1))
  "Returns all the possible position to place a block type on the board, following the game rules"
  (labels ((possible-pos-aux (x y board) 
             (cond ((= x (max-x board)) (possible-pos-aux 0 (1+ y) board))
                   ((= y (max-y board)) nil)
                   ((eq (board-cell x y board) player) (append (list (valid-diagonals (possible-diagonals x y board block-type) board block-type player)) (possible-pos-aux (1+ x) y board)))
                   (t (possible-pos-aux (1+ x) y board)))))  
    (cond ((empty-boardp board player) (valid-corner board block-type player))
          (t (remove-duplicates (apply #'append (possible-pos-aux 0 0 board)) :test #'equal-coords)))))

(defun valid-corner (board block-type &optional (player 1))
  (cond ((eq block-type 'square-1x1) (cond ((= *player1* player) (empty-positions board '((0 0)))) (t (empty-positions board '((13 13))))))  
        ((eq block-type 'square-2x2) 
         (apply #'append (mapcar #'(lambda (pos &aux (x (first pos)) (y (second pos))) 
                                     (if (= (length (empty-positions board (block-occupied-cells x y 'square-2x2))) 4) (list (list x y))))
                                 (cond ((= *player1* player) '((0 0))) (t '((12 12)))))))
        (t nil)))

(defun equal-coords (coorda coordb)
  (and (= (car coorda) (car coordb)) (= (cadr coorda) (cadr coordb))))

;;Checks if there is any "1" piece on the board
(defun empty-boardp (board &optional (player 1))
  "Verifies that the board is empty"
  (labels ((possible-pos-aux (x y board) 
             (cond ((= x (max-x board)) (possible-pos-aux 0 (1+ y) board))
                   ((= y (max-y board)) t)                   
                   ((eq (board-cell x y board) player) nil )
                   (t (possible-pos-aux (1+ x) y board)))))    
    (possible-pos-aux 0 0 board)))

;;;End expand aux

(defun block-count (board block-type)
  "Used to calculate how many blocks of a certain type exist"
  (labels ((block-count-aux (x y board) 
             (cond ((= x (max-x board)) (block-count-aux 0 (1+ y) board))
                   ((= y (max-y board)) 0)
                   ((and (eq (board-cell x y board) 1) (eq block-type 'square-1x1) 
                         (or (not (eq (board-cell (+ x 1) y board) 1)) (not (cell-inbounds (+ x 1) y board))) 
                         (or (not (eq (board-cell x (+ y 1) board) 1)) (not (cell-inbounds x (+ y 1) board)))
                         (or (not(eq (board-cell x (- y 1) board) 1)) (not (cell-inbounds x (- y 1) board)))
                         (or (not(eq (board-cell (- x 1) y board) 1)) (not (cell-inbounds (- x 1) y board)))) 
                    (+ 1 (block-count-aux (1+ x) y board)))
                   ((and (eq (board-cell x y board) 1) (eq block-type 'square-2x2) 
                         (eq (board-cell (+ x 1) y board) 1) 
                         (eq (board-cell (+ x 1) (+ y 1) board) 1) 
                         (eq (board-cell x (+ y 1) board) 1))
                    (+ 1 (block-count-aux (1+ x) y board)))
                   ((and (eq (board-cell x y board) 1) (eq block-type 'cross) 
                         (eq (board-cell x (+ y 1) board) 1) 
                         (eq (board-cell (+ x 1) (+ y 1) board) 1)
                         (not (eq (board-cell (+ x 1) y board) 1))) 
                    (+ 1 (block-count-aux (1+ x) y board)))
                   (t (+ 0 (block-count-aux (1+ x) y board)))))) 
    (block-count-aux 0 0 board)))