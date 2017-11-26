;;;;Puzzle file
;;;;IA
;;;;Jos� Pereira e Lyudmyla Todoriko
;;;;

;;;Board 

(defun empty-board (&optional (board-size 14))
  "Retorna um tabuleiro 14x14 (default) com as casas vazias"
	(make-list board-size :initial-element (make-list board-size :initial-element '0))
)

(defun test-board-a ()
	'(
	(0 0 0 0 2 2 0 0 2 0 2 0 2 0)
	(0 0 0 0 2 2 0 2 2 2 0 2 2 2)
	(0 0 0 2 0 0 2 0 2 0 2 0 2 0)
	(0 0 0 0 0 0 2 2 0 2 2 2 0 2)
	(0 0 0 0 0 0 2 0 2 0 2 0 2 0)
	(0 0 0 0 0 0 0 2 2 2 0 2 2 2)
	(0 0 0 0 0 0 2 0 2 0 2 0 2 0)
	(0 0 0 0 0 0 2 2 0 2 2 2 0 2)
	(0 0 0 0 2 0 2 0 2 0 2 0 2 0)
	(0 0 0 2 1 2 0 2 2 2 0 2 2 2)
	(0 0 2 1 2 1 2 0 2 0 2 0 2 0)
	(1 2 2 2 1 2 2 2 0 2 2 2 0 0)
	(0 1 2 1 0 0 2 0 2 0 2 0 2 2)
	(1 0 1 2 1 2 0 2 0 2 0 0 2 2)
	))

(defun test-board-complete ()
	'(
	(0 0 0 0 2 2 0 0 2 0 2 0 2 0)
	(0 0 0 0 2 2 0 2 2 2 0 2 2 2)
	(0 0 0 2 0 0 2 0 2 0 2 0 2 0)
	(0 2 2 0 0 2 2 2 0 2 2 2 0 2)
	(0 2 2 0 2 0 2 0 2 0 2 0 2 0)
	(0 0 0 2 2 2 0 2 2 2 0 2 2 2)
	(0 2 2 0 2 0 2 0 2 0 2 0 2 0)
	(0 2 2 0 0 2 2 2 0 2 2 2 0 2)
	(0 1 1 0 2 0 2 0 2 0 2 0 2 0)
	(0 1 1 2 2 2 2 2 2 2 0 2 2 2)
	(0 1 2 1 2 1 2 0 2 0 2 0 2 0)
	(1 2 2 2 1 2 2 2 0 2 2 2 0 0)
	(0 1 2 1 0 2 2 0 2 0 2 0 2 2)
	(1 0 1 2 1 2 0 2 0 2 0 0 2 2)
	))


(defun board-print (board)
  (cond ((null board) nil)
        ((eq (length board) 1) (format t "~d~%~%" (car board)))
        (t (format t "~d~%" (car board)) (board-print (cdr board)))))

(defun replace-position (index board-list &optional (value 1))
  (cond ((or(null board-list) (not (numberp index))) nil)
        ((or (< index 0) (> index (length board-list))) nil)
        ((= index 0) (cons value (cdr board-list)))
        (t (cons (car board-list) (replace-position (- index 1) (cdr board-list) value)))))

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
  (cond ((or (not (numberp x)) (not (numberp y))) nil)
        ((or (< x 0) (< y 0)) nil)
        ((not (eq (board-cell x y board) 0)) nil)
        ;;((and (= (length (column 0 board)) x) (= (length (line 0 board)) y)) nil)
        (t t)))

;;;End of board 

;;;Node 

(defun test-node ()
  (list (list (test-board-a)'(4 10 15)) nil 0 (+ 10 (* 10 4) (* 15 5)) 1 2))

(defun test-node-complete ()
  (list (list (test-board-complete)'(0 9 15)) nil 0 (+ 10 (* 10 4) (* 15 5)) 1 2))

(defun test-node-empty ()
  (list (list (empty-board)'(0 9 15)) nil 0 (+ 10 (* 10 4) (* 15 5)) 1 2))

(defun node-print (node)
  (cond ((null node) nil)
        (t (board-print (node-board (node-state node))) 
           (format t "~%State: ~d~%Depth:~d~%" (node-pieces (node-state node)) (node-depth node)))))

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

;;;End of node 

;;;Operations

(defun operators ()
  '(SQUARE-1X1 SQUARE-2X2 CROSS))

(defun place-square (x y board)
  (cond ((verify-empty-cells board (block-occupied-cells x y 'square-1x1))  
         (replace-board x y board))
        (t nil)))

(defun update-pieces (pieces type)
  (cond ((eq type 'square-1x1) (list (1- (first pieces)) (second pieces) (third pieces)))
        ((eq type 'square-2x2) (list (first pieces) (1- (second pieces)) (third pieces)))
        ((eq type 'cross) (list (first pieces) (second pieces) (1- (third pieces))))
        (t (print "SOMETHING WENT WRONG") nil)))

(defun square-1x1 (x y node)
  (let ((pieces (node-pieces (node-state node))))
    (cond ((eq (first pieces) 0) nil)
          (t (list (place-square x y (node-board (node-state node))) 
                   (update-pieces pieces 'square-1x1))))))

(defun square-2x2(x y node)
         (labels ((square-aux (x y board cells) 
                  (if (null cells) (place-square x y board) 
                    (square-aux (first (first cells)) (second (first cells)) (place-square x y board) (cdr cells)))))
           (let ((pieces (node-pieces (node-state node))))
             (cond ((eq (second pieces) 0) nil)
                   (t (list (square-aux x y (node-board (node-state node)) (block-occupied-cells x y 'square-2x2))
                            (update-pieces pieces 'square-2x2)))))))

(defun cross (x y node)
         (labels ((cross-aux (x y board cells) 
                  (if (null cells) (place-square x y board) 
                    (cross-aux (first (first cells)) (second (first cells)) (place-square x y board) (cdr cells)))))
           (let ((pieces (node-pieces (node-state node))))
             (cond ((eq (third pieces) 0) nil)
                   (t (list (cross-aux (1+ x) (1+ y) (node-board (node-state node)) (block-occupied-cells x y 'cross))
                            (update-pieces pieces 'cross)))))))

;;;End of operations

;;;Expand

(defun solution-nodep (node) 
  (cond ((equal (node-pieces node) '(0 0 0)) t)
        ((null (node-expandp node)) t)
        (t nil)))

(defun expand (node operators search &optional (d 0))
  (flet ((expand-node (node operation)
           (let ((positions (possible-block-positions (node-board (node-state node)) operation)))
             (mapcar #'(lambda (position) 
                         (let ((state (funcall operation (first position) (second position) node)))                           
                           (if (not (null state)) (node-create state node (1+ (node-depth node)) 0 0 0))))
                     positions))))
    (mapcar #'(lambda(operation) (expand-node node operation)) operators)))

(defun node-expand (node operators search &optional (d 0))
  (labels ((place-nodes (node operation positions) 
             (cond ((null positions) nil)
                   ((null (funcall operation (first (car positions)) (second (car positions)) node))
                    (place-nodes node operation (cdr positions)))
                   (t (cons (node-create (funcall operation (first (car positions)) (second (car positions)) node)
                                         node 99 0 0 0) (place-nodes node operation (cdr positions)))))))             
    (flet ((expand-node (node operation)             
             (place-nodes node operation (possible-block-positions (node-board (node-state node)) operation))
             ))
      (apply #'append (mapcar #'(lambda(operation) (expand-node node operation)) operators)))))

(defun node-expandp (node)           
  (labels ((place-nodes (node operation positions) 
             (cond ((null positions) nil)
                   ((null (funcall operation (first (car positions)) (second (car positions)) node))
                    (place-nodes node operation (cdr positions)))
                   (t '(t)))))             
    (flet ((expand-node (node operation)             
             (place-nodes node operation (possible-block-positions (node-board (node-state node)) operation))
             ))
      (apply #'append (mapcar #'(lambda(operation) (expand-node node operation)) '(square-1x1 square-2x2 cross))))))

;;;End Expand


;;;Expand aux
(defun verify-empty-cells (board positions)
  (mapcar #' (lambda (board-cell) (empty-cellp (first board-cell) (second board-cell) board)) positions))

(defun empty-positions (board positions)  
  (apply #'append 
  (mapcar #' (lambda (board-cell &aux (empty (empty-cellp (first board-cell) (second board-cell) board)))               
               (if (and empty (cell-inbounds (first board-cell) (second board-cell) board)) (list board-cell))) positions)))

(defun block-occupied-cells (x y block-type)
  (cond ((eq block-type 'square-1x1) (list (list x y)))
        ((eq block-type 'square-2x2) (list (list x y) (list x (+ y 1))(list (+ 1 x) y) (list (+ x 1) (+ y 1))))
        ((eq block-type 'cross) (list (list x (+ y 1)) (list (+ x 1) (+ y 1)) (list (+ x 2) (+ y 1))(list (+ x 1) y) (list (+ x 1)(+ y 2))))
        (t nil)))


(defun cell-inbounds (x y board)
  (cond ((and (and (>= y 0) (<= y (1- (length (line 0 board))))) (and (>= x 0) (<= x (1- (length (column 0 board)))))) t)
        (t nil)))

(defun possible-diagonals (x y board block-type)
  (cond ((eq block-type 'square-1x1) (empty-positions board (list (list (1- x) (1- y)) (list (1- x) (1+ y)) 
                                                                  (list (1+ x) (1- y)) (list (1+ x) (1+ y)))))
        ((eq block-type 'square-2x2) (empty-positions board (list (list (- x 2) (- y 2)) (list (+ x 1) (- y 2)) 
                                                                  (list (- x 2) (+ y 1)) (list (+ x 1) (+ y 1)))))
        ((eq block-type 'cross) (empty-positions board (list (list (- x 3) (- y 2)) (list x (- y 3)) (list (+ x 1) y) 
                                                             (list (- x 2) (+ y 1)) (list (- x 3) y) 
                                                             (list x (+ y 1)) (list x (+ y 1)) (list (+ x 1) (- y 2)) 
                                                             (list (- x 2) (- y 3)))))))

(defun not-adjacent-pos (x y board block-type)
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
  (apply #'append(mapcar #'(lambda (position) (cond ((not-adjacent-pos (first position) (second position) board block-type) (list position)))) diagonal-positions)))


(defun possible-block-positions (board block-type)
  (labels ((possible-pos-aux (x y board) 
             (cond ((= x 14) (possible-pos-aux 0 (1+ y) board))
                   ((= y 14) nil)
                   ((eq (board-cell x y board) 1) (append (list (valid-diagonals (possible-diagonals x y board block-type) board block-type)) (possible-pos-aux (1+ x) y board)))
                   (t (possible-pos-aux (1+ x) y board)))))  
    (cond ((and (eq block-type 'square-1x1) (empty-boardp board)) (list '(0 0) '(0 13) '(13 0) '(13 13)))
          ((and (equal block-type 'square-2x2) (empty-boardp board)) '((0 0) (0 12) (12 0) (12 12)))
          ((and (eq block-type 'cross) (empty-boardp board)) nil)
          (t (remove-duplicates (apply #'append (possible-pos-aux 0 0 board)) :test #'equal-coords)))))

(defun equal-coords (coorda coordb)
  (and (= (car coorda) (car coordb)) (= (cadr coorda) (cadr coordb))))

(defun invert-append (nodes)
  (labels ((invert-nodes (nodes)
             (labels ((invert-node (node) (list (second node) (first node))))
               (cond ((null nodes) nil)
                     (t (cons (invert-node (car nodes)) (invert-nodes (cdr nodes))))))))
    (append nodes (invert-nodes nodes))))

;;Checks if there is any "1" piece on the board
(defun empty-boardp (board)
  (labels ((possible-pos-aux (x y board) 
             (cond ((= x 14) (possible-pos-aux 0 (1+ y) board))
                   ((= y 14) t)                   
                   ((eq (board-cell x y board) 1) nil )
                   (t (possible-pos-aux (1+ x) y board)))))    
    (possible-pos-aux 0 0 board)))
;;;End expand aux