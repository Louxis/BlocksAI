;;;;Puzzle file
;;;;IA
;;;;José Pereira e Lyudmyla Todoriko
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
	(0 2 2 0 0 2 2 2 0 2 2 2 0 2)
	(0 2 2 0 2 0 2 0 2 0 2 0 2 0)
	(0 0 0 2 2 2 0 2 2 2 0 2 2 2)
	(0 2 2 0 2 0 2 0 2 0 2 0 2 0)
	(0 2 2 0 0 2 2 2 0 2 2 2 0 2)
	(0 0 0 0 2 0 2 0 2 0 2 0 2 0)
	(0 0 0 2 2 2 0 2 2 2 0 2 2 2)
	(0 0 2 1 2 1 2 0 2 0 2 0 2 0)
	(1 2 2 2 1 2 2 2 0 2 2 2 0 0)
	(0 1 2 1 0 0 2 0 2 0 2 0 2 2)
	(1 0 1 2 1 2 0 2 0 2 0 0 2 2)
	))
;;;End of board 

;;;Node 

(defun test-node ()
  (list (list (empty-board)'(10 10 15)) nil 0 (+ 10 (* 10 4) (* 15 5)) 1 2))

(defun create-node (board pieces parent d g h f)
  (list (list board pieces) parent d g h f))

(defun node-board (node)
  (caar node))

(defun node-pieces (node)
  (cadr (car node)))

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



;;;End of operations

;;;Expand



;;;End Expand


;;;??


;;;??