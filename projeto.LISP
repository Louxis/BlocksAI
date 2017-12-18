
;;Read file function. Reads the file in the given filepath
(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read stream nil)
          while line
          collect line)))

;;Start function. To start the simulation, this function needs to be called in the listener
(defun start()
  "Start function. To start the simulation, this function needs to be called in the listener"
  (let ((boards (initialize)))
    (labels ((read_board () 
                        (let ((board (choose-board)))
                          (cond 
                           ((not (numberp board)) nil)
                           ((or(< board 0) (> board (length boards))) (read_board))
                           (t (choose-algorithm (nth board boards)))))))(read_board))))

;;Function that introduces simulation to user, greeting him and asking him for filepath
(defun initialize ()
  "Function that introduces simulation to user, greeting him and asking him for filepath"
 (format t "~%Welcome to BlocksAI!~%Please insert the filepath you want to simulate.~%") 
 (read-file (read)))

;;function that asks user which board from file he wants to test
(defun choose-board()
  "Function that asks user which board from file he wants to test"
  (format t "~%Choose the board you want to test~%Type restart to go back to begining.~%Type exit to end program.~%")
  (let ((input (read)))
    (cond
     ((eq input 'exit) (format t "~%Ended Session"))
     ((eq input 'restart) (start))
     (t (*(- input 1) 2)))))

;;Function that asks user which algorithm he wants to test
(defun algorithm-number ()
  "Function that asks user which algorithm he wants to test"
  (format t "~%Please choose the algorithm you want to work with.~%1 - BFS ~%2 - DFS~%3 - A*~%4 - IDA* ~%")
  (read))

;;Auxiliar function tho execute bfs algorithm
(defun bfs-io-aux(board)
 (bfs-it (node-create (list board (list 
                                   (- 10 (block-count board 'square-1x1)) 
                                   (- 10 (block-count board 'square-2x2)) 
                                   (- 15 (block-count board 'cross)))) nil 0 0 0 0 ) 
                     'solution-nodep 'node-expand (operators)))

;;Auxiliar function to execute dfs algorithm asking user to insert the maximum depth
(defun dfs-io-aux (board)
  (format t "~%Please insert maximum depth ~%")
  (let ((max-depth (read)))
    (cond ((numberp max-depth) (dfs-it (node-create (list board (list 
                                   (- 10 (block-count board 'square-1x1)) 
                                   (- 10 (block-count board 'square-2x2)) 
                                   (- 15 (block-count board 'cross)))) nil 0 0 0 0 ) 
                     'solution-nodep 'node-expand (operators) max-depth))
        (t (dfs-io-aux board)))))

;;Auxiliar function to execute a-star with certain heuristic
(defun a-star-io-aux (board)
  (format t "~%Please insert wanted heuristic ~%1 - Default ~%2 - Student Custom~%")
  (let ((heuristic (read)))
    (cond ((and (numberp heuristic) (= heuristic 1)) (a-star (node-create (list board (list 
                                   (- 10 (block-count board 'square-1x1)) 
                                   (- 10 (block-count board 'square-2x2)) 
                                   (- 15 (block-count board 'cross)))) nil 0 0 0 0 )  'solution-nodep 'node-expand-a (operators) 'heuristic-squares 'node-f))
          ((and (numberp heuristic) (= heuristic 2)) (a-star (node-create (list board (list 
                                   (- 10 (block-count board 'square-1x1)) 
                                   (- 10 (block-count board 'square-2x2)) 
                                   (- 15 (block-count board 'cross)))) nil 0 0 0 0 )  'solution-nodep 'node-expand-a (operators) 'heuristic-custom-complex 'node-f))
        (t (a-star-io-aux board)))))

;;Auxiliar function to execute ida-star with certain heuristic
(defun ida-star-io-aux (board)
  (format t "~%Please insert wanted heuristic ~%1 - Default ~%2 - Student Custom~%")
  (let ((heuristic (read)))
    (cond ((and (numberp heuristic) (= heuristic 1)) (ida-star (node-create (list board (list 
                                   (- 10 (block-count board 'square-1x1)) 
                                   (- 10 (block-count board 'square-2x2)) 
                                   (- 15 (block-count board 'cross)))) nil 0 0 0 0 )  'solution-nodep 'node-expand-a (operators) 'heuristic-squares 'node-f))
          ((and (numberp heuristic) (= heuristic 2)) (ida-star (node-create (list board (list 
                                   (- 10 (block-count board 'square-1x1)) 
                                   (- 10 (block-count board 'square-2x2)) 
                                   (- 15 (block-count board 'cross)))) nil 0 0 0 0 )  'solution-nodep 'node-expand-a (operators) 'heuristic-custom-complex 'node-f))
        (t (ida-star-io-aux board)))))

(defun insert-record-file ()
  "Function that asks user to type the file path in which the program will record the results"
    (format t "~%Please insert the filepath where you want to save the results~%")
     (read))

(defun choose-algorithm (board)
  "Function that asks user to choose the algorithm to work with"
  (let ((algorithm (algorithm-number)))
    (cond ((= algorithm 1) (bfs-io-aux board)(results-write-file (insert-record-file)) (start));;BFS
          ((= algorithm 2) (dfs-io-aux board)(results-write-file (insert-record-file)) (start));;DFS
          ((= algorithm 3) (a-star-io-aux board)(results-write-file (insert-record-file)) (start));;A*
          ((= algorithm 4) (ida-star-io-aux board)(results-write-file (insert-record-file)) (start));;IDA*
          (t (choose-algorithm board))
          )))