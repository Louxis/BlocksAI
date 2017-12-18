;(compile-file "C:/Users/jcper/Desktop/puzzle" :load t) 
;(compile-file "C:/Users/jcper/Desktop/procura" :load t) 

"C:/Users/jcper/Desktop/compiletest.txt"
"C:/Users/jcper/Desktop/problemas.dat"
"C:/Users/jcper/Desktop/bfs1.txt"
"C:/Users/jcper/Desktop/dfs2.txt"

;;Read file function. Reads the file in the given filepath
(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read stream nil)
          while line
          collect line)))

;;Start function. To start the simulation, this function needs to be called in the listener
(defun start(&optional boards-value)
  "Start function. To start the simulation, this function needs to be called in the listener"
  (let ((boards (if (null boards-value) (initialize) boards-value)))
    (if (equal boards 'exit) (format t "~%Ended Session")
      (labels ((read_board () 
                 (let ((board (choose-board)))
                   (cond 
                    ((not (numberp board)) nil)
                    ((or(< board 0) (> board (length boards))) (read_board))
                    (t (choose-algorithm (nth board boards) boards))))))(read_board)))))

;;Function that introduces simulation to user, greeting him and asking him for filepath
(defun initialize ()
  "Function that introduces simulation to user, greeting him and asking him for filepath"
  (format t "~%Welcome to BlocksAI!~%Please insert the filepath you want to simulate or type exit to leave.~%") 
  (let ((filepath (read)))
    (cond ((eq filepath 'exit) 'exit)
          ((or (pathnamep filepath) (stringp filepath)) 
           (read-file filepath) ;test file existence
           (if (eq (if (not (fboundp 'node-create)) (read-compile 'puzzle)) 'exit) 'exit
             (if (eq (if (not (fboundp 'bfs-it)) (read-compile 'procura)) 'exit) 'exit (read-file filepath))))
          (t (format t "~%Invalid file path!~%") (initialize)))))


(defun read-compile (filename)
  (format t "~%Please insert the filepath to the file with ~d logic:~%" filename)
   (let ((filepath (read)))
   (cond ((eq filepath 'exit) 'exit)
         ((or (pathnamep filepath) (stringp filepath)) (compile-file filepath :load t))
         (t (format t "~%Invalid file path!~%") (read-compile filename)))))

;;function that asks user which board from file he wants to test
(defun choose-board()
  "Function that asks user which board from file he wants to test"
  (format t "~%Type restart to go back to begining.~%Type exit to end program.~%Choose the board you want to test:~%")
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

(defun state-calculator (board)
  (list 
   (- 10 (block-count board 'square-1x1)) 
   (- 10 (block-count board 'square-2x2)) 
   (- 15 (block-count board 'cross))))

;;Auxiliar function tho execute bfs algorithm
(defun bfs-io-aux(board)
 (bfs-it (node-create (list board (state-calculator board)) nil 0 0 0 0 ) 
                     'solution-nodep 'node-expand (operators)))

;;Auxiliar function to execute dfs algorithm asking user to insert the maximum depth
(defun dfs-io-aux (board)
  (format t "~%Please insert maximum depth ~%")
  (let ((max-depth (read)))
    (cond ((numberp max-depth) 
           (dfs-it (node-create(list board (state-calculator board)) nil 0 0 0 0 ) 
                     'solution-nodep 'node-expand (operators) max-depth))
        (t (dfs-io-aux board)))))

;;Auxiliar function to execute a-star with certain heuristic
(defun a-star-io-aux (board)
  (format t "~%Please insert wanted heuristic ~%1 - Default ~%2 - Student Custom~%")
  (let ((heuristic (read)))
    (cond ((and (numberp heuristic) (= heuristic 1)) 
           (a-star (node-create (list board (state-calculator board)) nil 0 0 0 0 )  
                   'solution-nodep 'node-expand-a (operators) 'heuristic-squares 'node-f))
          ((and (numberp heuristic) (= heuristic 2)) 
           (a-star (node-create (list board (state-calculator board)) nil 0 0 0 0 )
                   'solution-nodep 'node-expand-a (operators) 'heuristic-custom-complex 'node-f))
        (t (a-star-io-aux board)))))

;;Auxiliar function to execute ida-star with certain heuristic
(defun ida-star-io-aux (board)
  (format t "~%Please insert wanted heuristic ~%1 - Default ~%2 - Student Custom~%")
  (let ((heuristic (read)))
    (cond ((and (numberp heuristic) (= heuristic 1)) 
           (ida-star (node-create (list board (state-calculator board)) nil 0 0 0 0 )
                     'solution-nodep 'node-expand-a (operators) 'heuristic-squares 'node-f))
          ((and (numberp heuristic) (= heuristic 2)) 
           (ida-star (node-create (list board (state-calculator board)) nil 0 0 0 0 )
                     'solution-nodep 'node-expand-a (operators) 'heuristic-custom-complex 'node-f))
        (t (ida-star-io-aux board)))))

(defun insert-record-file ()
  "Function that asks user to type the file path in which the program will record the results"
    (format t "~%Please insert the filepath where you want to save the results~%")
     (read))

(defun choose-algorithm (board boards)
  "Function that asks user to choose the algorithm to work with"
  (let ((algorithm (algorithm-number)))
    (cond ((eq algorithm 'exit) (format t "~% Ended Session"))
          ((= algorithm 1) (bfs-io-aux board)); BFS
          ((= algorithm 2) (dfs-io-aux board)); DFS
          ((= algorithm 3) (a-star-io-aux board)); A*
          ((= algorithm 4) (ida-star-io-aux board)); IDA*
          (t (choose-algorithm board boards))
          ))
  (results-write-file (insert-record-file))
  (start boards))