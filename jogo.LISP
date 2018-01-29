;;O JOGO
;;read the other files

(defpackage :p150221044-150221059)


;(in-package :p150221044-150221059)

;;Place load files here
;(load)
;(load)

;Base node/board
(defun empty-board (&optional (board-size 14))
  "Retorna um tabuleiro 14x14 (default) com as casas vazias"
	(make-list board-size :initial-element (make-list board-size :initial-element '0)))

(defun node-empty (&optional (size 14))
  (list (list (empty-board size) '((10 10 15) (10 10 15))) nil 0))

;;Read file function. Reads the file in the given filepath
(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read stream nil)
          while line
          collect line)))

(defun ask-input (message &optional (min -100) (max 100))
  (format t message)
  (let ((input-user (read)))
    (cond ((or (< input-user min) (> input-user max)) (ask-input message min max))
          (t input-user))))

(defun ask-pathname (message)
  (format t message)
  (read))

(defun start ()
  (let ((selector (initialize)))
    (cond ((= selector 1) (start-player-game));player
          ((= selector 2) (start-machine-game));machine
          ((= selector 0) (format t "Thanks for playing!~%")))
    (if (not (= selector 0)) (start))))

(defun initialize()
  (format t "~%Welcome to Blokus Duo!~%")
  ;(if (eq (if (not (fboundp 'board-print)) (read-compile 'puzzle)) 'exit) 'exit
  ;  (if (eq (if (not (fboundp 'time-negamax)) (read-compile 'algoritmo)) 'exit) 'exit))
  (format t "Pick a game mode:~% 1. Player vs. Machine~% 2. Machine vs. Machine~% 0. Exit~%")
  (read))

(defun start-player-game ()
  (let* ((starter (ask-input "Who starts first? (1/2): " 1 2))
         (time (ask-input "How much time can the computer think? (1-10 seconds): " 1 10))
         (log-location (ask-pathname "Where do you want the actions to be logged? Include a filename or nil if a log is not desired!~%")))
    (if (not (null log-location)) (start-log log-location))
    (player-machine (node-empty) starter 'solution-nodep 'node-expand (operators) 'cost-function time log-location)))


(defun start-machine-game ()
  (let* ((time (ask-input "How much time can the computer think? (1-10): " 1 10))
         (log-location (ask-pathname "Where do you want the actions to be logged? Include a filename or nil if a log is not desired!~%")))
    (if (not (null log-location)) (start-log log-location))
    (machine-play (node-empty) 1 'solution-nodep 'node-expand (operators) 'cost-function time log-location)))

(defun start-log (pathname)
  (with-open-file (out pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "--Log started--")))

(defun write-log (pathname result time player)
  (let ((move (car result))
        (explored (second result))
        (cuts (third result))
        (depth (fourth result)))
    (with-open-file (out pathname :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~%Player~d played:~s Explored: ~d Alpha Cuts: ~d Took: ~d ms Depth: ~d~%" player (move-writer move) explored cuts time depth))))


(defun move-writer (play)
  "Translates an internal move to an external move"
  (let ((move (first play))
        (x (first (second play)))
        (y (second (second play))))
    (cond ((eq move 'SQUARE-1X1) (list 'q1 (list x y)))
          ((eq move 'SQUARE-2x2) (list 'q4 (list x y)))
          ((eq move 'CROSS) (list 'x (list (1+ x) (1+ y))))))) ;fix hotspot


(defun read-compile (filename)
  (format t "~%Please insert the filepath to the file with ~d logic:~%" filename)
   (let ((filepath (read)))
   (cond ((eq filepath 'exit) 'exit)
         ((or (pathnamep filepath) (stringp filepath)) (compile-file filepath :load t))
         (t (format t "~%Invalid file path!~%") (read-compile filename)))))
