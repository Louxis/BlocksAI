(defun switch-player (player)
  (cond ((= player *player1*) *player2*)
        (t *player1*)))
;nega
(defun result-negamax (node p alpha beta player solution expand operators cost)
  (let ((color (if (= player *player1*) 1 -1)))
  (calculate-made-play (second (clear-negamax node p alpha beta color solution expand operators cost player)) node player)))

(defun clear-negamax (node p alpha beta color solution expand operators cost player)
  (cond ((or (= p 0) (funcall solution node player)) (list (* color (funcall cost node)))) ;list so we can car
        (t (let ((child-nodes (sort (funcall expand node (operators) player) #'< :key cost)))
             (loop-child child-nodes p alpha beta color solution expand operators cost player)))))

(defun loop-child (nodes p alpha beta color solution expand operators cost player &optional (bestValue most-negative-fixnum) (next-move nil))
  (cond ((null nodes) (list bestValue next-move))
        (t
         (let* ((v (* -1 (car (clear-negamax (car nodes) (1- p) (* -1 beta) (* -1 alpha) (* -1 color) solution expand operators cost (switch-player player)))))
                (current-alpha (max alpha v))
                (keep-value (max bestValue v)))
           (cond ((>= current-alpha beta) (loop-child nil p current-alpha beta color solution expand operators cost player keep-value)) ;cut
                 (t (if (> v bestValue) 
                        (loop-child (cdr nodes) p current-alpha beta color solution expand operators cost player keep-value (car nodes))                      
                      (loop-child (cdr nodes) p current-alpha beta color solution expand operators cost player keep-value next-move))))))))
;nega

(defun move-parser (node play player)
  "Plays must respect the format (move (x y))"  
  (let ((move (first play))
        (x (first (second play)))
        (y (second (second play))))
    ;(funcall move x y node player)
    (cond ((eq move 'SQUARE-1X1) (node-create (square-1x1 x y node player) node 0 0 0 0)) 
          ((eq move 'SQUARE-2X2) (node-create (square-2x2 x y node player) node 0 0 0 0)) 
          ((eq move 'CROSS) (node-create (cross x y node player) node 0 0 0 0)) 
)))

(defun move-reader (&optional (node nil))
  (labels ((validate-input (input node)
           (cond ((null node) input)                 
                 ((member (second input) (possible-block-positions (node-board (node-state node)) (first input) 1) :test #'equal) input)
                 (t (move-reader node)))))
  (format t "~%Write your move in the (MOVE (X Y)) format~%")
  (let ((move (read)))
    (if (not (= (length move) 2)) (move-reader node)
      (let ((x (first (second move)))
            (y (second (second move))))
        (cond ((not (= (length move) 2)) (move-reader node))
              ((not (= (length (second move)) 2)) (move-reader node))
              ((eq (car move) 'q1) (validate-input (list 'SQUARE-1X1 (list x y)) node))
              ((eq (car move) 'q4) (validate-input (list 'SQUARE-2x2 (list x y)) node))
              ((eq (car move) 'x) 
               (validate-input (list 'CROSS (list (1- x) (1- y))) node)) ;fix hotspot
              (t (move-reader node))))))))

;(time-machine (test-node-empty) 1 'solution-nodep 'node-expand (operators) 'cost-function 5)
(defun time-machine (node player solution expand operators cost p)
  (let ((start (get-universal-time)))
    (progn (machine-play-basic node player solution expand operators cost p 100) (- (get-universal-time) start))))

(defun machine-play-basic (node player solution expand operators cost p &optional (limit most-positive-fixnum) (end-flag nil))
  (cond ((= limit 0) node)
        (t 
         (cond ((and end-flag (funcall solution node player)) (format t "Winner is: ~d~%" (assert-winner node)) (node-print node))
               ((funcall solution node player) (machine-play-basic node (switch-player player) solution expand operators cost p (1- limit) t))
               (t
                (machine-play-basic (move-parser node
                                                 (result-negamax node p most-negative-fixnum most-positive-fixnum player solution expand operators cost) player)
                                    (switch-player player) solution expand operators cost p (1- limit)))))))

(defun player-machine (node player solution expand operators cost p &optional (limit most-positive-fixnum) (end-flag nil))
  (cond ((and end-flag (funcall solution node player)) (format t "Winner is: ~d~%" (assert-winner node)) (node-print node))
        ((funcall solution node player) (player-machine node
                                                        (switch-player player) solution expand operators cost p (1- limit) t))
        ((= player *player1*) (node-print node) (player-machine (move-parser node (move-reader node) player)
                                    (switch-player player) solution expand operators cost p (1- limit)))
        (t (format t "~%~%Machine is thinking...~%~%")           
           (player-machine (move-parser node (result-negamax node p most-negative-fixnum most-positive-fixnum player solution expand operators cost) player)
                           (switch-player player) solution expand operators cost p (1- limit)))))

(defun cost-function (node)
  "Each player 1 (max) occupied cell gives +1, and each player 2 (min) occupied cell gives -1"
  (let ((state (node-state node)))
    (apply '+ (apply #'append 
                     (mapcar #'(lambda(line) 
                                 (mapcar #'(lambda(position) 
                                             (cond ((= position *player1*) 1) 
                                                   ((= position *player2*) -1) 
                                                   (t 0))) line)) (node-board state))))))
;
(defun estimate-time ()
  (let ((start (get-universal-time)))
  (labels ((fibonacci(n)
             (cond
              ((eq n 1) 0)
              ((eq n 2) 1)
              ((+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))
    (progn (fibonacci 43) (- (get-universal-time) start)))))

(defun time-depth (time)
  (print "Computing performance..")
  (let ((base-time (estimate-time)))    
    (cond ((<= base-time 5) (if (= time 10) 5 4))
          (t (if (>= time 8) 3 2)))))