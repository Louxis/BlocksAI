(defun switch-player (player)
  (cond ((= player *player1*) *player2*)
        (t *player1*)))
;;;Lexic to registar negamax statistics and time calculations
(let ((time-left most-positive-fixnum)
      (start-time 0)
      (time-check 0)
      (time-over nil))

(defun result-negamax (node p alpha beta player solution expand operators cost)
  "Uses negamax to calculate the optimal play"
  (let* ((color (if (= player *player1*) 1 -1))
         (result (clear-negamax node p alpha beta color solution expand operators cost player))
         (play (calculate-made-play (second result) node player))
         (explored (third result))
         (cuts (fourth result)))        
    (list play explored cuts p)))

(defun clear-negamax (node p alpha beta color solution expand operators cost player &optional (explored-nodes 0) (cuts 0))               
  (cond ((or (= p 0) (funcall solution node player)) (list (* color (funcall cost node)))) ;list so we can car
        (t (let ((child-nodes (sort (funcall expand node (operators) player) #'< :key cost)))
             (loop-child child-nodes p alpha beta color solution expand operators cost player most-negative-fixnum node explored-nodes cuts)))))



(defun loop-child (nodes p alpha beta color solution expand operators cost player &optional (bestValue most-negative-fixnum) (next-move nil) (explored-nodes 0) (cuts 0))
  (setq time-left (- time-left (used-time)))
  (cond ((null nodes) (list bestValue next-move explored-nodes cuts))
        (t ;1 nó analisado
         (let* ((result (clear-negamax (car nodes) (1- p) (* -1 beta) (* -1 alpha) (* -1 color) solution expand operators cost (switch-player player) explored-nodes cuts))
                (v (* -1 (car result)))
                (current-cuts (or (fourth result) 0))
                (current-alpha (max alpha v))
                (keep-value (max bestValue v))
                (total-explored (+ explored-nodes 1)))
           (cond ((< time-left 50) (setq time-over t) ;invalidate result
                  (loop-child nil p current-alpha beta color solution expand operators cost player keep-value next-move total-explored current-cuts)) ;break
                  ((>= current-alpha beta); 1 corte feito
                   (loop-child nil p current-alpha beta color solution expand operators cost player keep-value next-move total-explored (1+ current-cuts))) ;cut
                 (t (if (> v bestValue) 
                        (loop-child (cdr nodes) p current-alpha beta color solution expand operators cost player keep-value (car nodes) total-explored current-cuts)
                      (loop-child (cdr nodes) p current-alpha beta color solution expand operators cost player keep-value next-move total-explored current-cuts))))))))

;;;TIME
(defun used-time ()
  (let ((curr-time time-check))
    (setq time-check (get-internal-real-time))
    (- (get-internal-real-time) curr-time)))

(defun time-negamax (node alpha beta player solution expand operators cost time &optional (p 1) (keep nil))
  (setq time-left (* time 1000)) ;convert seconds to milliseconds  
  (setq start-time (get-internal-real-time))
  (setq time-check (get-internal-real-time))
  (setq time-over nil)
  (time-aux node p alpha beta player solution expand operators cost keep))

(defun time-aux (node p alpha beta player solution expand operators cost keep)
  (setq time-left (- time-left (used-time)))
  (if (< time-left 50) (end-search keep) ;don't try to explore any other solution
    (let ((result (result-negamax node p alpha beta player solution expand operators cost)))
      (if time-over (end-search keep) ;time was over so result gets invalidated
        (time-aux node (1+ p) alpha beta player solution expand operators cost result)))))
;;;END TIME
(defun end-search (result)
  result)
)
;nega

;;Play Logic
;(time-machine (test-node-empty) 2 'solution-nodep 'node-expand (operators) 'cost-function 1)
(defun time-machine (node player solution expand operators cost time &optional path)
  (let ((start (get-universal-time)))
    (progn (player-machine node player solution expand operators cost time path 100) (- (get-universal-time) start))))

(defun machine-play (node player solution expand operators cost time &optional (path nil) (limit most-positive-fixnum) (end-flag nil))
  (cond ((= limit 0) node)
        (t 
         (cond ((and end-flag (funcall solution node player)) (format t "Winner is: ~d~%" (assert-winner node)) (node-print node))
               ((funcall solution node player) (machine-play node (switch-player player) solution expand operators cost time path (1- limit) t))
               (t
                (let* ((start (get-internal-real-time))
                       (result (time-negamax node most-negative-fixnum most-positive-fixnum player solution expand operators cost time))
                       (move (car result))
                       (explored (second result))
                       (cuts (third result))
                       (depth (fourth result)))
                  (format t "~%Player~d played:~s Explored: ~d Alpha Cuts: ~d Took: ~d ms Depth: ~d~%" player (move-writer move) explored cuts (- (get-internal-real-time) start) depth)
                  (if (not (null path)) (write-log path result (- (get-internal-real-time) start) player)) ;logging is optional
                  (machine-play (move-parser node move player) (switch-player player) solution expand operators cost time path (1- limit))))))))
                                

(defun player-machine (node player solution expand operators cost time &optional (path nil) (limit most-positive-fixnum) (end-flag nil))
  (cond ((and end-flag (funcall solution node player)) (format t "Winner is: ~d~%" (assert-winner node)) (node-print node))
        ((funcall solution node player) (player-machine node
                                                        (switch-player player) solution expand operators cost time path (1- limit) t))
        ((= player *player1*) (node-print node) (player-machine (move-parser node (move-reader node) player)
                                                                (switch-player player) solution expand operators cost time path (1- limit)))
        (t (format t "~%~%Machine is thinking...~%~%")     
           (let* ((start (get-internal-real-time))
                  (result (time-negamax node most-negative-fixnum most-positive-fixnum player solution expand operators cost time))
                  (move (car result))
                  (explored (second result))
                  (cuts (third result))
                  (depth (fourth result)))
             (format t "~%Player~d played:~s Explored: ~d Alpha Cuts: ~d Took: ~d ms Depth: ~d~%" player (move-writer move) explored cuts (- (get-internal-real-time) start) depth)
             (if (not (null path)) (write-log path result (- (get-internal-real-time) start) player)) ;logging is optional
             (player-machine (move-parser node move player) (switch-player player) solution expand operators cost time path (1- limit))))))
                        

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
;;Play Logic

;;Nega with memoization
(let ((tab (make-hash-table)))
(defun memo-nega (node p alpha beta color solution expand operators cost player)
  (let* ((resultNega (let ((ttEntry (gethash node tab)))
                       (cond ((and ttEntry (>= (third ttEntry) p))
                              (let ((ttFlag (first ttEntry))
                                    (ttValue (second ttEntry)))
                                (cond ((= ttFlag 'EXACT) ttValue)
                                      ((= ttFlag 'LOWERBOUND) (if (>= (max alpha ttValue) beta) ttValue
                                                                (clear-negamax node p (max alpha ttValue) beta color solution expand operators cost player)));change alpha
                                      ((= ttFlag 'UPPERBOUND) (if (>= alpha (min beta ttValue)) ttValue
                                                                (clear-negamax node p alpha (min beta ttValue) color solution expand operators cost player))))))
                             (t (clear-negamax node p alpha beta color solution expand operators cost player)))));change beta
         (bestValue (first resultNega)))    
    (cond ((<= bestValue alpha) (setf (gethash node tab) (list 'UPPERBOUND resultNega p))) ;flag =upper
          ((>= bestValue beta) (setf (gethash node tab) (list 'LOWERBOUND resultNega p))) ;flag = lower
          (t (setf (gethash node tab) (list 'EXACT bestValue p)))) ;flag = exact
          resultNega)
))
;;Nega with memoization

;;Aux move
(defun move-parser (node play player)
  "Plays must respect the format (move (x y))"  
  (let ((move (first play))
        (x (first (second play)))
        (y (second (second play))))
    ;(funcall move x y node player)
    (cond ((eq move 'SQUARE-1X1) (node-create (square-1x1 x y node player) node 0)) 
          ((eq move 'SQUARE-2X2) (node-create (square-2x2 x y node player) node 0)) 
          ((eq move 'CROSS) (node-create (cross x y node player) node 0)) 
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

(defun move-writer (play)
  "Translates an internal move to an external move"
  (let ((move (first play))
        (x (first (second play)))
        (y (second (second play))))
    (cond ((eq move 'SQUARE-1X1) (list 'q1 (list x y)))
          ((eq move 'SQUARE-2x2) (list 'q4 (list x y)))
          ((eq move 'CROSS) (list 'x (list (1+ x) (1+ y))))))) ;fix hotspot
      
;;Aux move

;;Costs
(defun cost-function (node)
  "Each player 1 (max) occupied cell gives +1, and each player 2 (min) occupied cell gives -1"
  (let ((state (node-state node)))
    (apply '+ (apply #'append 
                     (mapcar #'(lambda(line) 
                                 (mapcar #'(lambda(position) 
                                             (cond ((= position *player1*) 1) 
                                                   ((= position *player2*) -1) 
                                                   (t 0))) line)) (node-board state))))))
;;Costs