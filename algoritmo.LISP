(defun switch-player (player)
  (cond ((= player *player1*) *player2*)
        (t *player1*)))

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
           (cond ((>= current-alpha beta) (loop-child nil p current-alpha beta player solution expand operators cost keep-value)) ;cut
                 (t (if (> v bestValue) 
                        (loop-child (cdr nodes) p current-alpha beta color solution expand operators cost player keep-value (car nodes))                      
                      (loop-child (cdr nodes) p current-alpha beta color solution expand operators cost player keep-value next-move))))))))

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

(defun machine-play-basic (node player solution expand operators cost &optional (limit most-positive-fixnum) (end-flag nil))
  (cond ((= limit 0) node)
        (t 
         (cond ((and end-flag (funcall solution node player)) (print limit) (node-print node))
               ((funcall solution node player) (machine-play-basic node (switch-player player) solution expand operators cost (1- limit) t))
               (t
                (machine-play-basic (move-parser node
                                                 (result-negamax node 1 most-negative-fixnum most-positive-fixnum player solution expand operators cost) player)
                                    (switch-player player) solution expand operators cost (1- limit)))))))

 



(defun negamax-alphabeta-solo (node p alpha beta player solution expand operators cost &optional (kept-value most-negative-fixnum) (kept-node nil))
  (labels ((negamax-aux (nodes p alpha beta player solution expand operators cost kept-value kept-node)
             (cond ((null nodes) (list kept-value alpha beta kept-node))
                   (t 
                    (let* ((revert-score (negamax-alphabeta-solo (car nodes) (1- p) (* -1 beta) (* -1 alpha) (switch-player player) solution expand operators cost))
                           (score (if (listp revert-score) (* -1 (car revert-score)) (* -1 revert-score)))
                           (current-value (max kept-value score))
                           (current-alpha (max alpha score)))
                      (if (>= current-alpha beta)
                          ;cut the branch
                          (negamax-aux nil p current-alpha beta player solution expand operators cost current-value kept-node)
                        ;else don't cut it
                        (if (> current-value score)
                            (negamax-aux (cdr nodes) p current-alpha beta player solution expand operators cost current-value kept-node)
                          (negamax-aux (cdr nodes) p current-alpha beta player solution expand operators cost current-value (car nodes)))))))))
  (cond ((or (= p 0) (funcall solution node player)) (* player (funcall cost node)))
        (t (let ((child-nodes (sort (funcall expand node (operators) player) #'< :key cost)))
             (list (car (negamax-aux child-nodes p alpha beta player solution expand operators cost kept-value kept-node))
                   (calculate-made-play (fourth (negamax-aux child-nodes p alpha beta player solution expand operators cost kept-value kept-node)) node)))))))


    


(defun negamax-alphabeta (node p alpha beta player solution expand operators cost &optional (kept-value most-negative-fixnum) (kept-node nil))
  (labels ((negamax-aux (nodes p alpha beta player solution expand operators cost kept-value kept-node)
             (cond ((null nodes) (list kept-value alpha beta kept-node))
                   (t
                    (let* ((revert-score (negamax-alphabeta (car nodes) (1- p) (* -1 beta) (* -1 alpha) (switch-player player) solution expand operators cost))
                           (score (if (listp revert-score) (* -1 (car revert-score)) (* -1 revert-score)))
                           (current-value (max kept-value score))
                           (current-alpha (max alpha score)))
                      (if (>= alpha beta)
                          ;cut the branch
                          (negamax-aux nil p current-alpha beta player solution expand operators cost current-value kept-node)
                        ;else don't cut it
                        (if (> current-value score)
                            (negamax-aux (cdr nodes) p current-alpha beta player solution expand operators cost current-value kept-node)
                          (negamax-aux (cdr nodes) p current-alpha beta player solution expand operators cost current-value (car nodes)))))))))

  (cond ((or (= p 0) (funcall solution node player)) (* player (funcall cost node)))
        (t (let ((child-nodes (sort (funcall expand node (operators) player) #'< :key cost)))
             (negamax-aux child-nodes p alpha beta player solution expand operators cost kept-value kept-node))))))
             
;test (negamax-alphabeta-root (test-node-empty) 1 most-negative-fixnum most-positive-fixnum 1 'solution-nodep 'node-expand (operators) 'cost-function)
(defun negamax-alphabeta-root (root p alpha beta player solution expand operators cost)
  (labels ((loop-negamax-aux (nodes p alpha beta player solution expand operators cost &optional (best-value most-negative-fixnum) (best-node nil))             
               (cond ((null nodes) best-node)
                     (t 
                      (let* ((nega-result (negamax-alphabeta (car nodes) p alpha beta player solution expand operators cost))
                             (current-node-value (car nega-result))
                             (current-alpha (second nega-result))
                             (current-beta (third nega-result)))
                        (if (>= current-alpha current-beta) 
                            (progn (print "cut")(loop-negamax-aux nil p player solution expand operators cost current-node-value (car nodes)))
                          (cond ((= (max best-value current-node-value) current-node-value) ;check if we found a better node
                               ;we found a better one:
                                 (loop-negamax-aux (cdr nodes) p current-alpha current-beta player solution expand operators cost current-node-value (car nodes)))
                              ;we use the best node found so far
                                (t (loop-negamax-aux (cdr nodes) p current-alpha current-beta player solution expand operators cost best-value best-node)))))))))
                               
    (cond ((or (= p 0) (funcall solution root player)) (* player (funcall cost root))) ; if for some reason we get a p = 0 or a solution node this could be ommited maybe?
          (t 
           (let ((child-nodes (sort (funcall expand root (operators) player) #'< :key cost)))
              (calculate-made-play (loop-negamax-aux child-nodes p alpha beta player solution expand operators cost) root player))))))  

;test (negamax-root (test-node-empty) 1 1 'solution-nodep 'node-expand (operators) 'cost-function)
(defun negamax-root (root p player solution expand operators cost)
  (labels ((loop-negamax-aux (nodes p player solution expand operators cost &optional (best-value most-negative-fixnum) (best-node nil))             
               (cond ((null nodes) best-node) ;this is the last node
                     (t 
                      (let* ((current-node-value (negamax-base (car nodes) p player solution expand operators cost)))
                        (cond ((= (max best-value current-node-value) current-node-value) ;check if we found a better node
                               ;we found a better one:
                               (loop-negamax-aux (cdr nodes) p player solution expand operators cost current-node-value (car nodes)))
                              ;we use the best node found so far
                              (t (loop-negamax-aux (cdr nodes) p player solution expand operators cost best-value best-node))))))))                               
    (cond ((or (= p 0) (funcall solution root player)) (* player (funcall cost root))) ; if for some reason we get a p = 0 or a solution node this could be ommited maybe?
          (t 
           (let ((child-nodes (funcall expand root (operators) player)))
             (list (calculate-made-play (loop-negamax-aux child-nodes p player solution expand operators cost) root player) (loop-negamax-aux child-nodes p player solution expand operators cost)))))))     
  
(defun cost-function (node)
  "Each player 1 (max) occupied cell gives +1, and each player 2 (min) occupied cell gives -1"
  (let ((state (node-state node)))
    (apply '+ (apply #'append 
                     (mapcar #'(lambda(line) 
                                 (mapcar #'(lambda(position) 
                                             (cond ((= position *player1*) 1) 
                                                   ((= position *player2*) -1) 
                                                   (t 0))) line)) (node-board state))))))


        