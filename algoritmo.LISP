(defun switch-player (player)
  (cond ((= player *player1*) *player2*)
        (t *player1*)))

(defun negamax-base (node p player solution expand operators cost &optional (kept-value most-negative-fixnum))
  (labels ((negamax-base-aux (nodes p kept-value player solution expand operators cost)
             (cond ((null nodes) kept-value)
                   (t 
                    (let* ((score (* -1 (negamax-base (car nodes) (1- p) (switch-player player) solution expand operators cost))) (current-value (max kept-value score)))
                      (negamax-base-aux 
                       (cdr nodes) p 
                       current-value player solution expand operators cost))))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (cond ((or (= p 0) (funcall solution node player)) (* player (funcall cost node)))
          (t 
           (let* ((child-nodes (funcall expand node (operators) player)))
             (negamax-base-aux child-nodes p kept-value player solution expand operators cost))         
           ))))

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
             (calculate-made-play (fourth (negamax-aux child-nodes p alpha beta player solution expand operators cost kept-value kept-node)) node))))))
             
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
             (list 
              (calculate-made-play (loop-negamax-aux child-nodes p alpha beta player solution expand operators cost) root player) 
              (loop-negamax-aux child-nodes p alpha beta player solution expand operators cost)))))))  

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


        