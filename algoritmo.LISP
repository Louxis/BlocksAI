;;;
(defvar *player1* 1)
(defvar *player2* -1)

(defun switch-player (player)
  (cond ((= player *player1*) *player2*)
        (t *player1*)))

(defun negamax (node p alpha beta player solution expand cost)
  (cond ((or (= p 0) (funcall solution node)) (* player (node-h node)))
        (t 
         (let* ((child-nodes (funcall expand node)) (sorted-child (qsort child-nodes #'< cost))) 
           
           )
)))

(defun negamax-base (node p player solution expand operators cost &optional (kept-value most-negative-fixnum ))
  (cond ((or (= p 0) (funcall solution node player)) (* player (funcall cost node)))
        (t 
         (let* ((child-nodes (funcall expand node (operators) player)))
           (negamax-base-aux child-nodes p kept-value player solution expand operators cost))         
)))

(defun negamax-base-aux (nodes p kept-value player solution expand operators cost)
  (cond ((null nodes) kept-value)
        (t 
         (let* ((score (* -1 (negamax-base (car nodes) (1- p) (switch-player player) solution expand operators cost))) (current-value (max kept-value score)))
           (negamax-base-aux 
            (cdr nodes) p 
            current-value player solution expand operators cost)))))

(defun negamax-root (root p player solution expand operators cost)
  (labels ((loop-negamax-aux (nodes p player solution expand operators cost &optional (best-value most-negative-fixnum) (best-node nil))
             (let* ((current-node-value (negamax-base (car nodes) p player solution expand operators cost)))
               (cond ((= (length nodes) 1) ;this is the last node
                      (if (= (max current-node-value best-value) current-node-value) ;check if we found a better node
                          (list current-node-value (car nodes)) ; we found a better node
                        (list best-value best-node))) ; we return the best node
                     (t (cond ((= (max best-value current-node-value) current-node-value) ;check if we found a better node
                               ;we found a better one:
                               (loop-negamax-aux (cdr nodes) p player solution expand operators cost current-node-value (car nodes)))
                              ;we use the best node found so far
                              (t (loop-negamax-aux (cdr nodes) p player solution expand operators cost best-value best-node))))))))                               
    (cond ((or (= p 0) (funcall solution root player)) (* player (funcall cost root))) ; if for some reason we get a p = 0 or a solution node this could be ommited maybe?
          (t 
           (let ((child-nodes (funcall expand root (operators) player)))
             (loop-negamax-aux child-nodes p player solution expand operators cost))))))     
  
(defun cost-function (node)
  "Each player 1 (max) occupied cell gives +1, and each player 2 (min) occupied cell gives -1"
  (let ((state (node-state node)))
    (apply '+ (apply #'append 
                     (mapcar #'(lambda(line) 
                                 (mapcar #'(lambda(position) 
                                             (cond ((= position *player1*) 1) 
                                                   ((= position *player2*) -1) 
                                                   (t 0))) line)) (node-board state))))))


        