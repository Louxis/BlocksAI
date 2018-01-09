;;;
(defvar *player1* 1)
(defvar *player2* 2)

(defun switch-player (player)
  (cond ((= player *player1*) *player2*)
        (t *player1*)))

(defun negamax (node p alpha beta player solution expand cost)
  (cond ((or (= p 0) (funcall solution node)) (* player (node-h node)))
        (t 
         (let* ((child-nodes (funcall expand node)) (sorted-child (qsort child-nodes #'< cost))) 
           
           )
)))

(defun negamax-base (node p player solution expand operators cost &optional (kept-value -1000))
  (cond ((or (= p 0) (funcall solution node)) (* player (funcall cost node)))
        (t 
         (let* ((child-nodes (funcall expand node (operators) 'bfs)) (sorted-child (qsort child-nodes #'< cost)))
           (negamax-base-aux child-nodes p kept-value player solution expand operators cost))         
)))

(defun negamax-base-aux (nodes p kept-value player solution expand operators cost)
  (cond ((null nodes) kept-value)
        (t 
         (let* ((score (* -1 (negamax-base (car nodes) (1- p) (switch-player player) solution expand operators cost))) (current-value (max kept-value score)))
           (negamax-base-aux 
            (cdr nodes) p 
            current-value player solution expand operators cost)))))
  
(defun cost-function (node)
  "Each player 1 (max) occupied cell gives +1, and each player 2 (min) occupied cell gives -1"
  (let ((state (node-state node)))
    (apply '+ (apply #'append 
                     (mapcar #'(lambda(line) 
                                 (mapcar #'(lambda(position) 
                                             (cond ((= position *player1*) 1) 
                                                   ((= position *player2*) -1) 
                                                   (t 0))) line)) (node-board state))))))


        