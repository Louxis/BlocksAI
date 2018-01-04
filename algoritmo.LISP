;;;
(defvar *player1* 1)
(defvar *player2* 2)

(defun invert-player (value)
  (cond ((= value *player1*) *player2*) 
        ((= value *player2*) *player1*) 
        (t value)))

(defun invert-board (board)
  (mapcar #'(lambda(line) (mapcar #'(lambda (cell) (invert-player cell)) line)) board))

(defun negamax (node p alpha beta player solution expand cost)
  (cond ((or (= p 0) (funcall solution node)) (* player (node-h node)))
        (t 
         (let* ((child-nodes (funcall expand node)) (sorted-child (qsort child-nodes #'< cost))) 
           
           )
)))

(defun negamax-base (node p color solution expand operators cost &optional (kept-value -1000))
  (cond ((or (= p 0) (funcall solution node)) (* color (funcall cost node)))
        (t 
         (let* ((child-nodes (funcall expand node (operators) 'bfs)) (sorted-child (qsort child-nodes #'< cost)))
           (negamax-base-aux child-nodes p kept-value color solution expand operators cost))         
)))

(defun negamax-base-aux (nodes p kept-value color solution expand operators cost)
  (cond ((null nodes) kept-value)
        (t 
         (let* ((score (* -1 (negamax-base (car nodes) (1- p) (* -1 color) solution expand operators cost))) (current-value (max kept-value score)))
           (negamax-base-aux 
            (cdr nodes) p 
            current-value color solution expand operators cost)))))
  

;;; based on: https://stackoverflow.com/questions/34032558/non-destructive-sort-in-lisp
(defun qsort (input predicate cost)
  (if input
    (let* ((pivot (first input))
           (rest (rest input))
           (lesser (remove-if-not #'(lambda (x)
                                      (funcall predicate (funcall cost x) (funcall cost pivot)))
                                  rest))
           (greater (remove-if-not #'(lambda (x)
                                       (not (funcall predicate (funcall cost x) (funcall cost pivot))))
                                   rest)))
      (append (qsort lesser predicate cost)
              (list pivot)
              (qsort greater predicate cost)))
    nil)) 


        