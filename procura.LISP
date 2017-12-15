;;;;Searching algorithms
;;;;Made by José Pereira and Lyudmyla Todoriko
;;;;The node structure is {(board) (pieces remaining) (parent node) (g) (optional h and f)

;;;Global variables
(defparameter *open* nil)
(defparameter *close* nil)

;;;
(defun open-bfs (open child)
  (append open child))

(defun open-dfs (open child)
  (append child open))


(defun equal-node (node1 node2)
  (equal (node-board (node-state node1)) (node-board (node-state node2))))

(defun node-existsp (node list)
  (if (member node list :test 'equal-node) t nil))

(defun filter-nodes (node-list open-list close-list)
  (cond ((null node-list) nil)
        ((or (node-existsp (car node-list) open-list) (node-existsp (car node-list) close-list)) (filter-nodes (cdr node-list) open-list close-list))
        (t (cons (car node-list) (filter-nodes (cdr node-list) open-list close-list)))))

(let ((results (list 0 -1 0 nil)))
;;;BF Search Iterative
;;;Test: (node-print (bfs-it (test-node) 'solution-nodep 'node-expand (operators)))
(defun bfs-it (node solution expand operators &optional (d nil))
  (setq *open* (list node))
  (setq *close* nil)
  (loop while (not (null *open*)) do
        (let* ((currentNode (car *open*)) (expandedNodes (filter-nodes (funcall expand currentNode operators 'bfs d) *open* *close*)))  
          (add-explored 1)
          (add-generate (length expandedNodes))
          ;Add currentNode to closed list
          (setq *close* (append *close* (list currentNode)))
          ;Remove current node from open
          (setq *open* (cdr *open*))
          ;Add expanded nodes to open
          (setq *open* (open-bfs *open* expandedNodes))
          ;Check if a node is a possible solution and return it
          (mapcar #'(lambda (nodeit) (if (funcall solution nodeit) (progn (return nodeit)))) expandedNodes))))

;;;DF Search
(defun dfs-it (node solution expand operators p)
   (setq *open* (list node))
   (setq *close* nil)
  (loop while (not (null *open*)) do
        (let* ((currentNode (car *open*)) (expandedNodes (filter-nodes (funcall expand currentNode operators 'dfs p) *open* *close*)))
          (add-explored 1)
          (add-generate (length expandedNodes))
          ;Add currentNode to closed list
          (setq *close* (append *close* (list currentNode)))
          ;Remove current node from open
          (setq *open* (cdr *open*))
          ;Add expanded nodes to open
          (setq *open* (open-dfs *open* expandedNodes))
          ;Check if a node is a possible solution and return it
          (mapcar #'(lambda (nodeit) (if (funcall solution nodeit) (return nodeit))) expandedNodes))))

;;;A* search iterative
;;;Test: (node-print (a-star (test-node-a) 'solution-nodep 'node-expand-a (operators) 'heuristic-squares))
(defun a-star (node solution expand operators heuristic)
  (start-performance)
  (setq *open* (list node))
  (setq *close* nil)
  (start-timer)
  (loop while (not (null *open*)) do
        (let* ((current-node (car *open*)) (expanded-nodes (filter-nodes (funcall expand current-node operators heuristic) *open* *close*)))            
          (add-explored 1)
          (add-generate (length expanded-nodes))
          (setq *close* (append *close* (list current-node)))
          (cond ((funcall solution current-node) (print-results current-node)(return current-node)))
          (setq *open* (ordered-insert-list (cdr *open*) expanded-nodes))
)))


;;;IDA* search iterative
;;;(node-print (ida-star (test-node-a) 'solution-nodep 'node-expand-a (operators) 'heuristic-squares 'node-f))
(defun ida-star (node solution expand operators heuristic cost &optional (bound (funcall cost node)))
  (start-performance)
  (setq *open* (list node))
  (setq *close* nil)
  (start-timer)
  (loop while (not (null *open*)) do
        (add-explored 1)
        (if (> (funcall cost (car *open*)) bound) (return (ida-star node solution expand operators heuristic cost (funcall cost (car *open*)))))
        (let* ((current-node (car *open*)) (expanded-nodes (filter-nodes (funcall expand current-node operators heuristic) *open* *close*)))  
          (add-generate (length expanded-nodes))
          (setq *close* (append *close* (list current-node)))
          (cond ((funcall solution current-node) (stop-performance current-node)(return current-node)))
          (setq *open* (ordered-insert-list (cdr *open*) expanded-nodes)))
))

;;;Perfomance calculations
(defun start-performance ()
  (setf results (list 0 0 0 nil))
  (start-timer))

(defun stop-performance (node)
  (stop-timer)
  (setf results (list (nth 0 results) (nth 1 results) (nth 2 results) node))
  (print-results))  

(defun get-generate () (nth 0 results))
(defun get-explored () (nth 1 results))
(defun get-timer () (nth 2 results))
(defun get-node () (nth 3 results))

(defun add-generate (value) (setf results (list (+ (nth 0 results) value) (nth 1 results) (nth 2 results) (nth 3 results))))

(defun add-explored (value) (setf results (list (nth 0 results) (+ (nth 1 results) value) (nth 2 results) (nth 3 results))))

(defun start-timer () 
  (setf results (list (nth 0 results) (nth 1 results) (get-internal-real-time) (nth 3 results))))

(defun stop-timer () 
  (setf results (list (nth 0 results) (nth 1 results) (- (get-internal-real-time)(nth 2 results)) (nth 3 results))))

(defun calculate-pen(node) 
  (/ (node-solution-size node) (nth 0 results)))

(defun print-results ()
  (format t 
"Tamanho da solução: ~d~%
Nós gerados: ~d~%
Nós explorados: ~d~%
Tempo de execução: ~dms~%
Penetrância: ~d~%~%
Ramificação média: ~s~%"
          (node-solution-size (nth 3 results))
          (get-generate)
          (get-explored)
          (get-timer)
          (calculate-pen (get-node))
          (Newton-Raphson #'base-ramification #'derivative-ramification 0.0 5.0)))

(defun calculate-average-ramification())

;;test (Newton-Raphson #'exp-2 #'exp 0.0 20.0)
(defun Newton-Raphson
       (f
        f-prime
        x-left
        x-right
        &key
        (accuracy 0.01)
        (maximum-number-of-iterations 100)
        (prevent-bracket-jumping-p t))
  "given
   [1] f (required)
       ==> a function with a single argument
   [2] f-prime (required)
       ==> another function with a single argument,
           this one being the first derivative of f
   [3] x-left (required)
       ==> left-hand bracket for the desired root;
           i.e., left-hand bracket <= desired root
   [4] x-right (required)
       ==> right-hand bracket for the desired root;
           i.e., desired root <= right-hand bracket"
  (assert (< x-left x-right))  
  (let ((x (* 0.5 (+ x-left x-right)))
        delta-x denom-for-accuracy-test)      
    (dotimes (j maximum-number-of-iterations                
                (if (not (cerror "returns solution so far"
                                 "exceeding maximum number of iterations"))                    
                    (values x)))
      (setf delta-x (/ (funcall f x)  (funcall f-prime x)))
      (setf denom-for-accuracy-test (+ (abs x)
                                       (abs (decf x delta-x)))) 
      
      (cond
       (prevent-bracket-jumping-p
        (if (< x x-left) (setf x x-left))
        (if (> x x-right) (setf x x-right))
        (if (< (/ (abs delta-x) denom-for-accuracy-test) accuracy)
          (return (values x ))))
       ((<= x-left x x-right)
        (if (< (/ (abs delta-x) denom-for-accuracy-test) accuracy)
          (return (values x ))))
       (t
        (error "jumped out of brackets")
        )))))
  
(defun exp-2 (x) (- (exp x) 2.0))

(defun base-ramification (b)
  (labels ((build-expression(b l)
             (cond ((= l 1) b)
                   (t (+ (expt b l) (build-expression b (1- l)))))))
    (- (build-expression b (node-solution-size (get-node))) (get-generate))))

(defun derivative-ramification (b)
  (labels ((build-expression(b l)
             (cond ((= l 1) 1)
                   (t (+ (* l (expt b (1- l))) (build-expression b (1- l)))))))
    (build-expression b (node-solution-size (get-node)))))

)

(defun base-ramification-test (b)
  (labels ((build-expression(b l)
             (cond ((= l 1) b)
                   (t (+ (expt b l) (build-expression b (1- l)))))))
    (- (build-expression b 25) (get-generate))))
             
             
  


