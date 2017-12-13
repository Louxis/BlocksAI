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

(let ((results (list 0 -1 0 0)))
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
  (reset-results)
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
  (reset-results)
  (setq *open* (list node))
  (setq *close* nil)
  (start-timer)
  (loop while (not (null *open*)) do
        (add-explored 1)
        (if (> (funcall cost (car *open*)) bound) (return (ida-star node solution expand operators heuristic cost (funcall cost (car *open*)))))
        (let* ((current-node (car *open*)) (expanded-nodes (filter-nodes (funcall expand current-node operators heuristic) *open* *close*)))  
          (add-generate (length expanded-nodes))
          (setq *close* (append *close* (list current-node)))
          (cond ((funcall solution current-node) (print-results current-node)(return current-node)))
          (setq *open* (ordered-insert-list (cdr *open*) expanded-nodes)))
))

;;;Perfomance calculations

(defun add-generate (value) (setf results (list (+ (nth 0 results) value) (nth 1 results) (nth 2 results) (nth 3 results))))

(defun add-explored (value) (setf results (list (nth 0 results) (+ (nth 1 results) value) (nth 2 results) (nth 3 results))))

(defun start-timer () 
  (setf results (list (nth 0 results) (nth 1 results) (get-internal-real-time) (nth 3 results))))

(defun stop-timer () 
  (setf results (list (nth 0 results) (nth 1 results) (- (get-internal-real-time)(nth 2 results)) (nth 3 results))))

(defun reset-results () 
  (setf results (list 0 0 0 0)))

(defun calculate-pen(node) 
  (/ (node-solution-size node) (nth 0 results)))

(defun print-results (node)
  (stop-timer)
  (format t "Nós gerados:~d~%Nós explorados:~d~%Tempo de execução:~dms~%Penetrância:~d~%~%"
          (nth 0 results)
          (nth 1 results)
          (nth 2 results)
          (calculate-pen node)))


(defun calculate-average-ramification())

)


