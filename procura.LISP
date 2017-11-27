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

;;;BF Search Iterative
;;;Test: (node-print (bfs-it (test-node) 'solution-nodep 'node-expand (operators)))
(defun bfs-it (node solution expand operators &optional (d nil))
  (setq *open* (list node))
  (setq *close* nil)
  (loop while (not (null *open*)) do
        (let* ((currentNode (car *open*)) (expandedNodes (filter-nodes (funcall expand currentNode operators 'bfs d) *open* *close*)))  
          ;Add currentNode to closed list
          (setq *close* (append *close* (list currentNode)))
          ;Remove current node from open
          (setq *open* (cdr *open*))
          ;Add expanded nodes to open
          (setq *open* (open-bfs *open* expandedNodes))
          ;Check if a node is a possible solution and return it
          (mapcar #'(lambda (nodeit) (if (funcall solution nodeit) (progn (return nodeit)))) expandedNodes))))

;;;BF Search Recursive
(defun bfs-rec (node solution expand operatores)
  (labels ((bfs-aux (node solution expand operatores open close)
             (let ((expandedNodes (funcall expand node operatores 'bfs nil)))
             (cond ((null open) nil)
                   ((funcall solution node) node)
                   (t (bfs-aux (car open) solution expand operatores (open-bfs (cdr open) expandedNodes) (append close (list node))))))))
    (bfs-aux node solution expand operatores (list node) nil)))


;;;DF Search
(defun dfs-it (node solution expand operators p)
   (setq *open* (list node))
   (setq *close* nil)
  (loop while (not (null *open*)) do
        (let ((currentNode (car *open*)) (expandedNodes (filter-nodes (funcall expand currentNode operators 'dfs p) *open* *close*)))
          ;Add currentNode to closed list
          (setq *close* (append *close* (list currentNode)))
          ;Remove current node from open
          (setq *open* (cdr *open*))
          ;Add expanded nodes to open
          (setq *open* (open-dfs *open* expandedNodes))
          ;Check if a node is a possible solution and return it
          (mapcar #'(lambda (nodeit) (if (funcall solution nodeit) (return nodeit))) expandedNodes))))

;;;DF Search Recursive
(defun dfs-rec (node solution expand operatores)
  (labels ((bfs-aux (node solution expand operatores open close)
             (let ((expandedNodes (funcall expand node operatores 'bfs nil)))
             (cond ((null open) nil)
                   ((funcall solution node) node)
                   (t (bfs-aux (car open) solution expand operatores (open-dfs (cdr open) expandedNodes) (append close (list node))))))))
    (bfs-aux node solution expand operatores (list node) nil)))