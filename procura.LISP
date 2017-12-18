 
;;;;Searching algorithms
;;;;Made by José Pereira and Lyudmyla Todoriko
;;;;The node structure is {((board) (pieces remaining)) (parent node) (g) (optional h and f)

;;;Global variables
(defparameter *open* nil)
(defparameter *close* nil)

;;;Aux functions
(defun open-bfs (open child)
  (append open child))

(defun open-dfs (open child)
  (append child open))

(defun equal-node (node1 node2)
  (equal (node-board (node-state node1)) (node-board (node-state node2))))

(defun node-existsp (node list)
  (if (member node list :test 'equal-node) t nil))
;;;

;;;Use with BFS and DFS
(defun filter-nodes (node-list open-list close-list)
  (cond ((null node-list) nil)
        ((or (node-existsp (car node-list) open-list) (node-existsp (car node-list) close-list)) (filter-nodes (cdr node-list) open-list close-list))
        (t (cons (car node-list) (filter-nodes (cdr node-list) open-list close-list)))))

;;;Use with A*
(defun filter-nodes-update-open (node-list open-list &optional (cost 'node-f))
  "Verifies if any of the duplicate nodes are better than the current nodes"
  (cond ((null node-list) open-list)
        ((node-existsp (car node-list) open-list)
         (let* ((new-node (car node-list)) (existing-node (car (member new-node open-list :test 'equal-node))))
           (if (>= (funcall cost new-node) (funcall cost existing-node))
               (filter-nodes-update-open (cdr node-list) open-list)
             (filter-nodes-update-open (cdr node-list) (substitute new-node existing-node open-list :count 1 :test #'equal))))) 
        (t (filter-nodes-update-open (cdr node-list) open-list))))

;;;Performance lexic
(let ((results (list 0 -1 0 nil)))
;;;BF Search Iterative
;;;Test: (node-print (bfs-it (test-node-a) 'solution-nodep 'node-expand (operators)))
(defun bfs-it (node solution expand operators &optional (d nil))
  "Breadth-first search (BFS) is an algorithm for traversing or searching tree or graph data structures. 
   It starts at the tree root and explores the neighbor nodes first, before moving to the next level neighbours."
  (start-performance)
  (setq *open* (list node))
  (setq *close* nil)
  (loop while (not (null *open*)) do
        (let* ((current-node (car *open*)) (expanded-nodes (filter-nodes (funcall expand current-node operators 'bfs d) *open* *close*)))  
          (add-explored 1)
          (add-generate (length expanded-nodes))
          ;Add currentNode to closed list
          (setq *close* (append *close* (list current-node)))
          ;Remove current node from open
          (setq *open* (cdr *open*))
          ;Add expanded nodes to open
          (setq *open* (open-bfs *open* expanded-nodes))
          ;Check if a node is a possible solution and return it
          (mapcar #'(lambda (expanded-node) (cond ((funcall solution expanded-node) (stop-performance expanded-node)(return expanded-node))))expanded-nodes))))

;;;DF Search Iterative
(defun dfs-it (node solution expand operators p)
  "Depth-first search (DFS) is an algorithm for traversing or searching tree or graph data structures.
   One starts at the root and explores as far as possible along each branch before backtracking."
  (start-performance)
  (setq *open* (list node))
  (setq *close* nil)
  (loop while (not (null *open*)) do
        (let* ((current-node (car *open*)) (expanded-nodes (filter-nodes (funcall expand current-node operators 'dfs p) *open* *close*)))
          (add-explored 1)
          (add-generate (length expanded-nodes))
          ;Add currentNode to closed list
          (setq *close* (append *close* (list current-node)))
          ;Remove current node from open
          (setq *open* (cdr *open*))
          ;Add expanded nodes to open
          (setq *open* (open-dfs *open* expanded-nodes))
          ;Check if a node is a possible solution and return it
          (mapcar #'(lambda (expanded-node) 
                      (cond ((funcall solution expanded-node) 
                             (stop-performance expanded-node)(return expanded-node)))) expanded-nodes))))

;;;A* search iterative
;;;Test: (node-print (a-star (test-node-a) 'solution-nodep 'node-expand-a (operators) 'heuristic-squares 'node-f))
(defun a-star (node solution expand operators heuristic cost)
  "A* is an informed search algorithm, or a best-first search, meaning that it solves problems by searching among all possible paths
   to the solution (goal) for the one that incurs the smallest cost.
   Will use the given heuristic"
  (start-performance)
  (setq *open* (list node))
  (setq *close* nil)
  (loop while (not (null *open*)) do
        (let* ((current-node (car *open*)) 
               (unfiltered-nodes (funcall expand current-node operators heuristic))
               (expanded-nodes (filter-nodes unfiltered-nodes *open* *close*)))            
          (add-explored 1)
          (add-generate (length expanded-nodes))
          (setq *close* (append *close* (list current-node)))
          (cond ((funcall solution current-node) (stop-performance current-node)(return current-node)))
          ;(setq *open* (append (cdr *open*) expanded-nodes))       
          (setq *open* (ordered-insert-list (cdr *open*) expanded-nodes))
          (setq *open* (filter-nodes-update-open unfiltered-nodes *open*))
          ;Failsafe
          (stable-sort *open* #'< :key cost)
)))


;;;IDA* search iterative
;;;(node-print (ida-star (test-node-a) 'solution-nodep 'node-expand-a (operators) 'heuristic-squares 'node-f))
(defun ida-star (node solution expand operators heuristic cost &optional (bound (funcall cost node)))  
  "Iterative-deepening-A* works as follows: at each iteration, perform a depth-first search,
   cutting off a branch when its total cost f(n)=g(n)+h(n) exceeds a given threshold.
   This threshold starts at the estimate of the cost at the initial state, and increases for each iteration of the algorithm.
   At each iteration, the threshold used for the next iteration is the minimum cost of all values that exceeded the current threshold."
  (labels ((ida-star-aux (node solution expand operators heuristic cost &optional (bound (funcall cost node)))
             (setq *open* (list node))
             (setq *close* nil)
             (loop while (not (null *open*)) do       
                   ;Node cost is bigger than the current bound, start a new search with a bigger bound
                   (if (> (funcall cost (car *open*)) bound) (return (ida-star node solution expand operators heuristic cost (funcall cost (car *open*)))))
                   ;Nodes cost is lesser than the current bound, keep searching with the current bound
                           (let* ((current-node (car *open*)) 
                                  (unfiltered-nodes (funcall expand current-node operators heuristic))
                                  (expanded-nodes (filter-nodes unfiltered-nodes *open* *close*))) 
                             (add-explored 1)
                             (add-generate (length expanded-nodes))
                             (setq *close* (append *close* (list current-node)))
                             (cond ((funcall solution current-node) (stop-performance current-node)(return current-node)))
                             (setq *open* (ordered-insert-list (cdr *open*) expanded-nodes))
                             (setq *open* (filter-nodes-update-open unfiltered-nodes *open*))
                             ;Failsafe
                             (stable-sort *open* #'< :key cost)))))
    (start-performance)  
    ;auxiliar function to avoid restarting performance operations
    (ida-star-aux node solution expand operators heuristic cost bound)))

;;;Perfomance calculations
(defun start-performance ()
  "Starts performance with the results at 0"
  (setf results (list 0 0 0 nil))
  (start-timer))


(defun stop-performance (node)
  "Stops timer and prints the results with print-results function"
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
  "Starts timer with the internal-real-time function for better precision, the time is counted in ms"
  (setf results (list (nth 0 results) (nth 1 results) (get-internal-real-time) (nth 3 results))))

(defun stop-timer () 
  "Calculates the difference between the current time and the timer registered on start in ms"
  (setf results (list (nth 0 results) (nth 1 results) (- (get-internal-real-time)(nth 2 results)) (nth 3 results))))

(defun calculate-pen(node) 
  "Calculates penetration with L/T"
  (/ (node-solution-size node) (nth 0 results)))

(defun print-results ()
  "Prints all the relevant results based on the final node, only to be used by the function stop-performance"
  (format t 
"Tamanho da solução: ~d~%
Nós gerados: ~d~%
Nós explorados: ~d~%
Tempo de execução: ~dms~%
Penetrância: ~d~%~%
Ramificação média: ~s~%"
          (node-solution-size (get-node))
          (get-generate)
          (get-explored)
          (get-timer)
          (calculate-pen (get-node))
          (calculate-average-ramification)))

(defun board-write-file (board pathname &optional message)
  (with-open-file (out pathname :direction :output :if-exists :append :if-does-not-exist :create)
    (format out message))
  (mapcar #'(lambda (line) (board-line-write line pathname)) board)
  (with-open-file (out pathname :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~%")))

(defun results-write-file (pathname)
    (with-open-file (out pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "Tamanho da solução: ~d~%
Nós gerados: ~d~%
Nós explorados: ~d~%
Tempo de execução: ~dms~%
Penetrância: ~d~%~%
Ramificação média: ~s~%~%"
          (node-solution-size (get-node))
          (get-generate)
          (get-explored)
          (get-timer)
          (calculate-pen (get-node))
          (calculate-average-ramification)))
    (board-write-file (node-board (node-state (node-original (get-node)))) pathname "Inicio:~%")
    (board-write-file (node-board (node-state (get-node))) pathname "Fim:~%")
    (format t "~%Results were saved in ~s~%~%" pathname))

(defun board-line-write (line pathname)
  (with-open-file (out pathname :direction :output :if-exists :append :if-does-not-exist :create)    
    (mapcar #'(lambda (cell) (format out "~d " cell)) line)
    (format out "~%")))

(defun calculate-average-ramification()
  "Used to calculate our average ramification base on Newton-Raphson and testing if a solution only contains one node"
  (if (= (node-solution-size (get-node)) 1)
      ;If the solution only contains 1 node, then we have ramification = generate nodes (b^1 = t)
      (get-generate)
    ;Else we can apply the normal formula b+b^1+b^2....b^L = T
    (Newton-Raphson #'base-ramification #'derivative-ramification 0.0 5.0)))

;;Newton-Raphson function used to calculate the root of a function between two intervals
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
        (error "jumped out of brackets"))))))  

;;Base function to calculate ramification
;;We have b+b^1+b^2...+b^L = T and we can convert it into
;;b+b^1+b^2...+b^L - T = 0
(defun base-ramification (b)
  "Ramification expression based on a variable B and the solution node found in results"
  (labels ((build-expression(b l)
             (cond ((= l 1) b)
                   (t (+ (expt b l) (build-expression b (1- l)))))))
    (- (build-expression b (node-solution-size (get-node))) (get-generate))))

;;To be used on Raphson, we need the first derivative of our ramification expression
(defun derivative-ramification (b)
  "First derivative expression of the ramification formula used on base-ramification"
  (labels ((build-expression(b l)
             (cond ((= l 1) 1)
                   (t (+ (* l (expt b (1- l))) (build-expression b (1- l)))))))
    (build-expression b (node-solution-size (get-node)))))
;;;End of performance lexic
)         
  


