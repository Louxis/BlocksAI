;;;BF Search Recursive
(defun bfs-rec (node solution expand operatores)
  (labels ((bfs-aux (node solution expand operatores open close)
             (let ((expandedNodes (funcall expand node operatores 'bfs nil)))
             (cond ((null open) nil)
                   ((funcall solution node) node)
                   (t (bfs-aux (car open) solution expand operatores (open-bfs (cdr open) expandedNodes) (append close (list node))))))))
    (bfs-aux node solution expand operatores (list node) nil)))

;;;DF Search Recursive
(defun dfs-rec (node solution expand operatores)
  (labels ((bfs-aux (node solution expand operatores open close)
             (let ((expandedNodes (funcall expand node operatores 'bfs nil)))
             (cond ((null open) nil)
                   ((funcall solution node) node)
                   (t (bfs-aux (car open) solution expand operatores (open-dfs (cdr open) expandedNodes) (append close (list node))))))))
    (bfs-aux node solution expand operatores (list node) nil)))