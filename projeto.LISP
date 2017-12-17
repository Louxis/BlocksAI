

(defun write-file (filename message)
  (with-open-file (f filename :direction :output
                     :if-exists :append)
    (format f message)))


(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read stream nil)
          while line
          collect line)))

(defun start()
  (let ((boards (initialize)))
    (labels ((read_board () 
                        (let ((board (choose-board)))
                          (cond
                           ((or(< board 0) (> board (length boards))) (read_board))
                           (t (nth board boards))))))(read_board))))

(defun initialize ()
 (format t "~%Welcome to BlocksAI!~% Please insert the filepath you want to play with.~%")
 (read-file (read)))

(defun choose-board()
  (format t "~%Choose the board you want to test~%")
   (*(- (read) 1) 2))