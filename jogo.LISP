;;O JOGO
;;read the other files

(defpackage :p150221044-150221059)


(in-package :p150221044-150221059)


;(load)
;(load)

(defun start-log (pathname)
  (with-open-file (out pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "--Log started--")))

(defun write-log (pathname result time player)
  (let ((move (car result))
        (explored (second result))
        (cuts (third result))
        (depth (fourth result)))
    (with-open-file (out pathname :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~%Player~d played:~s Explored: ~d Alpha Cuts: ~d Took: ~d ms Depth: ~d~%" player (move-writer move) explored cuts time depth))))