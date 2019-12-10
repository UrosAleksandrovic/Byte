(load "Byte.lisp")

(defun get-yes-no (msg &optional (default t))
  (let ((answer (progn (format t msg) (char-downcase (read-char)))))
    (cond ((eq answer #\y) t)
          ((eq answer #\Newline) default)
          (t nil))))

(format t "~%===========================")
(format t "~%= Welcome to Byte! =")
(format t "~%===========================~%")

(setf human1 (create-player BLACK))
(setf human2 (create-player WHITE))
(loop as answer = t then (get-yes-no "~%Do you want to play again? [Y,n] ")
      while answer
      do (setf start? (get-yes-no "Do you want to start? [Y,n] "))
          (setf myGame (create-game human1 human2 (create-initial-state 8)))
          (startGame myGame))