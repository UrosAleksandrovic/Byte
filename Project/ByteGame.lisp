(load "Byte.lisp")

(defun get-yes-no (msg &optional (default t))
  (let ((answer (progn (format t msg) (char-downcase (read-char)))))
    (cond ((eq answer #\y) t)
          ((eq answer #\Newline) default)
          (t nil))))

(format t "~%===========================")
(format t "~%= Welcome to Byte! =")
(format t "~%===========================~%")

(setf human1 (create-human-player BLACK))
(setf human2 (create-human-player WHITE))
(loop as answer = t then (get-yes-no "~%Do you want to play again? [Y,n] ")
      while answer
      do (setf start? (get-yes-no "Do you want to start? [Y,n] "))
         (setf winner (ttt-game-start (create-ttt-game (if start? human ai) (if start? ai human))))
         (if winner
           (format t "~%~%The winner is ~A!~%" (player-symbol winner))
           (format t "~%~%Its a tie!!~%")))