(load "Byte.lisp")

(defun get-yes-no (msg &optional (default t))
  (let ((answer (progn (format t msg) (char-downcase (read-char)))))
    (cond ((eq answer #\y) t)
          ((eq answer #\Newline) default)
          (t nil))))

(format t "~%===========================")
(format t "~%= Welcome to Byte! =")
(format t "~%===========================~%")

(setf human1 (createHuman BLACK))
(setf human2 (createHuman WHITE))
(setf ai (createAI WHITE))
(loop as answer = t then (get-yes-no "~%Do you want to play again? [Y,n] ")
      while answer
      do (setf start? (get-yes-no "Do you want to start? [Y,n] "))
        (cond ((equalp start? t) (setf myGame (create-game human1 ai (create-initial-state 8))))
              (t(setf myGame (create-game ai human1 (create-initial-state 8)))))
          (startGameWithAI myGame))

;;(loop as answer = t then (get-yes-no "~%Do you want to play again? [Y,n] ")
;;      while answer
;;      do (setf board? (get-yes-no "Do you want to play on 10x10? [Y,n] "))
;;         (cond ((equalp board? t) (setf myGame (create-game human1 ai (create-initial-state 10))))
;;              (t(setf myGame (create-game ai human1 (create-initial-state 8)))))
;;          (startGame myGame))