;;This function returns sub list of a list
;;Arguments are list, first element of sublist and length of sublist
(defun getSubList (list firstElement lengthOfSublist) 
    (cond ((or (< firstElement 0) (> (+ lengthOfSublist firstElement) (length list)))
            (format t "First and last elements got to be in the range of given list"))
        ((null list) '())
        ((> firstElement 0) (getSubList (cdr list) (- firstElement 1) lengthOfSublist))
        ((> lengthOfSublist 0) (cons (car list) (getSubList (cdr list) firstElement (- lengthOfSublist 1))))))


;;Reads character from standard intput and puts in the list. Additionaly it shows message before reading.
(defun readChar (Message) (progn (format t  "~A " Message) (list (read))))

;;Returnes the index of element in list
(defun indexOf (element list)
    (cond ((null list) '())
        ((equalp element (car list)) 0)
        (t(let ((index (indexOf element (cdr list))))
               (if (null index) '() (+ 1 index))))))

;;Returnes true if index of element is even
(defun evenIndexp (element list) (evenp (indexOf element list)))

;;Round number division
(defun div (divisor divident) (/ (- divisor (mod divisor divident)) divident))

;;Tests if memList is part of second list 
(defun memberList (memList secondList) 
    (cond ((null secondList) '())
          ((equalp memList (car secondList)) t)
          (t(memberList memList (cdr secondList)))))

;;Joins two lists without duplicates
(defun join (firstList secondList)
    (cond ((null firstList) secondList)
          ((memberList (car firstList) secondList) (join (cdr firstList) secondList))
          (t(cons (car firstList) (join (cdr firstList) secondList)))))

;;Counts number of given element in the list
(defun countEl (element list) 
    (cond ((null list) 0)
          ((equal element (car list)) (+ 1 (countEl element (cdr list)))) 
          (t(countEl element (cdr list)))))

;;Inserts element in the list at givent index and deleting previous element from that place
(defun insertElement (element list index)
    (cond ((null list) '())
          ((equal index 0) (cons element (cdr list)))
          (t(cons (car list) (insertElement element (cdr list) (- index 1))))))

;;Gets first N elements of passed list 
(defun getFirstN (numOfElements list) 
(cond ((or (null list) (> numOfElements (length list)) (= numOfElements 0)) '())
      (t(cons (car list) (getFirstN (- numOfElements 1) (cdr list))))))

;;Returnes last N elements of passed list
(defun getLastN (numOfElements list)
(cond ((null list) (list EMPTY))
      ((> numOfElements 1) (getLastN (- numOfElements 1) (cdr list)))
      ((= numOfElements 0) (list EMPTY))
      (t list)))

;;Returnes all indexes of symbol in given list. Current index is 0 at the start
(defun findIndexOf (element list currentIndex)
    (cond ((null list) '())
          ((equalp element (car list)) (cons currentIndex (findIndexOf element (cdr list) (+ currentIndex INCREMENT))))
          (t(findIndexOf element (cdr list) (+ currentIndex INCREMENT)))))

;;Returnes elements from the list that are grater then given number
(defun returnIfGreaterOrEqual (comparisonElement list)
    (cond ((null list) '())
          ((>= (car list) comparisonElement) (cons (car list) (returnIfGreaterOrEqual comparisonElement (cdr list))))
          (t(returnIfGreaterOrEqual comparisonElement (cdr list)))))