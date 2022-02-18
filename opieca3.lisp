#|  Name:       Craig Opie
    Assignment: 3
    Sources:    https://lisp-lang.org/learn/getting-started/
                https://linux.die.net/man/1/sbcl
                http://www.sbcl.org/manual/#Global-and-Always_002dBound-variables
                https://gigamonkeys.com/book/syntax-and-semantics.html
                https://gigamonkeys.com/book/numbers-characters-and-strings.html
                https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node52.html
                https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node75.html
|#

;;; Create a recursive implementation of the Fibonacci function.
(defun FIBONACCI(&optional index)
    (cond 
        ((null index) (format t "Error: Index is null."))
        ((not (integerp index)) (format t "Error: Index is not an integer."))
        ((< index 0) (format t "Error: Index must be positive."))
        ((equal index 0) 0)
        ((equal index 1) 1)
        ((+ (FIBONACCI (- index 1)) (FIBONACCI (- index 2))))))

;;; Create a recursive implementation of the Greatest Common Divisor function.
(defun MYGCD(num_1 num_2 &optional num_3)
    (cond 
        ((or (null num_1) (null num_2)) (format t "Error: Minimum number of inputs: (2)"))
        ((not (or (integerp num_1) (integerp num_2))) (format t "Error: Inputs must be positive integers."))
        ((or (< num_1 0) (< num_2 0)) (format t "Error: Inputs must be positive integers."))
        ((equal num_1 0) num_2)
        ((null num_3) (MYGCD (mod num_2 num_1) num_1))
        (T (MYGCD (MYGCD num_1 num_2) num_3))))

;;; Create a recursive implementation of the Least Common Multiple function.
(defun MYLCM(num_1 num_2 &optional num_3)
    (cond 
        ((or (null num_1) (null num_2)) (format t "Error: Minimum number of inputs: (2)"))
        ((not (or (integerp num_1) (integerp num_2))) (format t "Error: Inputs must be positive integers."))
        ((or (< num_1 0) (< num_2 0)) (format t "Error: Inputs must be positive integers."))
        ((equal num_1 0) num_2)
        ((null num_3) (/ (* num_1 num_2) (MYGCD num_1 num_2)))
        (T (MYLCM (MYLCM num_1 num_2) num_3))))

;;; Create a recursive implementation of a remove-numbers function.
(defun REMOVE-NUMBERS(list_)
    (cond 
        ((not (listp list_)) (format t "Error: Argument must be a list."))
        ((null list_) nil)
        ((listp (car list_)) (cons (REMOVE-NUMBERS (car list_)) (REMOVE-NUMBERS(cdr list_))))
        ((equal (cdr list_) 0) (if (numberp (car list_)) nil (car list_)))
        ((numberp (car list_)) (REMOVE-NUMBERS (cdr list_)))
        (T (cons (car list_) (REMOVE-NUMBERS (cdr list_))))))

