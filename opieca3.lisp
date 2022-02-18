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

;;; Write a macro called MYSHOUT that takes a string, and prints to the screen your name (taken from +ID+), 
;;; "says", then the string in all capital letters followed by an exclamation point. Thus: (MYSHOUT "get off
;;; my lawn") prints "[Your Name] says GET OFF MY LAWN!" to the screen. It should return T. Use the built-in
;;; functions FORMAT (once) and STRING-UPCASE in this macro.
(defmacro MYSHOUT()

)

;;; Write a macro CALLFUNCTION that takes two arguments: the name of a function and a list of arguments. It
;;; returns the result of calling that function on the arguments. Thus: (CALLFUNCTION + (1 2)) --> 3. Another
;;; example: (CALLFUNCTION LIST ('a 'b 'c)) --> (a b c).
(defmacro CALLFUNCTION()

)

;;; Write a macro WEIRDCALL that takes two arguments: a list of two mathematical function names, and a list
;;; of two lists of arguments. It will return a list of the result of applying the first function to the
;;; second list of arguments, and the second function to the first list of arguments.
;;; Thus: (WEIRDCALL (+ -) ((2 1) (6 1))) --> (7 1). Another example: (WEIRDCALL (list list) (('a 'b) ('c 'd)))
;;; --> ((c d) (a b))
(defmacro WEIRDCALL()

)

;;; Rewrite 1-3 without any 'syntactic sugar' (i.e. backquote, comma, or splice). If you wrote them that way
;;; in the first place, then rewrite them using syntactic sugar.