(defun add (a b)
  (+ a b))

(defun main ()
  (print (filter (lambda (x) (< x 5)) [1 2 3 4 5 6 7 8 9 10]))
  (print (fold add 0 [1 2 3 4 5 6 7 8 9 10])))
