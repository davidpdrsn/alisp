(defun add (a b)
  (+ a b))

(defun main ()
  (print (filter (lambda (x) (< x 2)) [1 2 3 4]))
  (print (fold add 0 [1 2 3 4])))
