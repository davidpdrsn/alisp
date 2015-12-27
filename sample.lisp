(defun fact (n)
  (if (= 0 n)
    1
    (* n (fact (- n 1)))))

(defun main ()
  (print (fact 100)))
