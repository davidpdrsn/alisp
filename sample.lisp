(defun add (a b)
  (+ a b))

(defun main ()
  (let ((a 1)
        (b 2)
        (z (print a)))
    (print (add (add a a) (add b b)))))
