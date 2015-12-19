(defun main ()
  (let ((a 1)
        (b 2)
        (c (if (< (unless (= 1 1) a 10) b)
             0
             666)))
    (print c)))

