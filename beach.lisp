(defparameter *beach-walkable*
  '(100))

(defun beach-walk (n)
  (multiply (stack (cell 100) 2) n))

(defun beach-level ()
  (beach-walk 256))
