(defparameter *beach-walkable*
  '(300))

(defun beach-walk (n)
  (multiply (stack (cell 300) 2) n))

(defun beach-level ()
  (beach-walk 256))
