(defparameter *alps-walkable*
  '(100))

(defun alps-walk (n)
  (multiply (stack (cell 100) 2) n))

(defun mountain-level ()
  (alps-walk 256))
