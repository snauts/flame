(defparameter *town-walkable*
  '(999))

(defun town-walk (n)
  (multiply (stack (cell 999) 2) n))

(defun town-level ()
  (join (town-walk 256) (empty 64)))
