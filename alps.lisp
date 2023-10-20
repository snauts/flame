(defparameter *alps-walkable*
  '(67 75 83 91 99 107 115 123))

(defun alpine-tile (id)
  (tile id :pl 1))

(defun rocks ()
  (fill-box 8 8 (alpine-tile 65)))

(defun alps-walk (&optional (n 1))
  (multiply (crop 0 4 8 8 (rocks)) n))

(defun mountain-level ()
  (alps-walk 16))
