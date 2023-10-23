(defparameter *alps-walkable*
  '(67 75 83 91 99 107 115 123 71 79))

(defun alpine-tile (id)
  (tile id :pl 1))

(defun rocks ()
  (fill-box 8 8 (alpine-tile 65)))

(defun alps-walk (&optional (n 1))
  (join
   (crop 0 0 1 3 (rocks))
   (multiply (crop 0 4 8 8 (rocks)) n)
   (crop 1 0 2 3 (rocks))))

(defun mountain-level ()
  (join
   (alps-walk 3)
   (empty 4)
   (alps-walk 16)
   (empty 64)))
