(defparameter *alps-walkable*
  '(67 75 83 91 99 107 115 123 71 79))

(defun alpine-tile (id)
  (tile id :pl 1))

(defun rocks ()
  (fill-box 8 8 (alpine-tile 65)))

(defvar *rock-type* nil)

(defun two-leaf ()
  (crop 0 3 2 4 (rocks)))

(defun small-plant ()
  (crop 2 2 6 4 (rocks)))

(defun alpine-rocks ()
  (let ((rocks (crop 0 4 8 8 (rocks))))
    (case *rock-type*
      (0 rocks)
      (1 (place 2 3 rocks (two-leaf)))
      (2 (place 4 3 rocks (small-plant))))))

(defun alps-walk (&key (width 1) (type 0))
  (let ((*rock-type* type))
    (join
     (crop 0 0 1 3 (rocks))
     (multiply (alpine-rocks) width)
     (crop 1 0 2 3 (rocks)))))

(defun mountain-level ()
  (join
   (alps-walk :width 3)
   (empty 4)
   (alps-walk :width 1 :type 1)
   (empty 2)
   (alps-walk :width 1 :type 2)
   (trigger "emit_bee_stream")
   (empty 2)
   (alps-walk :width 8)
   (empty 64)))
