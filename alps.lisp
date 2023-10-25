(defparameter *alps-walkable*
  '(67 75 83 91 99 107 115 123 71 79 119 127))

(defun alpine-tile (id)
  (tile id :pl 1))

(defun rocks ()
  (fill-box 16 8 (alpine-tile 65)))

(defvar *rock-type* nil)

(defun two-leaf ()
  (crop 0 3 2 4 (rocks)))

(defun small-plant ()
  (crop 2 2 6 4 (rocks)))

(defun flower-bush ()
  (crop 2 0 6 2 (rocks)))

(defun flowers-and-plant (rocks)
  (box-pipe
   (place 4 3 rocks (small-plant))
   (place 0 3 pipe (flower-bush))))

(defun alpine-rock-base (hang)
  (let ((rock (crop 0 4 8 8 (rocks))))
    (place 0 0 rock (if (= hang 0) (empty 1) (crop 8 7 16 8 (rocks))))))

(defun alpine-rocks (hang)
  (let ((rocks (alpine-rock-base hang)))
    (case *rock-type*
      (0 rocks)
      (1 (place 2 3 rocks (two-leaf)))
      (2 (place 4 3 rocks (small-plant)))
      (3 (place 0 3 rocks (flower-bush)))
      (4 (flowers-and-plant rocks))
      (5 (crop 6 0 8 4 (rocks))))))

(defun alps-walk (&key (width 1) (type 0) (hang 0))
  (let ((*rock-type* type))
    (join
     (crop 0 0 1 3 (rocks))
     (multiply (alpine-rocks hang) width)
     (crop 1 0 2 3 (rocks)))))

(defun mountain-level ()
  (join
   (alps-walk :width 3)
   (empty 2)
   (alps-walk :width 1 :type 1)
   (empty 2)
   (alps-walk :width 1 :type 2)
   (trigger "emit_bee_stream")
   (empty 2)
   (alps-walk :width 1 :type 3)
   (empty 2)
   (alps-walk :width 1 :type 4)
   (empty 2)
   (alps-walk :width 1 :type 5)
   (empty 2)
   (place 0 3 (empty 1) (alps-walk :width 1 :type 0 :hang 1))
   (empty 2)
   (place 0 5 (empty 1) (alps-walk :width 1 :type 4 :hang 1))
   (empty 64)))
