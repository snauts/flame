(defparameter *beach-walkable*
  '(130 138 146 154 162 170 178 186 132 140))

(defun beach-tile (id)
  (tile id :pl 1))

(defun beach-rocks (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (beach-tile 129))))

(defun dune-right-end ()
  (beach-rocks 0 4 1 6))

(defun dune-left-end ()
  (beach-rocks 1 4 2 6))

(defun beach-dune ()
  (beach-rocks 0 6 8 8))

(defun dune-platform (&key (width 1))
  (join (dune-left-end)
	(multiply (beach-dune) width)
	(dune-right-end)))

(defun beach-level ()
  (join
   (dune-platform :width 2)
   (empty 2)
   (dune-platform :width 2)
   (empty 2)
   (dune-platform :width 10)
   (empty 48)))
