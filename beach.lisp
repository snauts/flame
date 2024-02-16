(defparameter *beach-walkable*
  '(130 138 146 154 162 170 178 186
    132 140 148 156 164 172 180 188))

(defun beach-tile (id)
  (tile id :pl 1))

(defun beach-rocks (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (beach-tile 129))))

(defun dune-R ()
  (beach-rocks 2 4 4 6))

(defun dune-L ()
  (beach-rocks 4 4 6 6))

(defun dune-2 ()
  (beach-rocks 0 4 2 6))

(defun dune-4 ()
  (join (beach-rocks 0 6 2 8) (beach-rocks 6 6 8 8)))

(defun dune-8 ()
  (beach-rocks 0 6 8 8))

(defun beach-dune (type)
  (case type
    (2 (dune-2))
    (4 (dune-4))
    (8 (dune-8))))

(defun dune-platform (&key (width 1) (type 8))
  (join (dune-L) (multiply (beach-dune type) width) (dune-R)))

(defun beach-level ()
  (join
   (dune-platform :width 2)
   (empty 2)
   (dune-platform :width 2 :type 2)
   (empty 2)
   (dune-platform :width 2 :type 4)
   (empty 2)
   (dune-platform :width 10)
   (empty 48)))
