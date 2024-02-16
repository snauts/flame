(defparameter *beach-walkable*
  '(130 138 146 154 162 170 178 186
    132 140 148 156
    134 142 150 158 166 174))

(defun beach-tile (id &key (v 0) (h 0))
  (tile id :pl 1 :v v :h h))

(defun beach-rocks (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (beach-tile 129))))

(defun dune-R ()
  (beach-rocks 0 4 2 6))

(defun dune-L ()
  (beach-rocks 2 4 4 6))

(defun dune-4 ()
  (join (beach-rocks 0 6 2 8) (beach-rocks 6 6 8 8)))

(defun dune-8 ()
  (beach-rocks 0 6 8 8))

(defun beach-dune (type)
  (case type
    (4 (dune-4))
    (8 (dune-8))))

(defun dune-platform (&key (width 1) (type 8))
  (join (dune-L) (multiply (beach-dune type) width) (dune-R)))

(defun bamboo-platform-base (width)
  (let ((left (beach-rocks 0 2 2 4))
	(right (beach-rocks 4 2 6 4)))
    (join left (multiply (beach-rocks 2 2 4 4) width) right)))

(defun toping-map (n)
  (case n
    (0 (list nil))
    (1 (list 164 172))
    (2 (list 163 171))))

(defun toping-tiles (toping)
  (apply #'append (mapcar #'toping-map toping)))

(defun bamboo-platform (&key (width 1) (pos 2) toping)
  (box-pipe
   (bamboo-platform-base width)
   (dolist (item (toping-tiles toping) pipe)
     (when (numberp item)
       (let ((tile (cell (beach-tile item))))
	 (setf pipe (place pos 1 pipe tile))))
     (incf pos))))

(defun simple-cross ()
  (beach-rocks 6 0 8 2))

(defun dimmed-V ()
  (beach-rocks 2 1 4 2))

(defun dimmed-^ ()
  (beach-rocks 0 1 2 2))

(defun bamboo-latice (w h)
  (let ((cx (1+ (* w 2)))
	(cy (1- (* h 2))))
    (box-pipe
     (dotimes (y h pipe)
       (dotimes (x w pipe)
	 (setf pipe (place (1+ (* 2 x)) (* 2 y) pipe (simple-cross)))))
     (dotimes (x w pipe)
       (setf pipe (place (1+ (* 2 x)) 0 pipe (beach-rocks 2 0 4 1)))
       (setf pipe (place (1+ (* 2 x)) cy pipe (dimmed-V))))
     (dotimes (y (* 2 (1- h)) pipe)
       (let ((L-cell (cell (beach-tile (if (oddp y) 181 182))))
	     (R-cell (cell (beach-tile (if (oddp y) 189 190)))))
	 (setf pipe (place 0 (1+ y) pipe L-cell))
	 (setf pipe (place cx (1+ y) pipe R-cell))))
     (place 0 0 pipe (cell (beach-tile 175 :h 1)))
     (place 0 cy pipe (cell (beach-tile 143)))
     (place cx 0 pipe (cell (beach-tile 175)))
     (place cx cy pipe (cell (beach-tile 143 :h 1))))))

(defun single-bamboo-platform (width height)
  (box-pipe
   (place 1 0 pipe (bamboo-latice width height))
   (place 0 (* 2 height) pipe (bamboo-platform :width width))))

(defun beach-level ()
  (join
   (dune-platform :width 2)
   (empty 2)
   (single-bamboo-platform 1 2)
   (empty 2)
   (single-bamboo-platform 2 3)
   (empty 2)
   (single-bamboo-platform 3 1)
   (empty 2)
   (single-bamboo-platform 4 2)
   (empty 2)
   (dune-platform :width 2 :type 4)
   (empty 2)
   (dune-platform :width 2 :type 4)
   (empty 2)
   (dune-platform :width 10)
   (empty 48)))
