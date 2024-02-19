(defparameter *beach-walkable*
  '(130 138 146 154 162 170 178 186
    132 140 148 156
    134 142 150 158 166 174
    204 212))

(defun beach-tile (id &key (v 0) (h 0) (pr 0))
  (tile id :pl 1 :v v :h h :pr pr))

(defun beach-cell (id &key (v 0) (h 0) (pr 0))
  (cell (beach-tile id :v v :h h :pr pr)))

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
    (0 (list nil nil))
    (1 (list 164 172))
    (2 (list 163 171))))

(defun toping-tiles (toping)
  (apply #'append (mapcar #'toping-map toping)))

(defun bamboo-platform (&key (width 1) (pos 2) toping)
  (s-push (bamboo-platform-base width))
  (dolist (item (toping-tiles toping) (s-pop))
    (when (numberp item)
      (s-place pos 1 (beach-cell item)))
    (incf pos)))

(defun knot-cross ()
  (beach-rocks 6 0 8 2))

(defun simple-cross ()
  (on-top (beach-rocks 2 0 4 1)
	  (beach-rocks 0 0 2 1)))

(defun dimmed-V ()
  (beach-rocks 2 1 4 2))

(defun bamboo-side (&key (flip 0))
  (s-push (beach-cell 175 :h 1))
  (s-place 1 0 (beach-cell 176 :h 1))
  (s-place 2 0 (beach-cell 135 :h 1))
  (s-place 1 1 (beach-cell 175 :h 1))
  (s-place 2 1 (beach-cell (if (= flip 1) 160 152) :h flip))
  (if (= 0 flip) (s-pop) (flip (s-pop))))

(defun select-damage (type)
  (case type
    (1 '((190 182) (189 181)))
    (2 '((160 180) (187 136)))
    (3 '((188 152) (144 179)))
    (4 '((#x0 182) (#x0 181)))
    (5 '((190 #x0) (189 #x0)))
    (6 '((#x0 #x0) (#x0 #x0)))))

(defun beach-map (map)
  (for-all (transpone map) #'beach-tile))

(defun latice-damage (type)
  (beach-map (select-damage type)))

(defun bamboo-damage (dmg)
  (let ((y 1))
    (dolist (row dmg)
      (let ((x (if (oddp y) 0 1)))
	(dolist (item row)
	  (when (numberp item)
	    (s-place x y (latice-damage item)))
	  (incf x 2))
	(incf y)))))

(defun bamboo-latice (w h &key dmg (side 0))
  (let ((cx (1+ (* w 2)))
	(cy (1- (* h 2))))
    (s-push (empty 1))
    (dotimes (y h)
      (dotimes (x w)
	(s-place (1+ (* 2 x)) (* 2 y) (knot-cross))))
    (dotimes (x w)
      (s-place (1+ (* 2 x)) 0 (beach-rocks 2 0 4 1))
      (s-place (1+ (* 2 x)) cy (dimmed-V)))
    (dotimes (y (* 2 (1- h)))
      (s-place 0 (1+ y) (beach-cell (if (oddp y) 181 182)))
      (s-place cx (1+ y) (beach-cell (if (oddp y) 189 190))))
    (bamboo-damage dmg)
    (s-place 0 cy (beach-cell 143))
    (s-place cx cy (beach-cell 143 :h 1))
    (s-place (+ (* 2 side) cx) 0 (beach-cell 175))
    (s-place (* -2 side) 0 (beach-cell 175 :h 1))
    (when (= side 1)
      (s-place (+ 2 cx) 0 (bamboo-side :flip 1))
      (s-place 0 0 (bamboo-side)))
    (s-pop)))

(defun single-bamboo-platform (w h &key (side 0) top dmg)
  (let ((px (- (* 2 side) 1)))
    (s-push (bamboo-latice w h :side side :dmg dmg))
    (s-place px (* 2 h) (bamboo-platform :width w :toping top))
    (s-pop)))

(defun stalk-tile (h)
  (cond ((evenp h) 196)
	((> h 1) 195)
	(t 194)))

(defun single-stalk (x y pr h)
  (loop while (> (decf h) 0) do
    (s-place x (incf y) (beach-cell (stalk-tile h) :pr pr))))

(defun bamboo-stalks (&key (width 2) (height 5) (variation 2))
  (s-push (dune-platform :width width :type 8))
  (dotimes (i (* 4 width) (s-pop))
    (let* ((x (+ 3 (* i 2)))
	   (y (logand i 1))
	   (pr (- 1 (logand i 1)))
	   (id (elt '(204 203 212 211) (logand i 3))))
      (s-place x y (beach-cell id :pr pr))
      (single-stalk x y pr (+ height (xor-random variation))))))

(defun diagonal-stick (len &key (side 168) (end (beach-tile 202 :h 1)))
  (s-push (empty 1))
  (let ((x 0) (y 0))
    (dotimes (i len (s-pop))
      (let ((tile (if (= i (1- len)) end (beach-tile 176 :h 1))))
	(s-place (incf x) (incf y) (beach-cell tile))
	(s-place (1- x) y (beach-cell 175 :h 1))
	(s-place x (1- y) (beach-cell side :h 1))))))

(defun collapsed-scaffold ()
  (s-push (bamboo-latice 1 2 :side 0 :dmg nil))
  (s-place -8 0 (dune-platform :width 2 :type 8))
  (s-place  2 1 (beach-rocks  8 7 10 8))
  (s-place  2 2 (beach-rocks  0 0  2 1))
  (s-place  3 2 (diagonal-stick 4))
  (s-place  1 2 (flip (diagonal-stick 1 :end 210)))
  (s-place  3 2 (flip (diagonal-stick 5 :end 210)))
  (s-place  8 3 (flip (diagonal-stick 1 :side 135)))
  (s-place  8 2 (flip (diagonal-stick 6)))
  (s-place 10 3 (diagonal-stick 4 :end 210))
  (s-place 15 2 (diagonal-stick 3))
  (s-place 11 4 (simple-cross))
  (s-place  5 4 (simple-cross))
  (s-place  8 2 (beach-cell 136))
  (s-place 11 2 (beach-cell 189))
  (s-place  8 1 (beach-rocks  8 7 12 8))
  (s-place  9 3 (beach-rocks  0 0  2 1))
  (s-place 14 1 (beach-rocks 12 7 14 8))
  (s-place 14 2 (beach-rocks  0 0  2 1))
  (s-place 17 3 (beach-rocks 11 5 12 7))
  (s-pop))

(defun watchtower ()
  (let ((dmg '(nil (2) nil nil (4) nil nil (3) nil nil (nil 5))))
    (s-push (beach-rocks  8 7 16 8))
    (s-place 0 1 (single-bamboo-platform 1 8 :side 1 :dmg dmg))
    (s-place 0 1 (beach-map '((181 nil 179 191 183 187 nil 189))))
    (s-pop)))

(defun watchtower-and-sentinel (distance)
  (s-push (dune-platform :width distance))
  (let ((offset (+ 2 (* 8 (- distance 2)))))
    (s-place offset 1 (watchtower))
    (s-inject "emit_sentinel" (+ 3 offset))
    (s-pop)))

(defun beach-level ()
  (setf *seed* (* 1905 05 27))
  (join
   ;; PART1 watchtower with sentinel
   (watchtower-and-sentinel 8)

   ;; SANDBOX
   (empty 3)
   (collapsed-scaffold)
   (empty 3)
   (bamboo-stalks :width 3)
   (empty 1)
   (single-bamboo-platform 1 2 :side 1)
   (empty 1)
   (single-bamboo-platform 2 3 :side 1 :dmg '((nil 1) nil (4)))
   (empty 1)
   (single-bamboo-platform 3 1)
   (empty 1)
   (single-bamboo-platform 4 2 :side 1 :dmg '((nil 2 nil 3)))
   (empty 1)
   (dune-platform :width 2 :type 4)
   (empty 2)
   (dune-platform :width 2 :type 4)
   (empty 2)
   (dune-platform :width 10)
   (empty 48)))
