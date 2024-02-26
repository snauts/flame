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

(defun dune-segment (&key (width 1) (type 8))
  (multiply (beach-dune type) width))

(defun dune-platform (&key (width 1) (type 8))
  (join (dune-L) (dune-segment :width width :type type) (dune-R)))

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

(defun sand-pillar-base-decoration ()
  (beach-map '((181 nil 179 191 183 187 nil 189))))

(defun sand-bamboo-stubs (stubs)
  (s-push nil)
  (dolist (x stubs (s-pop))
    (s-join (beach-rocks (+ 8 (* x 2)) 7 (+ 10 (* x 2)) 8))))

(defun watchtower ()
  (let ((dmg '(nil (2) nil nil (4) nil nil (3) nil nil (nil 5))))
    (s-push (sand-bamboo-stubs '(0 1 2 3)))
    (s-place 0 1 (single-bamboo-platform 1 8 :side 1 :dmg dmg))
    (s-place 0 1 (sand-pillar-base-decoration))
    (s-place 7 7 (bamboo-platform :width 0))
    (s-place 5 3 (diagonal-stick 3))
    (s-place 5 3 (beach-cell 144))
    (s-place 5 4 (beach-cell 188))
    (s-place 8 6 (beach-cell 167 :h 1))
    (s-place 9 6 (beach-cell 143 :h 1))
    (s-pop)))

(defun watchtower-and-sentinel (distance)
  (s-push (dune-platform :width (- distance 1)))
  (let ((offset (+ 2 (* 8 (- distance 2)))))
    (s-place offset 1 (watchtower))
    (s-inject "emit_sentinel" (+ 3 offset))
    (s-pop)))

(defun stepping-platform (type)
  (case type
    (0 (single-bamboo-platform 3 1))
    (1 (single-bamboo-platform 1 2 :side 1))
    (2 (single-bamboo-platform 2 3 :side 1 :dmg '((nil 1) nil (4))))
    (3 (single-bamboo-platform 4 2 :side 1 :dmg '((nil 2 nil 3))))))

(defun shed-pillar (height)
  (let ((dmg '(nil nil (4) nil (nil 5) nil (4) nil (nil 5) nil (4))))
    (s-push (bamboo-latice 1 height :side 1 :dmg dmg))
    (s-place 0 0 (sand-pillar-base-decoration))
    (s-pop)))

(defun abandoned-shed (&key (width 10) (height 7))
  (s-push (dune-platform :width (floor (- width 2) 2)))
  (let ((x (+ 6 (* 2 (- width 2)))))
    (s-place 6 2 (shed-pillar height))
    (s-place 6 1 (sand-bamboo-stubs '(2 3 0 1)))
    (s-place x 2 (flip (shed-pillar height)))
    (s-place x 1 (sand-bamboo-stubs '(2 3 0 1)))
    (s-place 6 (+ 2 (* 2 height)) (bamboo-platform :width width))
    (s-inject "emit_drop_bears" 0)
    (s-pop)))

(defun pachinko-platform-row (w)
  (s-push nil)
  (dotimes (i w (s-pop))
    (let ((top (unless (or (= i 0) (= i (1- w))) '(1 1))))
      (s-place (* 10 i) 0 (bamboo-platform :width 1 :pos 1 :toping top)))))

(defun pachinko-dmg (i)
  (case i
    (0 nil)
    (1 '(nil nil (4) nil (nil 5)))
    (2 '(nil nil nil nil nil nil (4) nil (nil 5)))
    (3 '(nil nil (4) nil (nil 5) nil nil nil nil nil (4) nil (nil 5)))
    (4 '(nil nil nil nil nil nil (4) nil (nil 5)))
    (5 '(nil nil (4) nil (nil 5)))
    (6 nil)))

(defun pachinko-join ()
  (s-push (beach-cell 227))
  (s-place 1 0 (beach-cell 220))
  (s-place 1 1 (beach-cell 226))
  (s-place 2 0 (flip (beach-cell 227)))
  (s-pop))

(defun pachinko-latice (i)
  (let ((h (+ 2 (* 2 (if (< i 4) i (- 6 i))))))
    (bamboo-latice 1 h :side 1 :dmg (pachinko-dmg i))))

(defun dimmed-W ()
  (s-push (beach-cell 143))
  (s-place 1 0 (dimmed-V))
  (s-place 3 0 (flip (beach-cell 143)))
  (s-pop))

(defun pachinko-pyramid ()
  (s-push nil)
  (dotimes (i 7)
    (s-place (* 5 i) 0 (pachinko-latice i)))
  (dotimes (i 6)
    (s-place (+ 5 (* 5 i)) 0 (pachinko-join)))
  (s-place 12 3 (dimmed-W))
  (s-place 17 7 (dimmed-W))
  (s-place 22 3 (dimmed-W))
  (let ((x -4) (y 0))
    (dotimes (i 4 (s-pop))
      (s-place (incf x 5) (incf y 4) (pachinko-platform-row (- 4 i))))))

(defun beach-level ()
  (setf *seed* (* 1905 05 27))
  (join
   ;; PART1 watchtower with sentinel
   (watchtower-and-sentinel 8)
   (stepping-platform 1)

   ;; PART2 collapsed-scaffold
   (inject (collapsed-scaffold) "emit_crab_squad" 4)
   (empty 1)
   (stepping-platform 1)
   (empty 1)
   (inject (stepping-platform 2) "emit_twins" 2)
   (empty 1)
   (stepping-platform 1)
   (empty 1)

   ;; PART3 bamboo stalks
   (inject (bamboo-stalks :width 3) "emit_stalk_patrol" 1)
   (empty 2)
   (stepping-platform 0)
   (empty 2)
   (inject (stepping-platform 3) "emit_dirty_trio" 1)
   (empty 2)

   ;; PART4 abandoned shed
   (dune-platform :type 4)
   (empty 3)
   (abandoned-shed)
   (empty 3)

   ;; PART5 pachinko pyramid
   (bamboo-stalks :width 2)
   (trigger "emit_falling_crabs")
   (pachinko-pyramid)

   ;; ENDING
   (inject (dune-platform :width 10) "level_done" 48)
   (empty 48)))

(defun watchtower-MK2 ()
  (s-push (watchtower))
  (s-place 7 3 (bamboo-platform :width 0))
  (s-place 7 1 (diagonal-stick 1))
  (s-place 8 2 (beach-cell 167 :h 1))
  (s-place 9 2 (beach-cell 143 :h 1))
  (s-place 6 1 (beach-cell 136))
  (s-place 7 1 (beach-cell 144))
  (s-place 7 4 (beach-cell 228))
  (s-pop))

(defun sapling-top ()
  (case (xor-random 2)
    (0 (beach-rocks 10 2 13 3))
    (1 (beach-rocks 10 3 13 4))))

(defun bamboo-sapling (type)
  (case type
    (0 (beach-cell 197))
    (1 (beach-cell 198))
    (2 (beach-cell 205))
    (3 (beach-cell 206))
    (4 (beach-rocks 8 0 10 1))
    (5 (beach-rocks 8 1 10 2))
    (6 (beach-rocks 10 0 12 1))
    (7 (beach-rocks 10 1 12 2))))

(defun watchtower-plant (height base)
  (s-push nil)
  (s-place 1 0 (beach-cell base))
  (loop for y from 1 to height do
    (let* ((type (xor-random 8))
	   (x (if (>= type 6) 0 1)))
      (s-place x y (bamboo-sapling type))))
  (s-place 0 (1+ height) (sapling-top))
  (s-pop))

(defun add-watchtower-bamboo-saplings (offset)
  (loop for x from 1 to offset by 4 do
    (let ((base (if (= 0 (logand x 4)) 234 242)))
      (s-place x 1 (watchtower-plant (+ 3 (xor-random 4)) base)))))

(defun extra-platform ()
  (s-push nil)
  (s-place 0 6 (bamboo-platform :width 0))
  (s-place 2 0 (flip (diagonal-stick 5 :end (tile 167 :h 1))))
  (s-place 1 5 (beach-cell 143))
  (s-place 7 0 (beach-cell 136))
  (s-pop))

(defun planted-watchtower (call &key extra (distance 6))
  (s-push (dune-segment :width (1- distance)))
  (s-join (dune-segment :type 4))
  (let ((offset (* 8 (- distance 2))))
    (s-place offset 1 (watchtower-MK2))
    (add-watchtower-bamboo-saplings offset)
    (when extra (s-place (- offset 5) 12 (extra-platform)))
    (s-inject call (+ offset (if extra -4 3)))
    (s-pop)))

(defun dune-with-saplings (width)
  (s-push (dune-segment :width width))
  (add-watchtower-bamboo-saplings (* 8 (- width 1)))
  (s-pop))

(defun dunes-level ()
  (setf *seed* 794)
  (join
   (dune-segment :width 2)
   (planted-watchtower "emit_marksman")
   (planted-watchtower "emit_gunner")
   (planted-watchtower "emit_sniper")
   (planted-watchtower "emit_crossfire")
   (planted-watchtower "emit_grenadiers" :distance 7 :extra 'YES-PLEASE)

   (inject (dune-with-saplings 10) "level_done_burn_mobs" 48)
   (empty 48)))
