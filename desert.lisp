(defparameter *desert-walkable*
  '(103 111 119 127 135 143 151 159
    167 175 183 191 199 207 215 223
    163 171 179 187 195 203 211 219
    258 266 274))

(defun desert-tile (id)
  (tile id :pl 1))

(defun desert-cell (id)
  (make 1 1 :e (desert-tile id)))

(defun desert ()
  (fill-box 8 8 (desert-tile 97)))

(defun cliffs ()
  (fill-box 16 8 (desert-tile 161)))

(defun plant (x y w h)
  (let ((plant (crop x y (+ x w) (+ y h) (desert)))
	(under (crop x 0 (+ x w) 2 (desert))))
    (place 0 2 under plant)))

(defun bush ()
  (plant 2 4 6 2))

(defun aloe ()
  (plant 2 6 6 2))

(defun cacti ()
  (plant 0 4 2 4))

(defun ground (&key (x1 0) (x2 8) (n 1))
  (multiply (crop x1 0 x2 4 (desert)) n))

(defun top-ground (&key (x1 0) (x2 8))
  (place 0 0 (ground :x1 x1 :x2 x2) (crop x1 0 x2 1 (cliffs))))

(defun crust (&key (x1 0) (x2 8))
  (crop x1 0 x2 1 (desert)))

(defun edge (type)
  (case type
    (0 (crop 14 6 15 8 (cliffs)))
    (1 (crop 13 6 14 8 (cliffs)))
    (2 (crop 12 6 13 8 (cliffs)))))

(defun platform-edge (&key (type 0) flip)
  (funcall (if (null flip) #'identity #'flip) (edge type)))

(defparameter *decorations*
  (mapcar #'desert-tile '(225 226 249 250)))

(defun random-elt (list)
  (elt list (random (length list))))

(defun random-decoration (flip)
  (tile (random-elt *decorations*) :h flip :v flip))

(defun decorate-side (pipe x count &key (move 3) (flip 0))
  (cond ((= count 0) pipe)
	(t (let ((done (poke pipe x (+ move count) (random-decoration flip))))
	     (decorate-side done  x (1- count) :move move :flip flip)))))

(defun shaded-ground (&key type)
  (case type
    (0 (crop 0 0 8 4 (cliffs)))
    (1 (crop 0 4 4 8 (cliffs)))
    (2 (crop 4 4 8 8 (cliffs)))))

(defun platform (&key (h 0))
  (box-pipe
   (shaded-ground :type 0)
   (place 0 0 pipe (crust))
   (place 0 (+ h 4) pipe (top-ground))
   (place -1 0 pipe (ground :x1 7))
   (place 9 0 pipe (ground :x1 7))
   (place 0 2 pipe (desert-cell 233))
   (place 0 (+ h 5) pipe (platform-edge :type 0 :flip t))
   (place 9 (+ h 5) pipe (platform-edge :type 1))
   (place 2 4 pipe (make 6 h :e (desert-tile 234)))
   (decorate-side pipe 8 h :flip 1)
   (decorate-side pipe 1 h)))

(defun double-platform (&key (h 0) (n 0))
  (box-pipe
   (platform :h (+ h n 4))
   (place 5 0 pipe (shaded-ground :type 0))
   (place 9 (+ h 4) pipe (top-ground :x1 4 :x2 8))
   (place 5 (+ h 4) pipe (shaded-ground :type 1))
   (place 1 0 pipe (shaded-ground :type 2))
   (place 4 (+ h 5) pipe (platform-edge :type 2))
   (place 13 (+ h 5) pipe (platform-edge :type 1))
   (place 6 4 pipe (make 6 h :e (desert-tile 234)))
   (place 13 0 pipe (ground :x1 7))
   (place 5 3 pipe (desert-cell 241))
   (place 0 2 pipe (desert-cell 242))
   (place 0 3 pipe (desert-cell 0))
   (place 13 2 pipe (desert-cell 242))
   (place 13 3 pipe (desert-cell 0))
   (decorate-side pipe 12 h :flip 1)
   (place 1 0 pipe (crust :x2 4))
   (place 5 0 pipe (crust))))

(defun sandwich-platform (&key (h 0) (n 0))
  (box-pipe
   (place 0 0 (platform :h (+ h n 4)) (platform :h h))
   (place 1 (+ h 4) pipe (shaded-ground :type 0))))

(defun hole (width)
  (box-pipe
   (place 0 1 pipe (platform-edge :type 1))
   (place 0 0 pipe (desert-cell (tile 281 :pr 1)))
   (place 1 0 pipe (desert-cell (tile 282 :pr 1)))
   (place (1- width) 0 pipe (desert-cell (tile 281 :pr 1 :h 1)))
   (place (- width 2) 0 pipe (desert-cell (tile 282 :pr 1 :h 1)))
   (place (1- width) 1 pipe (platform-edge :type 1 :flip t))))

(defun hole-with-platform (&key (h 2))
  (incf h 4)
  (box-pipe
   (hole 14)
   (place 3 h pipe (top-ground))
   (place 2 (1+ h) pipe (platform-edge :type 0 :flip t))
   (place 11 (1+ h) pipe (platform-edge :type 1))
   (place 4 0 pipe (make 6 h :e (desert-tile 234)))
   (decorate-side pipe 10 h :move -1 :flip 1)
   (decorate-side pipe 3 h :move -1)))

(defun desert-level ()
  (join (aloe) ;; reference
	(ground :x2 2)

	;; start
	(cacti)
	(ground :x2 4)
	(aloe)
	(ground :x2 4)
	(bush)
	(ground :x2 8)
	(flip (aloe))
	(ground :x2 4)
	(trigger "emit_hopper_squad")

	;; holes
	(ground)
	(hole 4)
	(ground)
	(hole 4)
	(ground)
	(trigger "emit_hole_hoppers")
	(hole 4)
	(ground)

	;; sandbox
	(ground :n 2)
	(platform :h 3)
	(hole 4)
	(ground)
	(double-platform :h 4 :n 2)
	(sandwich-platform :h 1 :n 2)
	(ground :n 5)
	(bush)
	(empty 16)))
