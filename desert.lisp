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

(defun decorate-column (pipe x y &key flip)
  (poke pipe x (+ y 4) (tile (random-elt *decorations*) :h flip :v flip)))

(defun decorate-side (pipe x height &key (move 0) (flip 0))
  (dotimes (y height pipe)
    (decorate-column pipe x (+ y move) :flip flip)))

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

(defun desert-level ()
  (join (aloe)
	(ground :x2 2)
	(cacti)
	(ground :x2 4)
	(platform :h 3)
	(ground)
	(double-platform :h 5 :n 3)
	(ground :n 3)
	(bush)
	(empty 16)))
