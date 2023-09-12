(defun desert ()
  (fill-box 8 8 (tile 97 :pl 1)))

(defun cliffs ()
  (fill-box 10 8 (tile 161 :pl 1)))

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

(defun platform-edge (&key type flip)
  (let ((edge (crop 8 (+ 1 (* type 4)) 9 (+ 3 (* type 4)) (cliffs))))
    (if (null flip) edge (flip-horizontally edge))))

(defparameter *decorations*
  (list (tile 225 :pl 1)
	(tile 228 :pl 1)
	(tile 229 :pl 1)
	(tile 232 :pl 1)))

(defun random-elt (list)
  (elt list (random (length list))))

(defun decorate-column (pipe x y &key flip)
  (poke pipe x (+ y 4) (tile (random-elt *decorations*) :h flip :v flip)))

(defun decorate-sides (pipe h)
  (dotimes (y h)
    (decorate-column pipe 1 y :flip 0)
    (decorate-column pipe 8 y :flip 1))
  pipe)

(defun platform (&key (h 0))
  (box-pipe
   (place 0 (+ h 4) (crop 0 0 8 4 (cliffs)) (ground))
   (place -1 0 pipe (ground :x1 7))
   (place 0 2 pipe (crop 9 7 10 8 (cliffs)))
   (place 0 (+ h 5) pipe (platform-edge :type 0 :flip t))
   (place 9 0 pipe (ground :x1 7))
   (place 9 (+ h 5) pipe (platform-edge :type 1))
   (place 2 4 pipe (make 6 h :e (tile 234 :pl 1)))
   (decorate-sides pipe h)))

(defun desert-level ()
  (join (aloe)
	(ground :x2 2)
	(cacti)
	(ground :n 3)
	(platform :h 10)
	(ground :n 3)
	(bush)
	(empty 16)))
