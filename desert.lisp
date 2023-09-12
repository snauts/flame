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

(defun edge (type)
  (case type
    (0 (crop 8 1 9 3 (cliffs)))
    (1 (crop 8 5 9 7 (cliffs)))
    (2 (crop 9 4 10 6 (cliffs)))))

(defun platform-edge (&key (type 0) flip)
  (funcall (if (null flip) #'identity #'flip-horizontally) (edge type)))

(defparameter *decorations*
  (list (tile 225 :pl 1)
	(tile 228 :pl 1)
	(tile 229 :pl 1)
	(tile 232 :pl 1)))

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
   (place 0 (+ h 4) (shaded-ground :type 0) (ground))
   (place -1 0 pipe (ground :x1 7))
   (place 0 2 pipe (crop 9 7 10 8 (cliffs)))
   (place 0 (+ h 5) pipe (platform-edge :type 0 :flip t))
   (place 9 0 pipe (ground :x1 7))
   (place 9 (+ h 5) pipe (platform-edge :type 1))
   (place 2 4 pipe (make 6 h :e (tile 234 :pl 1)))
   (decorate-side pipe 1 h)
   (decorate-side pipe 8 h :flip 1)))

(defun double-platform (&key (h 0) (n 0))
  (box-pipe
   (place 0 (+ h 4) (shaded-ground :type 0) (shaded-ground :type 1))
   (place 4 (+ h 4) pipe (ground :x1 4 :x2 8))
   (place -4 0 pipe (shaded-ground :type 2))
   (place 4 3 pipe (crop 9 3 10 4 (cliffs)))
   (place 3 (+ h 5) pipe (platform-edge :type 2))
   (place 12 (+ h 5) pipe (platform-edge :type 1))
   (place 12 0 pipe (ground :x1 7))
   (place 5 4 pipe (make 6 h :e (tile 234 :pl 1)))
   (place 4 4 pipe (make 1 h :e (tile 234 :pl 1)))
   (place 3 4 pipe (make 1 (+ h 1) :e (tile 234 :pl 1)))
   (place 3 (+ h 7) pipe (singleton 234 :pl 1))
   (place 1 4 pipe (make 2 (+ h 4) :e (tile 234 :pl 1)))
   (place 1 (+ h 8) pipe (make 6 n :e (tile 234 :pl 1)))
   (place 0 (+ h n 8) pipe (ground))
   (place 8 (+ h n 9) pipe (platform-edge :type 1))
   (place -1 (+ h n 9) pipe (platform-edge :type 0 :flip t))
   (place 0 0 pipe (ground :x1 7))
   (place 0 2 pipe (singleton 238 :pl 1))
   (place 0 3 pipe (singleton 0 :pl 1))
   (place 13 2 pipe (flip-horizontally (singleton 238 :pl 1)))
   (place 13 3 pipe (singleton 0 :pl 1))
   (decorate-side pipe 1 (+ h n 4))
   (decorate-side pipe 8 n :flip 1 :move (+ h 4))
   (decorate-side pipe 12 h :flip 1)))

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
