(defparameter *desert-walkable*
  '(103 111 119 127 135 143 151 159
    163 171 179 187 195 203 211 219
    258 266 274 236 231 239 247 255
    263 271 279 287))

(defun desert-tile (id &optional (pr 0))
  (tile id :pl 1 :pr pr))

(defun desert-cell (id &optional (pr 0))
  (make 1 1 :e (desert-tile id pr)))

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

(defun shaded-bottom ()
  (place 0 1 (crop 0 0 8 4 (cliffs)) (crop 0 5 8 6 (cliffs))))

(defun shaded-ground (&key type)
  (case type
    (0 (shaded-bottom))
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
   (empty width)
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

(defun cacti-stem (h &optional (stem 251) (cap 235) (variate 1))
  (if (< h 1)
      (desert-cell cap)
      (let ((tile (+ stem (logand h variate))))
	(place 0 1 (desert-cell tile) (cacti-stem (1- h) stem cap variate)))))

(defun cacti-base (h &optional (base 244))
  (place 0 1 (desert-cell base) (cacti-stem h)))

(defun cacti-branch (h &optional (pos-x 8) (offset 0))
  (box-pipe
   (crop pos-x 3 (+ pos-x 2) 4 (cliffs))
   (place offset 1 pipe (cacti-stem h 228 227 0))))

(defun full-cacti (h b1p b1h b2p b2h &optional (base 244))
  (box-pipe
   (cacti-base h base)
   (place -1 b1p pipe (cacti-branch b1h))
   (place 1 b2p pipe (cacti-branch b2h 10 1))
   (if (and (= b1p b2p) (>= b1p 0))
       (place 1 b1p pipe (desert-cell 243))
       pipe)))

(defun front-cacti (h b1p b1h b2p b2h)
  (forward (full-cacti h b1p b1h b2p b2h 236)))

(defun back-cacti (h b1p b1h b2p b2h)
  (full-cacti h b1p b1h b2p b2h))

(defun cacti-height (i &optional (base 10))
  (round (+ base (* 2 (sin (* 0.25 i))))))

(defun cacti-variation (i h)
  (setf *seed* (* 1942 i))
  (+ (floor h 2) (xor-random 3) -2))

(defun cacti-params (n i)
  (let* ((h0 (cacti-height i))
	 (h1 (cacti-height (1- i)))
	 (h2 (cacti-height (1+ i)))
	 (b1 (cacti-variation (1- i) h1))
	 (b2 (cacti-variation (1+ i) h2)))
    (cond ((= n 0) (list h0 3 (- b1 2) 3 (- b2 2)))
	  ((= n 1) (list h0 (+ 2 b1) (- h1 b1 4) (+ 2 b2) (- h2 b2 4))))))

(defun place-cacti (garden fn x y)
  (place x (1+ y) garden (apply fn (cacti-params y x))))

(defun cacti-garden (&optional (n 6))
  (setf *seed* 1940)
  (let* ((garden (ground :n n)))
    (loop for i from 0 to (- (* n 8) 8) by 6 do
      (with-box garden
	(place-cacti garden #'front-cacti (+ i 1) 0)
	(place-cacti garden #'back-cacti (+ i 4) 1)))
    garden))

(defun tripple-sandwich ()
  (box-pipe
   (platform :h 11)
   (place 0 0 pipe (platform :h 1))
   (place 0 0 pipe (platform :h 6))
   (place 1 5  pipe (shaded-ground :type 0))
   (place 1 10 pipe (shaded-ground :type 0))))

(defun hanging-platform ()
  (place 0 0 (ground) (crop 0 1 8 2 (cliffs))))

(defun plateau-pillars (n)
  (join
   (tripple-sandwich)
   (hole (- (* 8 n) 2))
   (tripple-sandwich)))

(defun plateau (n)
  (let ((pillars (plateau-pillars n)))
    (dotimes (i n pillars)
      (with-box pillars
	(place (+ 9 (* 8 i)) 15 pillars (hanging-platform))))))

(defun rusty-walkway (&optional (n 1))
  (multiply (crop 9 0 11 3 (cliffs)) n))

(defun desert-to-rusty ()
  (crop 8 0 9 3 (cliffs)))

(defun rusty-to-desert ()
  (crop 15 0 16 3 (cliffs)))

(defun rusty-end ()
  (crop 11 0 12 3 (cliffs)))

(defun rusty-start ()
  (crop 12 0 13 3 (cliffs)))

(defun rusty-hole (n)
  (join (rusty-end)
	(empty n)
	(rusty-start)))

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
	(ground :x1 4)
	(hole 4)
	(ground :x2 4)
	(trigger "emit_hole_hoppers")
	(hole 4)
	(ground)

	;; platforms over bigger hole
	(aloe)
	(ground)
	(cacti)
	(flip (double-platform :h 1 :n 1))
	(hole-with-platform :h 10)
	(trigger "emit_sky_hoppers")
	(double-platform :h 1 :n 1)

	;; garden
	(multiply (bush) 3)
	(inject (cacti-garden) "emit_hopper_stream" 24)

	;; plateau
	(ground :n 3)
	(trigger "emit_plateau_patrollers")
	(inject (plateau 6) "emit_chasing_hoppers" 50)
	(ground :x2 2)
	(cacti)
	(hole 6)

	;; to next level
	(ground :n 4)
	(desert-to-rusty)
	(rusty-walkway 12)
	(trigger "level_done")
	(rusty-walkway 2)
	(empty 32)))

(defun rusty-level ()
  (join (rusty-walkway 8)
	(rusty-to-desert)
	(ground :n 1)
	(desert-to-rusty)
	(rusty-walkway 1)
	(rusty-hole 4)
	(rusty-walkway 4)
	(rusty-end)
	(empty 64)))
