(load "level.lisp")

(defparameter *desert-walkable*
  '(103 111 119 127 135 143 151 159
    163 171 179 187 195 203 211 219
    258 266 274 236 231 239 247 255
    263 271 279 287 285 260 261))

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
  (elt list (xor-random (length list))))

(defun random-decoration (flip)
  (tile (random-elt *decorations*) :h flip :v flip))

(defun decorate-side (x count &key (move 3) (flip 0))
  (when (> count 0)
    (s-poke x (+ move count) (random-decoration flip))
    (decorate-side x (1- count) :move move :flip flip)))

(defun shaded-bottom ()
  (place 0 1 (crop 0 0 8 4 (cliffs)) (crop 0 5 8 6 (cliffs))))

(defun shaded-ground (&key type)
  (case type
    (0 (shaded-bottom))
    (1 (crop 0 4 4 8 (cliffs)))
    (2 (crop 4 4 8 8 (cliffs)))))

(defun platform (&key (h 0))
  (s-push (shaded-ground :type 0))
  (s-place 0 0 (crust))
  (s-place 0 (+ h 4) (top-ground))
  (s-place -1 0 (ground :x1 7))
  (s-place 9 0 (ground :x1 7))
  (s-place 0 2 (desert-cell 233))
  (s-place 0 (+ h 5) (platform-edge :type 0 :flip t))
  (s-place 9 (+ h 5) (platform-edge :type 1))
  (s-place 2 4 (make 6 h :e (desert-tile 234)))
  (decorate-side 8 h :flip 1)
  (decorate-side 1 h)
  (s-pop))

(defun double-platform (&key (h 0) (n 0))
  (s-push (platform :h (+ h n 4)))
  (s-place 5 0 (shaded-ground :type 0))
  (s-place 9 (+ h 4) (top-ground :x1 4 :x2 8))
  (s-place 5 (+ h 4) (shaded-ground :type 1))
  (s-place 1 0 (shaded-ground :type 2))
  (s-place 4 (+ h 5) (platform-edge :type 2))
  (s-place 13 (+ h 5) (platform-edge :type 1))
  (s-place 6 4 (make 6 h :e (desert-tile 234)))
  (s-place 13 0 (ground :x1 7))
  (s-place 5 3 (desert-cell 241))
  (s-place 0 2 (desert-cell 242))
  (s-place 0 3 (desert-cell 0))
  (s-place 13 2 (desert-cell 242))
  (s-place 13 3 (desert-cell 0))
  (decorate-side 12 h :flip 1)
  (s-place 1 0 (crust :x2 4))
  (s-place 5 0 (crust))
  (s-pop))

(defun inv-double-platform ()
  (flip (double-platform :h 1 :n 1)))

(defun sandwich-platform (&key (h 0) (n 0))
  (s-push (platform :h (+ h n 4)))
  (s-place 0 0 (platform :h h))
  (s-place 1 (+ h 4) (shaded-ground :type 0))
  (s-pop))

(defun hole (width)
  (s-push (empty width))
  (s-place 0 1 (platform-edge :type 1))
  (s-place 0 0 (desert-cell (tile 281 :pr 1)))
  (s-place 1 0 (desert-cell (tile 282 :pr 1)))
  (s-place (1- width) 0 (desert-cell (tile 281 :pr 1 :h 1)))
  (s-place (- width 2) 0 (desert-cell (tile 282 :pr 1 :h 1)))
  (s-place (1- width) 1 (platform-edge :type 1 :flip t))
  (s-pop))

(defun hole-with-platform (&key (h 2))
  (incf h 4)
  (s-push (hole 14))
  (s-place 3 h (top-ground))
  (s-place 2 (1+ h) (platform-edge :type 0 :flip t))
  (s-place 11 (1+ h) (platform-edge :type 1))
  (s-place 4 0 (make 6 h :e (desert-tile 234)))
  (decorate-side 10 h :move -1 :flip 1)
  (decorate-side 3 h :move -1)
  (s-pop))

(defun cacti-stem (h &optional (stem 251) (cap 235) (variate 1))
  (if (< h 1)
      (desert-cell cap)
      (let ((tile (+ stem (logand h variate))))
	(place 0 1 (desert-cell tile) (cacti-stem (1- h) stem cap variate)))))

(defun cacti-base (h &optional (base 244))
  (place 0 1 (desert-cell base) (cacti-stem h)))

(defun cacti-branch (h &optional (pos-x 8) (offset 0))
  (s-push (crop pos-x 3 (+ pos-x 2) 4 (cliffs)))
  (s-place offset 1 (cacti-stem h 228 227 0))
  (s-pop))

(defun full-cacti (h b1p b1h b2p b2h &optional (base 244))
  (s-push (cacti-base h base))
  (s-place -1 b1p (cacti-branch b1h))
  (s-place 1 b2p (cacti-branch b2h 10 1))
  (when (and (= b1p b2p) (>= b1p 0))
    (s-place 1 b1p (desert-cell 243)))
  (s-pop))

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

(defun place-cacti (fn x y)
  (s-place x (1+ y) (apply fn (cacti-params y x))))

(defun cacti-garden (&optional (n 6))
  (setf *seed* 1940)
  (s-push (ground :n n))
  (loop for i from 0 to (- (* n 8) 8) by 6 do
    (place-cacti #'front-cacti (+ i 1) 0)
    (place-cacti #'back-cacti (+ i 4) 1))
  (s-pop))

(defun tripple-sandwich ()
  (s-push (platform :h 11))
  (s-place 0 0 (platform :h 1))
  (s-place 0 0 (platform :h 6))
  (s-place 1 5 (shaded-ground :type 0))
  (s-place 1 10 (shaded-ground :type 0))
  (s-pop))

(defun hanging-platform ()
  (place 0 0 (ground) (crop 0 1 8 2 (cliffs))))

(defun plateau-pillars (n)
  (join
   (tripple-sandwich)
   (hole (- (* 8 n) 2))
   (tripple-sandwich)))

(defun plateau (n)
  (s-push (plateau-pillars n))
  (dotimes (i n (s-pop))
    (s-place (+ 9 (* 8 i)) 15 (hanging-platform))))

(defun rusty-walkway (&optional (n 1))
  (multiply (crop 9 0 11 3 (cliffs)) n))

(defun desert-to-rusty ()
  (crop 8 0 9 3 (cliffs)))

(defun rusty-to-desert ()
  (crop 15 0 16 3 (cliffs)))

(defun rusty-platform-right (&optional (start 0))
  (crop 11 start 12 3 (cliffs)))

(defun rusty-platform-left (&optional (start 0))
  (crop 12 start 13 3 (cliffs)))

(defun rusty-hole (n)
  (join (rusty-platform-right) (empty n) (rusty-platform-left)))

(defun rusty-base (&optional (h 0))
  (on-top (stack (crop 13 3 15 4 (cliffs)) h)
	  (crop 9 0 11 3 (cliffs))))

(defun rusty-select-side (tile x)
  (if (= tile 280)
      (rusty-platform-left x)
      (rusty-platform-right x)))

(defun rusty-platform-side (tile h &optional base (x 0))
  (s-push (and base (desert-cell base)))
  (s-top (stack (desert-cell tile) h))
  (s-top (rusty-select-side tile x))
  (s-pop))

(defun base-cross (&optional (y 5))
  (let ((half (crop 13 y 15 (1+ y) (cliffs))))
    (on-top half (topple half))))

(defvar *disable-cross* nil)

(defun decorate-rusty (h n dx dy dt result)
  (let ((cross (base-cross dt)))
    (dotimes (i (1- n) result)
      (let ((y (1- (if (= h 0) 0 (xor-random h)))))
	(when (and (> h 1) (not (< y 0)) (not *disable-cross*))
	  (setf result (place (+ dx (* 2 i)) (+ dy y) result cross)))))))

(defun rusty-platform-middle (h n)
  (decorate-rusty h n 1 0 5 (multiply (rusty-base h) n)))

(defun rusty-base-platform (&optional (n 1) (h 0))
  (join (rusty-platform-side 280 h)
	(rusty-platform-middle h n)
	(rusty-platform-side 272 h)))

(defun rusty-over-platform-middle (&optional (n 1) (h 0))
  (s-push (crop 13 2 15 3 (cliffs)))
  (s-top (stack (crop 13 0 15 1 (cliffs)) h))
  (s-top (crop 13 1 15 2 (cliffs)))
  (s-top (crop 9 2 11 3 (cliffs)))
  (multiply (s-pop) n))

(defun over-cross ()
  (let ((half (crop 13 4 15 5 (cliffs))))
    (on-top half (topple half))))

(defun rusty-over-platform-decorated (&optional (n 1) (h 0))
  (decorate-rusty h n 0 1 4 (rusty-over-platform-middle n h)))

(defun rusty-over-platform (&optional (n 1) (h 0))
  (join (rusty-platform-side 280 h 278 1)
	(rusty-over-platform-decorated n h)
	(rusty-platform-side 272 h 270 1)))

(defun rusty-double-platform ()
  (s-push (rusty-base-platform 16 4))
  (s-place 2 6 (rusty-over-platform 14 4))
  (s-place 4 12 (rusty-over-platform 12 4))
  (s-pop))

(defun rusty-bridge-middle (n h)
  (on-top (stack (empty n) (1+ h)) (multiply (crop 15 3 16 5 (cliffs)) n)))

(defun rusty-stud-platform (n h &optional (x1 0) (x2 n) (c1 283) (c2 259))
  (s-push (rusty-over-platform n h))
  (s-place x1 (+ h 2) (desert-cell c1))
  (s-place (1+ x1) (+ h 2) (multiply (crop 13 2 15 3 (cliffs)) x2))
  (s-place (+ x1 (* 2 x2) 1) (+ h 2) (desert-cell c2))
  (s-pop))

(defun crop-bottom (box)
  (crop 0 1 (width box) (height box) box))

(defun martas-platformas (&optional (w 32))
  (let ((*disable-cross* t))
    (s-push (rusty-base-platform 8))
    (s-place 2 2 (rusty-over-platform 6 16))
    (s-place 6 2 (rusty-stud-platform 4 13))
    (s-place 2 2 (rusty-stud-platform 4 10))
    (s-place 6 2 (rusty-stud-platform 4 7))
    (s-place 2 2 (rusty-stud-platform 4 4))
    (s-place 6 2 (rusty-stud-platform 4 1))
    (s-place (+ w 14) 0 (crop-bottom (rusty-over-platform 4 19)))
    (s-place 15 18 (rusty-bridge-middle w 0))
    (s-place (+ w 14) 20 (desert-cell 246))
    (s-place (+ w 14) 19 (desert-cell 279))
    (s-pop)))

(defun rusty-cacti ()
  (s-push (ground :n 1))
  (s-place 3 2 (back-cacti 8 4 2 3 4))
  (s-place 1 2 (back-cacti 3 2 0 1 1))
  (s-pop))

(defun dab-cacti ()
  (s-push (ground :n 1))
  (s-place 4 1 (front-cacti 7 3 3 4 1))
  (s-place 1 1 (front-cacti 5 4 0 3 2))
  (s-pop))

(defun long-cacti ()
  (s-push (ground :n 1))
  (s-place 4 2 (back-cacti 15 12 1 9 4))
  (s-place 2 1 (front-cacti 11 7 2 8 2))
  (s-pop))

(defun rusty-dirt-with-cacti (&optional (fn #'rusty-cacti))
  (join
   (empty 3)
   (rusty-platform-left)
   (rusty-walkway 1)
   (rusty-to-desert)
   (funcall fn)
   (desert-to-rusty)
   (rusty-walkway 1)
   (rusty-platform-right)
   (empty 3)))

(defun rusty-bridge-base (n h)
  (join (rusty-base-platform 1 h)
	(rusty-bridge-middle n h)
	(rusty-base-platform 1 h)))

(defun rusty-bridge (&optional (n 4) (h 0))
  (s-push (rusty-bridge-base n h))
  (s-place (+ n 4) (+ h 0) (desert-cell 280))
  (s-place (+ n 4) (+ h 1) (desert-cell 261))
  (s-place (+ n 4) (+ h 2) (desert-cell 246))
  (s-place 3 (+ h 0) (desert-cell 272))
  (s-place 3 (+ h 1) (desert-cell 260))
  (s-place 3 (+ h 2) (desert-cell 238))
  (s-pop))

(defun bridge-support ()
  (s-push (rusty-over-platform 1 1))
  (s-place 0 3 (desert-cell 246))
  (s-place 0 2 (desert-cell 279))
  (s-place 0 0 (desert-cell 283))
  (s-pop))

(defun rusty-down-stairs ()
  (s-push (empty 1))
  (s-join (rusty-bridge 3 15))
  (s-place 1 17 (bridge-support))
  (loop for x from 8 by 7 for h from 12 downto 0 by 3 do
    (s-place x 0 (rusty-bridge 3 h))
    (s-place x (+ h 2) (bridge-support)))
  (s-pop))

(defparameter *jumps*
  '(((2 0) 2) ((1 1) 1) ((2 0) 2) ((1 1) 3)
    ((1 0) 2) ((1 3) 2)
    ((1 1) 2) ((1 4) 3)
    ((1 2) 2) ((1 5) 3)
    ((2 4) 1) ((1 1) 2) ((2 4) 1) ((1 1) 3)
    ((1 1) 2) ((1 3) 2) ((1 5) 2)
    ((1 1) 2) ((1 3) 2) ((1 5) 2)
    ((2 6) 1) ((1 2) 3) ((1 2) 2)
    ((2 5) 1) ((1 1) 3) ((1 1) 2)
    ((2 4) 1) ((1 0) 3) ((1 0) 0)))

(defun one-rusty-jump (jump)
  (join (apply #'rusty-base-platform (first jump))
	(empty (second jump))))

(defun rusty-jumps ()
  (let ((result nil))
    (dolist (x *jumps* result)
      (setf result (join result (one-rusty-jump x))))))

(defun dirt-n-rust-segment (x)
  (join (rusty-walkway 1) (rusty-to-desert) x (desert-to-rusty)))

(defun dirt-n-rust-decorations ()
  (list (ground :x1 4) (aloe) (ground :x2 4) (bush) (ground)))

(defun dirt-n-rust-middle ()
  (let ((middle nil))
    (dolist (x (dirt-n-rust-decorations) middle)
      (setf middle (join middle (dirt-n-rust-segment x))))))

(defun alternate-dirt-n-rust ()
  (join
   (rusty-platform-left)
   (dirt-n-rust-middle)
   (rusty-walkway 1)
   (rusty-platform-right)))

(defun desert-level ()
  (setf *seed* 1943)
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
	(inject (inv-double-platform) "emit_hopper_squad" 4)
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

(defun add-down-stairs (martas)
  (s-push martas)
  (s-place (- (width martas) 5) 0 (rusty-down-stairs))
  (s-inject "emit_down_stair_guards" (width martas))
  (s-pop))

(defun rusty-level ()
  (setf *seed* 1914)
  (join (rusty-base-platform 12)

	;; start
	(rusty-dirt-with-cacti)

	;; Martas platformas
	(trigger "emit_marta_platform_patrollers")
	(add-down-stairs (martas-platformas))
	(rusty-dirt-with-cacti #'dab-cacti)

	;; swarm chase
	(inject (rusty-jumps) "emit_chasing_swarm" 24)
	(rusty-dirt-with-cacti #'long-cacti)
	(trigger "ignite_swarm")

	;; ending
	(inject (alternate-dirt-n-rust) "level_done" 38)

	(empty 64)))

(defun mantis-stairs ()
  (s-push (empty 1))
  (s-place 0 2 (rusty-over-platform 5 3))
  (s-place 2 7 (rusty-over-platform 3 3))
  (s-place 4 12 (rusty-over-platform 1 3))
  (s-pop))

(defun mantis-ground ()
  (join (rusty-walkway 11)
	(rusty-to-desert)
	(cacti) (ground :x2 2) (aloe)
	(desert-to-rusty)
	(rusty-walkway 15)))

(defun mantis-level ()
  (setf *seed* 1918)
  (s-push (mantis-ground))
  (s-place 9 0 (mantis-stairs))
  (s-place 35 0 (mantis-stairs))
  (s-place 16 15 (rusty-bridge-middle 24 0))
  (s-place 39 17 (desert-cell 246))
  (s-place 39 16 (desert-cell 279))
  (s-pop))

(defun commit-save ()
  (push-level "desert_level" (desert-level))
  (push-level "mantis_level" (mantis-level))
  (push-level "rusty_level" (rusty-level))
  (save-level "desert.inc" *desert-walkable*))
