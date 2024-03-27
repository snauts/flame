(load "level.lisp")

(defparameter *town-walkable*
  '(130 138 146 154 162 170 136 144 152))

(defun street-tile (id &key (v 0) (h 0) (pr 0))
  (tile id :pl 1 :v v :h h :pr pr))

(defun street-cell (id &key (v 0) (h 0) (pr 0))
  (cell (street-tile id :v v :h h :pr pr)))

(defun street (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (street-tile 129))))

(defun walk-middle (n)
  (multiply (street 1 5 5 8) n))

(defun town-walk (n)
  (join (street 0 5 1 8) (walk-middle n) (street 5 5 6 8)))

(defun random-cell (&rest choices)
  (street-cell (elt choices (xor-random (length choices)))))

(defun bottom-boulder ()
  (join (random-cell 142 158) (random-cell 150 166)))

(defun wall-bottom-top (n)
  (s-push nil)
  (dotimes (i n (s-pop))
    (s-join (bottom-boulder))))

(defun wall-bottom-middle (w)
  (on-top (multiply (street 1 1 5 2) w) (wall-bottom-top (* 2 w))))

(defun wall-bottom (w)
  (join (street 0 1 1 3) (wall-bottom-middle w) (street 5 1 8 3)))

(defun small-brick ()
  (if (= 0 (xor-random 2))
      (street-cell 141)
      (street-cell 165)))

(defun large-brick-left ()
  (case (xor-random 3)
    (0 (street-cell 140))
    (1 (street-cell 156))
    (2 (street-cell 149))))

(defun large-brick-right ()
  (case (xor-random 3)
    (0 (street-cell 148))
    (1 (street-cell 164))
    (2 (street-cell 157))))

(defun large-brick ()
  (join (large-brick-left)
	(large-brick-right)))

(defun large-brick-row (n)
  (s-push nil)
  (dotimes (i n (s-pop))
    (s-join (large-brick))))

(defun brick-bottom-row (n)
  (join (small-brick) (large-brick-row n) (small-brick)))

(defun brick-row (n)
  (on-top (brick-bottom-row (1- n)) (large-brick-row n)))

(defun wall-row (w)
  (join (street 0 3 1 5) (brick-row (* 2 w)) (street 5 3 7 5)))

(defun stack-cells (&rest cells)
  (reduce #'on-top (mapcar #'street-cell cells)))

(defun shingles (n)
  (let ((middle (multiply (stack-cells 144 168) n)))
    (join (stack-cells 136 160) middle (stack-cells 152 176))))

(defun town-wall-top (w h)
  (s-push nil)
  (loop for y from 0 to (* 2 (1- h)) by 2 do
    (s-place 0 y (wall-row w)))
  (s-place 0 (* 2 h) (shingles (1+ (* 4 w))))
  (s-pop))

(defun town-wall (w h)
  (s-push (wall-bottom w))
  (s-place 0 2 (town-wall-top w h))
  (s-pop))

(defun lamp-post (&key (base-h 3) (post-h 6) walkable forward brick)
  (s-push (when (numberp brick) (street-cell brick)))
  (s-place-top 0 (stack (street-cell 179) base-h))
  (when (numberp walkable)
    (s-place 0 walkable (street-cell (set-walkable 179))))
  (s-place-top 0 (street-cell 178))
  (s-place-top 0 (stack (street-cell 177) post-h))
  (s-place-top -1 (street 7 5 10 6))
  (let ((shine (palette 0 (street 7 6 10 7))))
    (s-place-top 0 shine)
    (s-place-top 0 (topple shine)))
  (s-place-top 0 (street 7 7 10 8))
  (if forward (forward (s-pop)) (s-pop)))

(defun window-row (w)
  (join (street-cell 192) (multiply (street-cell 189) w) (street-cell 192)))

(defun brick-window (w h)
  (s-push (window-row w))
  (s-place 0 1 (stack (street-cell 188) h))
  (s-place (1+ w) 1 (stack (street-cell 188) h))
  (s-place-top 0 (window-row w))
  (let ((dark (stack (cell (tile 1 :pl 0)) h)))
    (dotimes (i w (s-pop))
      (s-place (1+ i) 1 dark))))

(defun simple-house ()
  (s-push (town-walk 6))
  (s-place 5 2 (town-wall 3 4))
  (s-place 10 5 (brick-window 2 3))
  (loop for x from 1 to 21 by 20 do
    (s-place x 0 (lamp-post :base-h 4 :walkable 1 :forward t)))
  (s-inject "emit_rat" 16)
  (s-pop))

(defun brick-fence ()
  (s-push (town-walk 6))
  (s-place 5 2 (town-wall 3 1))
  (loop for x from 1 to 21 by 20 do
    (s-place x 0 (lamp-post :base-h 4 :walkable 1 :forward t)))
  (s-inject "emit_dumpster_wave" 18)
  (s-pop))

(defun wall-join (w)
  (join (street-cell 200) (multiply (street 9 0 11 1) w) (street 11 0 13 1)))

(defun brick-bridge-pillar (h1 h2)
  (s-push (town-walk 6))
  (s-place 4 2 (town-wall 1 h1))
  (s-place 12 2 (town-wall 1 h2))
  (s-place 2 2 (town-wall 4 1))
  (s-place 4 7 (wall-join 2))
  (s-place 12 7 (wall-join 2))
  (s-pop))

(defun brick-column (w h)
  (lower (town-wall w h) 2))

(defun brick-bridge-base ()
  (join (brick-bridge-pillar 3 5)
	(empty 2)
	(brick-column 1 7)
	(empty 8)
	(brick-column 1 7)
	(empty 4)
	(brick-bridge-pillar 5 3)))

(defun brick-bridge ()
  (s-push (brick-bridge-base))
  (s-place 12 14 (shingles 53))
  (loop for x from 14 to 62 by 16 do
    (s-place x 15 (lamp-post :base-h 2 :post-h 2 :brick 240)))
  (s-inject "emit_bridge_alpha" 12)
  (s-inject "emit_bridge_beta" 28)
  (s-inject "emit_bridge_gamma" 51)
  (s-inject "emit_bridge_kappa" 67)
  (s-inject "emit_bridge_delta" 82)
  (s-pop))

(defun big-house ()
  (s-push (town-wall 4 12))
  (loop for y from 3 to 19 by 8 do
    (s-place 3 y (brick-window 3 4))
    (s-place 10 y (brick-window 3 4)))
  (s-pop))

(defun window-bottles ()
  (street 8 1 10 3))

(defun house-block ()
  (s-push (town-walk 13))
  (s-place 5 2 (big-house))
  (s-place 29 2 (big-house))
  (s-place 34 14 (window-bottles))
  (s-place 16 6 (window-bottles))
  (s-place 1 2 (lamp-post :base-h 2 :brick 184))
  (s-place 25 2 (lamp-post :base-h 2 :brick 184))
  (s-place 49 2 (lamp-post :base-h 2 :brick 184))
  (s-inject "emit_house_block_rat_delta" 48)
  (s-inject "emit_house_block_rat_charlie" 36)
  (s-inject "emit_house_block_rat_bravo" 30)
  (s-inject "emit_house_block_rat" 14)
  (s-pop))

(defun arch-pillar ()
  (s-push (town-wall 2 8))
  (s-place 2 3 (brick-window 4 4))
  (s-pop))

(defun arch-house ()
  (s-push (town-walk 13))
  (s-place 5 2 (arch-pillar))
  (s-place 37 2 (arch-pillar))
  (s-place 5 12 (town-wall-top 10 4))
  (s-place 18 2 (town-wall 1 1))
  (s-place 28 2 (town-wall 1 1))
  (loop for x from 7 to 40 by 8 do
    (s-place x 13 (brick-window 4 4)))
  (s-place 8 14 (window-bottles))
  (s-place 10 14 (window-bottles))
  (s-place 1 2 (lamp-post :base-h 2 :brick 184))
  (s-place 49 2 (lamp-post :base-h 2 :brick 184))
  (s-inject "emit_arch_alpha_wave" 30)
  (s-inject "emit_arch_bravo_wave" 38)
  (s-inject "emit_arch_charlie_wave" 46)
  (s-inject "emit_reverse_wave" 16)
  (s-pop))

(defun pillars-with-lamps ()
  (s-push (brick-column 1 3))
  (s-place 10 0 (brick-column 1 2))
  (s-place -10 0 (brick-column 1 2))
  (s-place 7 0 (lamp-post :base-h 8 :post-h 6 :forward t))
  (s-place 17 0 (lamp-post :base-h 8 :post-h 6 :forward t))
  (s-inject "emit_pillar_bravo" 26)
  (s-inject "emit_pillar_alpha" 16)
  (s-pop))

(defun simple-shed ()
  (join (brick-column 2 2) (empty 1) (simple-house)))

(defun town-level ()
  (setf *seed* (* 1815 06 18))
  (join (town-walk 8)
	(empty 2)
	(simple-shed)
	(empty 2)
	(house-block)
	(empty 2)
	(brick-fence)
	(empty 2)
	(arch-house)
	(empty 2)
	(pillars-with-lamps)
	(empty 1)
	(brick-bridge)
	(empty 2)

	;; level done
	(inject  (town-walk 12) "level_done_burn_mobs" 32)
	(empty 48)))

(defun ramp-stage (h &key next)
  (s-push nil)
  (let ((half (floor h 2)))
    (loop for x from 12 to 36 by 12 do
      (s-place x 0 (brick-column 1 half)))
    (when next (s-place 50 0 (brick-column 1 (+ 2 half))))
    (s-place 48 0 (brick-column 2 half))
    (s-place 0 h (shingles 57))
    (when next (s-place 50 (1+ h) (wall-join 2)))
    (s-place 2 (1+ h) (lamp-post :base-h 1 :post-h 0 :brick 240))
    (s-pop)))

(defun finish-ramp ()
  (s-push nil)
  (loop for x from 0 to 36 by 12 do
    (s-place x 0 (brick-column 1 10)))
  (s-place 0 20 (shingles 48))
  (loop for x from 0 to 36 by 12 do
    (s-place (+ x 2) 21 (lamp-post :base-h 1 :post-h 0 :brick 240)))
  (join (empty 2) (s-pop)))

(defun ramp-level ()
  (setf *seed* 1789)
  (s-push (town-walk 8))
  (s-place 24 2 (town-wall 1 0))
  (loop for x from 24 by 50 for y from 4 to 20 by 4 do
    (s-place x 0 (ramp-stage y :next (/= y 20))))
  (s-join (inject (finish-ramp) "level_done_burn_mobs" 34))
  (s-join (empty 48))
  (s-pop))

(defun king-level ()
  (setf *seed* 1715)
  (s-push (town-walk 16))
  (s-pop))

(defun commit-save ()
  (push-level "town_level" (town-level))
  (push-level "ramp_level" (ramp-level))
  (push-level "king_level" (king-level))
  (save-level "town.inc" *town-walkable*))
