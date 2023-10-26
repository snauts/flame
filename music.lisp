(defparameter *notes*
  '((C  0) (Cs 1) (D  2)  (Ds 3)
    (E  4) (F  5) (Fs 6)  (G  7)
    (Gs 8) (A  9) (As 10) (B 10)))

(defparameter *johnny*
  '((2 (0 0 D))
    (2 (0 0 G)  (4 0 D) (5 0 G))
    (2 (0 0 G))
    (2 (0 0 G))
    (4 (0 0 G))
    (2 (0 0 A))
    (4 (0 0 As) (4 0 D) (5 0 G))
    (2 (0 0 A))
    (4 (0 0 As))
    (2 (0 0 G))
    (6 (0 0 F)  (4 0 D) (5 0 F))
    (4                  (5 0 F))
    (2 (0 0 D))
    (6 (0 0 F)  (4 0 D) (5 0 F))
    (4                  (5 0 F))
    (2 (0 0 D))
    (2 (0 0 G)  (4 0 D) (5 0 G))
    (2 (0 0 G))
    (2 (0 0 G))
    (4 (0 0 G))
    (2 (0 0 A))
    (4 (0 0 As) (4 0 D) (5 0 G))
    (2 (0 0 A))
    (4 (0 0 As))
    (2 (0 1 C))
    (6 (0 1 D)  (4 0 D) (5 0 F))
    (4                  (5 0 F))
    (2 (0 0 As))
    (6 (0 1 D)  (4 0 D) (5 0 F))
    (2                  (5 0 F))
    (2 (0 0 As))
    (2 (0 1 C))
    (4 (0 1 D)  (4 0 D) (5 0 F))
    (2 (0 1 D))
    (2 (0 1 D))
    (2 (0 1 C))
    (2 (0 0 As))
    (4 (0 1 C)  (4 0 C) (5 0 Ds))
    (2 (0 1 C))
    (2 (0 1 C))
    (2 (0 0 As))
    (2 (0 0 A))
    (4 (0 0 As))
    (2 (0 0 As))
    (2 (0 0 As))
    (2 (0 0 A))
    (2 (0 0 G))
    (4 (0 0 A))
    (2 (0 0 A))
    (2 (0 0 A))
    (2 (0 0 As))
    (2 (0 1 C))
    (6 (0 1 D))
    (6 (0 1 C))
    (6 (0 0 As))
    (6 (0 0 A))
    (2 (0 0 G))
    (2 (0 0 G))
    (2 (0 0 G))
    (4 (0 0 G))
    (2 (0 0 F))
    (10(0 0 G))))

(defun drum-roll ()
  (copy-tree
   '((2 (2 0 G))
     (2 (2 0 G))
     (2 (2 0 G))
     (2 (2 0 G))
     (2 (2 0 G))
     (20 (2 0 G))
     (10 (2 0 G) (6 0 F))
     (20 (2 0 G) (6 0 F)))))

(defun drum-beat ()
  (copy-tree
   '((10 (2 0 G))
     (20 (2 0 G))
     (10 (2 0 G) (6 0 F))
     (20 (2 0 G) (6 0 F)))))

(defun drum-score ()
  (append
   (drum-roll)
   (drum-roll)
   (drum-beat)
   (drum-beat)
   (drum-roll)
   (drum-roll)
   (drum-beat)
   (drum-beat)
   (drum-roll)
   (drum-roll)
   (drum-roll)
   (drum-roll)
   (drum-beat)
   (drum-beat)
   (drum-roll)
   (drum-beat)))

(defun save-bytes (out bytes fmt)
  (let ((count 0))
    (dolist (b bytes)
      (format out fmt b)
      (when (>= (incf count) 8)
	(format out "~%")
	(setf count 0)))
    (when (/= count 0)
      (format out "~%"))))

(defun save-array (out name bytes &optional (type "byte") (fmt "0x~2,'0X, "))
  (format out "const ~A ~A[] = {~%" type name)
  (save-bytes out bytes fmt)
  (format out "};~%"))

(defun bit-n (x)
  (ash 1 x))

(defun select-channels (chord)
  (reduce #'logior (mapcar #'bit-n (mapcar #'first (rest chord)))))

(defun note-to-byte (note)
  (logior (second (assoc (third note) *notes*))
	  (ash (second note) 4)))

(defun is-key-off (note)
  (eq 'X (third note)))

(defun lookup-note (note)
  (if (not (is-key-off note))
      (note-to-byte note)
      (bit-n 7)))

(defun sort-chord (chord)
  (sort (copy-list chord) #'< :key #'first))

(defun save-score (score)
  (let ((result nil))
    (dolist (chord score)
      (push (select-channels chord) result)
      (dolist (note (sort-chord (rest chord)))
	(push (lookup-note note) result))
      (push (first chord) result))
    (push 0 result)
    (reverse result)))

(defun scale-chord (chord amount)
  (setf (first chord) (* amount (first chord))))

(defun scale-tempo (score amount)
  (mapc (lambda (chord) (scale-chord chord amount)) score))

(defun adjust-note (score data adjust-fn)
  (dolist (chord score)
    (dolist (note (rest chord))
      (funcall adjust-fn note data))))

(defun adjust-note-octaves (note octaves)
  (setf (second note) (+ (second note) (elt octaves (first note)))))

(defun adjust-octaves (score octaves)
  (adjust-note score octaves #'adjust-note-octaves))

(defun clean-up-score (score &optional prev)
  (unless (null score)
    (let ((head (first score)))
      (cond ((and prev (null (rest head)))
	     (incf (first (first prev)) (first head))
	     (setf (rest prev) (rest score))
	     (clean-up-score (rest score) prev))
	    (t (clean-up-score (rest score) score))))))

(defun copy-score (score)
  (copy-tree score))

(defun remove-channel (chord channel &key (test #'=))
  (setf (rest chord) (remove channel (rest chord) :key #'first :test test)))

(defun isolate-channel (score channel)
  (mapc (lambda (chord) (remove-channel chord channel :test #'/=)) score))

(defun key-off-all-score (score)
  (adjust-note score 'X (lambda (note data) (setf (third note) data))))

(defun rename-channels (score channel)
  (adjust-note score channel (lambda (note data) (setf (first note) data))))

(defun insert-at-start (score chord)
  (setf (rest score) (cons (first score) (rest score)))
  (setf (first score) chord))

(defun offset-score (score offset)
  (insert-at-start score (list offset)))

(defun divide-interval (score offset)
  (let ((interval (first (first score))))
    (cond ((< offset interval)
	   (setf (first (first score)) offset)
	   (setf (rest score) (cons (list (- interval offset)) (rest score))))
	  (t (error "divide offset larger than interval")))))

(defun consistency (notes)
  (unless (equal notes (remove-duplicates notes :key #'first))
    (error "inconsistent notes during merge~%~A" notes))
  notes)

(defun merge-chords (dst src)
  (cons (first dst) (consistency (append (rest dst) (rest src)))))

(defun merge-heads (dst src)
  (setf (first dst) (merge-chords (first dst) (first src))))

(defun merge-into (dst src)
  (let ((i (first (first dst)))
	(j (first (first src))))
    (when (> i j)
      (divide-interval dst j))
    (when (< i j)
      (divide-interval src i))
    (merge-heads dst src)
    (cond ((null (rest src)))
	  ((null (rest dst))
	   (setf (rest dst) (rest src)))
	  (t (merge-into (rest dst) (rest src))))))

(defun copy-channel (score src dst)
  (let ((tmp (copy-score score)))
    (isolate-channel tmp src)
    (rename-channels tmp dst)
    (merge-into score tmp)))

(defun channel-key-off (score channel interval)
  (if (consp channel)
      (mapc (lambda (x) (channel-key-off score x interval)) channel)
      (let ((tmp (copy-score score)))
	(isolate-channel tmp channel)
	(offset-score tmp interval)
	(key-off-all-score tmp)
	(merge-into score tmp))))

(defparameter *mute* '((0 0 X) (4 0 X) (5 0 X) (6 0 X)))

(defun append-to-start (score notes)
  (setf (first score) (append (first score) notes)))

(defun attach-at-end (score chord)
  (setf (rest (last score)) (list chord)))

(defun score-length (score)
  (reduce #'+ (mapcar #'first score)))

(defun multiply (score n)
  (apply #'append (make-list n :initial-element score)))

(defun johnny-mk1 ()
  (let ((flute (copy-score *johnny*)))
    (isolate-channel flute 0)
    (rename-channels flute 1)
    (scale-tempo flute 5)
    (let ((drums (drum-score)))
      (channel-key-off drums 6 1)
      (merge-into flute drums))
    (append-to-start flute *mute*)
    flute))

(defun johnny-mk2 ()
  (let ((score (copy-score *johnny*)))
    (copy-channel score 0 1)
    (channel-key-off score 2 1)
    (scale-tempo score 5)
    (merge-into score (drum-score))
    score))

(defun johnny-score ()
  (let ((score (append (johnny-mk1) (johnny-mk2))))
    (adjust-octaves score '(1 2 2 x 0 0 0))
    (clean-up-score score)
    score))

(defparameter *erika-A*
  '((6 (1 0 Cs))
    (2 (1 0 D))
    (4 (1 0 E))
    (4 (1 0 E))
    (4 (1 0 E))
    (4 (1 0 A))
    (4 (1 0 A))
    (4 (1 1 Cs))
    (6 (1 1 Cs))
    (2 (1 0 B))
    (4 (1 0 A))
    (4          (5 0 F))
    (4          (5 0 F))
    (4          (5 0 F))
    (4 (1 0 Gs))
    (4 (1 0 A))
    (4 (1 0 B))
    (4          (5 0 F))
    (4          (5 0 F))
    (4          (5 0 F))
    (6 (1 1 Cs))
    (2 (1 0 B))
    (4 (1 0 A))
    (4          (5 0 F))
    (4          (5 0 F))
    (4          (5 0 F))))

(defparameter *erika-B*
  '((6 (1 0 E))
    (2 (1 0 A))
    (4 (1 0 Gs))
    (4 (1 0 Gs))
    (4 (1 0 Gs))
    (4 (1 0 Gs))
    (4 (1 0 Fs))
    (4 (1 0 Gs))
    (4 (1 0 A))
    (4          (5 0 F))
    (4          (5 0 F))
    (4          (5 0 F))

    (6 (1 0 Gs))
    (2 (1 0 A))
    (4 (1 0 B))
    (4 (1 0 B))
    (4 (1 0 B))
    (4 (1 0 B))
    (4 (1 1 E))
    (4 (1 1 D))
    (4 (1 1 Cs))
    (4          (5 0 F))
    (4          (5 0 F))
    (4          (5 0 F))))

(defun erika-drum-1 ()
  '((4 (2 0 G))
    (4 (2 1 D))
    (4 (2 0 G))
    (4 (2 1 D) (4 0 G))))

(defun erika-drum-2 ()
  (append
   (erika-drum-1)
   '((4 (2 0 G) (4 0 G))
     (4 (2 1 D) (4 0 G)))))

(defun erika-drums-A ()
  (copy-tree
   (append
    (multiply (erika-drum-1) 2)
    (multiply (erika-drum-2) 3))))

(defun erika-drum-alt-1 ()
  '((4 (2 0 G))
    (4 (2 1 D) (4 0 G))))

(defun erika-drum-alt-2 ()
  (append
   (multiply (erika-drum-alt-1) 2)
   '((4 (2 0 G) (4 0 G))
     (4 (2 1 D) (4 0 G)))))

(defun erika-drums-alt-A ()
  (copy-tree
   (append
    (multiply (erika-drum-alt-1) 4)
    (multiply (erika-drum-alt-2) 3))))

(defun erika-drums-B-part ()
  (append
   (multiply (erika-drum-alt-1) 3)
   (erika-drum-alt-2)))

(defun erika-drums-B ()
  (copy-tree (multiply (erika-drums-B-part) 2)))

(defun erika-add-drums (original drums)
  (let ((score (copy-score original)))
    (when drums (merge-into score drums))
    score))

(defun erika-slow ()
  (let ((drums-A (erika-add-drums *erika-A* (erika-drums-A))))
    (copy-score (append *erika-A* drums-A *erika-B* drums-A))))

(defun erika-beat ()
  (let ((drums-A (erika-add-drums *erika-A* (erika-drums-alt-A)))
	(drums-B (erika-add-drums *erika-B* (erika-drums-B))))
    (copy-score (append drums-A drums-A drums-B drums-A))))

(defun adjust-end (score duration)
  (unless (null duration) (setf (caar (last score)) duration)))

(defun erika-music (speed music &optional pause)
  (let ((score (funcall music)))
    (scale-tempo score speed)
    (channel-key-off score '(1 2 4 5) (floor (* 1.5 speed)))
    (adjust-octaves score '(1 2 2 x 0 0 0))
    (clean-up-score score)
    (adjust-end score pause)
    score))

(defun erika-score ()
  (append
   (erika-music 6 #'erika-slow 15)
   (multiply '((24 (2 2 D) (4 0 G) (5 0 F))) 4)
   (erika-music 4 #'erika-beat)
   '((50 (2 0 X)))))

(defun psg-value (frequency volume)
  (logior (- 15 volume) (ash (floor 3579545 (* 32 frequency)) 4)))

(defun convert-sfx (samples f-fn v-fn)
  (let ((time 0.0)
	(result nil)
	(step (/ 1.0 samples)))
    (dotimes (i samples (reverse result))
      (let ((volume (round (+ 1.0 (* 14.0 (funcall v-fn time)))))
	    (frequency (round (funcall f-fn time))))
	(push (psg-value frequency volume) result)
	(incf time step)))))

(defun linear-fade (x)
  (- 1.0 x))

(defun quadratic-fade (x)
  (- 1.0 (expt x 2)))

(defun periodic (x period)
  (mod (* x period) 1.0))

(defun up-down (x)
  (- 1.0 (abs (- (* 2.0 x) 1.0))))

(defun perish (x)
  (- 500 (* 300 (periodic x 4))))

(defun wiggle (x)
  (+ 600 (* (up-down x) 300)))

(defun wiggle-volume (x)
  (- 1.0 (periodic x 2)))

(defun slash (x)
  (+ 1000 (- 1.0 x) (random 1000.0)))

(defun silence-stop ()
  (list #x000f))

(defun word-to-bytes (word)
  (list (ash word -8) (logand word #xff)))

(defun split-in-bytes (list)
  (unless (null list)
    (append (word-to-bytes (first list))
	    (split-in-bytes (rest list)))))

(defun save-sfx (out name sfx)
  (save-array out name (split-in-bytes (append sfx (silence-stop)))))

(defun generate-decople-table ()
  (let ((result nil))
    (dotimes (i 64 (reverse result))
      (push (min 255 (floor (expt i 2) 128)) result))))

(defun save-char-array (out name bytes)
  (save-array out name bytes "char" "~A, "))

(defun scale-and-diff (result radius)
  (mapcar (lambda (x) (floor (* radius x))) result))

(defun generate-circle-table (&optional (steps 64) (radius 32))
  (let ((result nil))
    (dotimes (i steps (reverse (scale-and-diff result radius)))
      (let ((angle (* 2 pi (/ i steps))))
	(push (sin angle) result)
	(push (cos angle) result)))))

(defun save-music ()
  (with-open-file (out "music.inc" :if-exists :supersede :direction :output)
    (save-array out "johnny_score" (save-score (johnny-score)))
    (save-array out "erika_score" (save-score (erika-score)))
    (save-array out "decople_table" (generate-decople-table))
    (save-char-array out "small_circle" (generate-circle-table))
    (save-sfx out "perish" (convert-sfx 16 #'perish #'quadratic-fade))
    (save-sfx out "wiggle" (convert-sfx 20 #'wiggle #'wiggle-volume))
    (save-sfx out "slash" (convert-sfx 32 #'slash #'linear-fade))))

(defun save-and-quit ()
  (handler-case (save-music)
    (condition (var) (format t "ERROR: ~A~%" var)))
  (quit))
