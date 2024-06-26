(defparameter *notes*
  '((C  0) (Cs 1) (D  2)  (Ds 3)
    (E  4) (F  5) (Fs 6)  (G  7)
    (Gs 8) (A  9) (As 10) (B 11)))

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
   '((4 (2 0 G))
     (4 (2 0 G))
     (4 (2 0 G))
     (18 (2 0 G))
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

(defun append-score (&rest rest)
  (copy-score (apply #'append rest)))

(defun remove-channel (chord channel &key (test #'=))
  (setf (rest chord) (remove channel (rest chord) :key #'first :test test)))

(defun isolate-channel (score channel)
  (mapc (lambda (chord) (remove-channel chord channel :test #'/=)) score))

(defun delete-channel (score channel)
  (mapc (lambda (chord) (remove-channel chord channel)) score))

(defun replace-notes-with-key-off (score)
  (adjust-note score 'X (lambda (note data) (setf (third note) data))))

(defun remove-hold-keys (chord)
  (setf (rest chord) (remove :hold (rest chord) :key #'fourth :test #'eq)))

(defun key-off-all-score (score)
  (replace-notes-with-key-off score)
  (mapc #'remove-hold-keys score))

(defun key-off-chord (chord)
  (mapcar (lambda (note) (setf (third note) 'X) note) (copy-tree chord)))

(defun fraction-delay (period fraction)
  (max 1 (min (1- period) (floor (* period fraction)))))

(defun key-off-after (chord period)
  (cons (- period (first chord)) (key-off-chord (rest chord))))

(defun key-off-head (score fraction)
  (let* ((chord (first score))
	 (period (first chord)))
    (unless (null (remove-hold-keys (copy-tree chord)))
      (setf (first chord) (fraction-delay period fraction))
      (push (key-off-after chord period) (rest score)))))

(defun key-off-fraction (score fraction)
  (unless (null score)
    (key-off-fraction (rest score) fraction)
    (key-off-head score fraction)))

(defun next-note (note)
  (first (rest (member (third note) *notes* :key #'first))))

(defun bump-up-note (note)
  (setf (third note) (first (next-note note))))

(defun transpone-note (note channel)
  (when (and (not (is-key-off note)) (= channel (first note)))
    (cond ((eq 'b (third note))
	   (setf (third note) 'c)
	   (incf (second note)))
	  (t (bump-up-note note)))))

(defun transpone-channel (score channel &optional (steps 1))
  (dotimes (i steps) (adjust-note score channel #'transpone-note)))

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

(defun key-off-channel-fraction (score channel fraction)
  (let ((tmp (copy-score score)))
    (delete-channel score channel)
    (isolate-channel tmp channel)
    (clean-up-score tmp)
    (key-off-fraction tmp fraction)
    (merge-into score tmp)))

(defun key-off-channel-interval (score channel interval)
  (let ((tmp (copy-score score)))
    (isolate-channel tmp channel)
    (offset-score tmp interval)
    (key-off-all-score tmp)
    (merge-into score tmp)))

(defun channel-key-off (score channel interval)
  (cond ((consp channel)
	 (mapc (lambda (x) (channel-key-off score x interval)) channel))
	((>= interval 1)
	 (key-off-channel-interval score channel interval))
	((< interval 1)
	 (key-off-channel-fraction score channel interval))))

(defparameter *mute* '((0 0 X) (4 0 X) (5 0 X) (6 0 X)))

(defun append-to-start (score notes)
  (setf (first score) (append (first score) notes)))

(defun attach-at-end (score chord)
  (setf (rest (last score)) (list chord)))

(defun score-length (score)
  (reduce #'+ (mapcar #'first score)))

(defun multiply (score n)
  (copy-score (apply #'append (make-list n :initial-element score))))

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
    (scale-tempo score 5)
    (merge-into score (drum-score))
    score))

(defun johnny-score ()
  (let ((score (append (johnny-mk1) (johnny-mk2))))
    (adjust-octaves score '(1 2 5 x 0 0 0))
    (channel-key-off score 0 9/10)
    (channel-key-off score 1 3/4)
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
  (append
   (multiply (erika-drum-1) 2)
   (multiply (erika-drum-2) 3)))

(defun erika-drum-alt-1 ()
  '((4 (2 0 G))
    (4 (2 1 D) (4 0 G))))

(defun erika-drum-alt-2 ()
  (append
   (multiply (erika-drum-alt-1) 2)
   '((4 (2 0 G) (4 0 G))
     (4 (2 1 D) (4 0 G)))))

(defun erika-drums-alt-A ()
  (append
   (multiply (erika-drum-alt-1) 4)
   (multiply (erika-drum-alt-2) 3)))

(defun erika-drums-B-part ()
  (append
   (multiply (erika-drum-alt-1) 3)
   (erika-drum-alt-2)))

(defun erika-drums-B ()
  (multiply (erika-drums-B-part) 2))

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
    (copy-channel score 1 0)
    (copy-channel score 1 6)
    (channel-key-off score '(0 1 2 4 5 6) (floor (* 1.5 speed)))
    (adjust-octaves score '(1 2 2 x 0 0 1))
    (clean-up-score score)
    (adjust-end score pause)
    score))

(defun erika-score ()
  (append
   (erika-music 6 #'erika-slow 15)
   (multiply '((24 (2 2 D) (4 0 G) (5 0 F))) 4)
   (erika-music 4 #'erika-beat)
   '((50 (2 0 X)))))

(defparameter *doves*
  '((8  (0 0 B))
    (8  (0 0 B))
    (12 (0 0 B  :hold))
    (4  (0 0 Fs))

    (8  (0 0 A))
    (8  (0 0 A))
    (12 (0 0 A  :hold))
    (4  (0 0 Fs))

    (4  (0 0 G))
    (4  (0 0 Fs))
    (8  (0 0 E))
    (12 (0 0 B  :hold))
    (4  (0 0 G))

    (8  (0 0 Fs))
    (8  (0 0 Fs))
    (16 (0 0 Fs))

    (16 (0 1 D  :hold))
    (16 (0 1 Cs :hold))

    (4  (0 1 D))
    (4  (0 1 Cs))
    (4  (0 0 B))
    (4  (0 0 As))
    (16 (0 0 B  :hold))

    (8  (0 0 A))
    (8  (0 0 A))
    (12 (0 0 B  :hold))
    (4  (0 0 G))

    (8  (0 0 Fs))
    (8  (0 0 Fs))
    (16 (0 0 B))))

(defun dove-drum-1 ()
  (copy-score
   '((8  (1 0 C))
     (8  (1 0 C))
     (16 (1 0 C)))))

(defun dove-drum-2 ()
  (copy-score
   '((4  (1 0 C))
     (4  (1 0 C))
     (8  (1 0 C))
     (16 (1 0 C)))))

(defun dove-drum-alt ()
  (copy-score
   '((16 (1 0 C))
     (16 (1 0 C))
     (4  (1 0 C))
     (4  (1 0 C))
     (4  (1 0 C))
     (4  (1 0 C))
     (16 (1 0 C)))))

(defun dove-drum-drop ()
  (copy-score
   '((16 (1 0 C))
     (16 (1 0 C))
     (16 (1 0 C))
     (16 (1 0 C))
     (4  (1 0 C))
     (4  (1 0 C))
     (4  (1 0 C))
     (4  (1 0 C))
     (8  (1 0 C))
     (8  (1 0 C))
     (16 (1 0 C))
     (16 (1 0 C)))))

(defun dove-drum-score-old ()
  (append
   (multiply (dove-drum-1) 2)
   (multiply (dove-drum-2) 2)
   (multiply (dove-drum-1) 2)
   (dove-drum-alt)

   (multiply (dove-drum-1) 2)
   (multiply (dove-drum-2) 2)
   (multiply (dove-drum-1) 2)
   (dove-drum-alt)

   (multiply (dove-drum-2) 2)
   (multiply (dove-drum-2) 2)
   (dove-drum-alt)
   (dove-drum-alt)

   (multiply (dove-drum-1) 2)
   (multiply (dove-drum-2) 2)
   (multiply (dove-drum-1) 2)
   (dove-drum-alt)))

(defun dove-drum-a ()
  (copy-score
   '((32 (1 0 C))
     (32 (1 0 C))
     (4  (1 0 C))
     (4  (1 0 C))
     (4  (1 0 C))
     (4  (1 0 C))
     (32 (1 0 C))
     (8 (1 0 C))
     (8 (1 0 C)))))

(defun dove-drum-b ()
  (copy-score
   '((32  (1 0 C))
     (32  (1 0 C))
     (64  (1 0 C)))))

(defun dove-drum-score ()
  (append
   (dove-drum-a)
   (dove-drum-a)
   (dove-drum-a)
   (dove-drum-b)

   (multiply (dove-drum-2) 2)
   (multiply (dove-drum-2) 2)
   (dove-drum-drop)

   (dove-drum-a)
   (dove-drum-b)))

(defun doves-score ()
  (let ((score (copy-score *doves*)))
    (copy-channel score 0 2)
    (channel-key-off score 2 2)
    (scale-tempo score 4)
    (channel-key-off score 0 7/8)
    (merge-into score (dove-drum-score))
    (adjust-octaves score '(2 5 2 x 0 0 0))
    (clean-up-score score)
    ;(adjust-end score 16) ; for dove-drum-score-old
    score))

(defparameter *battotai-1*
  '((4  (0 0 A))
    (4  (0 1 E))
    (8  (0 1 E))

    (8  (0 1 E))
    (8  (0 1 E))

    (4  (0 1 F))
    (4  (0 1 F))
    (4  (0 1 D))
    (4  (0 1 F))

    (12 (0 1 E))
    (4  (0 0 X))

    (4  (0 1 D))
    (8  (0 0 B))
    (4  (0 1 D))

    (4  (0 1 E))
    (4  (0 1 E))
    (4  (0 1 E))
    (4  (0 1 E))

    (8  (0 0 B))
    (6  (0 1 C))
    (2  (0 0 B))

    (8  (0 0 A))
    (8  (0 0 X))))

(defparameter *battotai-2*
  '((4  (0 1 A))
    (8  (0 1 A))
    (4  (0 1 B))

    (4  (0 2 C))
    (4  (0 2 C))
    (8  (0 2 C))

    (4  (0 2 C))
    (4  (0 1 B))
    (4  (0 1 A))
    (4  (0 1 B))

    (12 (0 1 E))
    (4  (0 0 X))

    (8  (0 1 D))
    (4  (0 1 E))
    (4  (0 1 F))

    (6  (0 1 G))
    (2  (0 1 G))
    (4  (0 1 G))
    (4  (0 1 G))

    (6  (0 1 D))
    (2  (0 1 D))
    (4  (0 1 E))
    (4  (0 1 D))

    (4  (0 1 C))
    (12 (0 0 X))))

(defparameter *battotai-3*
  '((4  (0 2 C))
    (4  (0 2 C))
    (4  (0 1 B))
    (4  (0 1 A))

    (6  (0 1 G))
    (2  (0 1 G))
    (8  (0 1 G))

    (4  (0 1 A))
    (4  (0 1 A))
    (4  (0 1 G))
    (4  (0 1 F))

    (12 (0 1 E))
    (4  (0 0 X))

    (4  (0 0 A))
    (4  (0 0 B))
    (4  (0 1 C))
    (4  (0 1 D))

    (4  (0 1 E))
    (4  (0 1 E))
    (8  (0 1 E))

    (4  (0 1 A))
    (4  (0 1 A))
    (4  (0 1 Gs))
    (4  (0 1 A))

    (12 (0 1 B))
    (4  (0 0 X))))

(defparameter *battotai-4*
  '((8  (0 2 Cs))
    (6  (0 1 B))
    (2  (0 1 A))

    (4  (0 1 A))
    (4  (0 1 A))
    (4  (0 1 A))
    (4  (0 1 A))

    (4  (0 1 Fs))
    (4  (0 1 Fs))
    (4  (0 1 A))
    (4  (0 1 A))

    (12 (0 2 Cs))
    (4  (0 0 X))

    (4  (0 1 A))
    (4  (0 1 A))
    (6  (0 1 Gs))
    (2  (0 1 A))

    (8  (0 1 Fs))
    (4  (0 1 Gs))
    (4  (0 1 A))

    (6  (0 1 B))
    (2  (0 1 B))
    (4  (0 1 B))
    (4  (0 1 B))

    (12 (0 1 B))
    (4  (0 0 X))))

(defparameter *battotai-5*
  '((4  (0 2 Cs))
    (4  (0 2 Cs))
    (4  (0 1 B))
    (4  (0 1 A))

    (4  (0 1 A))
    (4  (0 1 A))
    (8  (0 1 A))

    (4  (0 1 Fs))
    (4  (0 1 Fs))
    (4  (0 2 D))
    (4  (0 2 D))

    (12 (0 2 Cs))
    (4  (0 0 X))

    (8  (0 1 B))
    (4  (0 2 Cs))
    (4  (0 2 D))

    (4  (0 2 E))
    (4  (0 2 Cs))
    (4  (0 1 A))
    (4  (0 2 D))

    (8  (0 2 Cs))
    (6  (0 1 B))
    (2  (0 2 Cs))

    (8  (0 1 A))
    (4  (0 1 A))
    (4  (0 0 X))
))

(defun battotai-drums-a ()
  '((4  (1 0 C))
    (4  (1 0 C))
    (4  (1 0 C))
    (4  (1 0 C))
    (8  (1 0 C))
    (8  (1 0 C))
    (16 (1 0 C))
    (16 (1 0 C))))

(defun battotai-drums-b ()
  (multiply '((8 (1 0 C)) (8 (1 0 C)) (16 (1 0 C))) 2))

(defun battotai-drums-c ()
  '((4  (1 0 C))
    (4  (1 0 C))
    (8  (1 0 C))
    (16 (1 0 C))
    (16 (1 0 C))
    (4  (1 0 C))
    (4  (1 0 C))
    (8  (1 0 C))))

(defun battotai-drums-d ()
  (reverse (battotai-drums-a)))

(defun battotai-drums-r ()
  (multiply '((4 (1 0 C)) (4 (1 0 C)) (8 (1 0 C))) 4))

(defun battotai-drums-z ()
  '((16 (1 0 C)) (16 (1 0 C)) (32 (1 0 C))))

(defun battotai-select-drum (x)
  (case x
    (a (battotai-drums-a))
    (b (battotai-drums-b))
    (c (battotai-drums-c))
    (d (battotai-drums-d))
    (r (battotai-drums-r))
    (z (battotai-drums-z))))

(defun battotai-drum-row (row)
  (apply #'append (mapcar #'battotai-select-drum row)))

(defun battotai-drums ()
  (copy-score
   (append
    (multiply (battotai-drum-row '(a b r z c r d z)) 2)
    (multiply (battotai-drum-row '(b z d z a a a z)) 2)
    (battotai-drum-row '(r a r z r z r z))
    (battotai-drum-row '(d b r z b a a z))
    (battotai-drum-row '(a b b z d b d z)))))

(defun battotai-flute ()
  (append
   (multiply *battotai-1* 2)
   (multiply *battotai-2* 2)
   *battotai-3*
   *battotai-4*
   *battotai-5*))

(defun oom-pah ()
  '((32 (2 1 C) (6 0 C))
    (32 (2 0 G))))

(defun battotai-score ()
  (let ((score (copy-score (battotai-flute))))
    (scale-tempo score 4)
    (copy-channel score 0 4)
    (copy-channel score 2 5)
    (merge-into score (multiply (oom-pah) 56))
    (merge-into score (battotai-drums))
    (adjust-octaves score '(1 5 2 x 2 3 1))
    (channel-key-off score 0 3/4)
    (channel-key-off score 2 1/4)
    (channel-key-off score 4 1/2)
    (channel-key-off score 5 1/3)
    (channel-key-off score 6 1/8)
    (clean-up-score score)
    score))

(defparameter *onions-1a*
  '((1  (0 0 G))
    (1  (0 0 G))
    (1  (0 0 G))
    (3  (0 1 C))
    (2  (0 1 D))
    (1  (0 1 D))
    (2  (0 1 E))
    (1  (0 1 E))))

(defparameter *onions-1b*
  '((1  (0 0 G))
    (1  (0 0 G))
    (1  (0 0 G))
    (2  (0 1 C))
    (1  (0 1 C))
    (2  (0 1 E))
    (1  (0 1 E))
    (3  (0 1 D))))

(defparameter *onions-1c*
  '((1  (0 1 D))
    (1  (0 1 E))
    (1  (0 1 D))
    (3  (0 1 C))
    (1  (0 1 D))
    (1  (0 1 E))
    (1  (0 1 D))
    (3  (0 1 C))))

(defparameter *onions-2a*
  '((2  (0 1 G))
    (2  (0 1 E) (4 0 C))
    (1  (0 1 E))
    (1  (0 1 E))
    (2  (0 1 E))
    (2  (0 1 G))

    (2  (0 1 E) (4 0 C))
    (1  (0 1 E))
    (1  (0 1 E))
    (2  (0 1 E))
    (2  (0 1 G))

    (2  (0 1 A) (4 0 C))
    (2  (0 1 G))
    (2  (0 1 F) (4 0 C))
    (2  (0 1 E))
    (6  (0 1 D) (4 0 C))

    (2  (0 1 F))
    (2  (0 1 D) (4 0 C))
    (1  (0 1 D))
    (1  (0 1 D))
    (2  (0 1 D))
    (2  (0 1 F))

    (2  (0 1 D) (4 0 C))
    (1  (0 1 D))
    (1  (0 1 D))
    (2  (0 1 D))
    (2  (0 1 F))

    (2  (0 1 G) (4 0 C))
    (2  (0 1 F))
    (2  (0 1 E) (4 0 C))
    (2  (0 1 D))))

(defparameter *onions-2b*
  '((6  (0 1 C) (4 0 C))))

(defparameter *onions-2c*
  '((8  (0 1 C) (4 0 C))))

(defparameter *onion-pause* '((6)))

(defun onions-flute ()
  (let ((p1 (append-score *onions-1a* *onions-1b* *onions-1a* *onions-1c*))
	(p2 (append-score *onions-2a* *onions-2b* *onions-2a* *onions-2c*)))
    (scale-tempo p1 12)
    (scale-tempo p2 8)
    (append-score
     p1 *onion-pause*
     p2 *onion-pause*)))

(defun onion-drums-1 ()
  '((1  (6 0 C))
    (1  (6 0 C))
    (2  (6 0 C))
    (4  (6 0 C))
    (4  (6 0 C))

    (12 (6 0 C))
    (8  (6 0 C))
    (4  (6 0 C))
    (8  (6 0 C))

    (4  (6 0 C))
    (4  (6 0 C))
    (4  (6 0 C))
    (4  (6 0 C))))

(defun onion-drums-1a ()
  (append-score
   (onion-drums-1)
   '((8  (6 0 C))
     (4  (6 0 C))
     (8  (6 0 C))
     (4  (6 0 C))
     (12 (6 0 C)))))

(defun onion-drums-1b ()
  (append-score
   (onion-drums-1)
   '((12 (6 0 C))
     (4  (6 0 C))
     (4  (6 0 C))
     (4  (6 0 C))
     (12 (6 0 C)))))

(defun onion-roll ()
  '((1  (6 0 C))
    (1  (6 0 C))
    (1  (6 0 C))
    (1  (6 0 C))
    (2  (6 0 C))
    (2  (6 0 C))
    (4  (6 0 C))
    (4  (6 0 C))))

(defun onion-slow-roll ()
  '((1  (6 0 C))
    (1  (6 0 C))
    (2  (6 0 C))
    (2  (6 0 C))
    (2  (6 0 C))
    (4  (6 0 C))
    (4  (6 0 C))))

(defun onion-drums-2 ()
  (append
   '((4  (6 0 C)))
   (onion-roll)
   (onion-roll)
   (onion-slow-roll)
   '((12 (6 0 C)))

   '((4  (6 0 C)))
   (onion-slow-roll)
   (onion-slow-roll)
   (onion-roll)
   '((12 (6 0 C)))))

(defun onion-drums ()
  (let ((p1 (append-score (onion-drums-1a) (onion-drums-1b)))
	(p2 (multiply (onion-drums-2) 2)))
    (scale-tempo p1 3)
    (scale-tempo p2 4)
    (append-score
     p1 *onion-pause*
     p2 *onion-pause*)))

(defun onion-score-common (score)
  (merge-into score (onion-drums))
  (adjust-octaves score '(2 3 2 x 1 1 5))
  (channel-key-off score 0 3/4)
  (channel-key-off score 1 4/5)
  (channel-key-off score 2 2/3)
  (channel-key-off score '(4 5) 16)
  (clean-up-score score)
  score)

(defun onion-score-1 ()
  (let ((score (copy-score (onions-flute))))
    (delete-channel score 4)
    (transpone-channel score 0 8)
    (onion-score-common score)))

(defun onion-score-2 ()
  (let ((score (copy-score (onions-flute))))
    (copy-channel score 0 1)
    (copy-channel score 0 2)
    (copy-channel score 4 5)
    (delete-channel score 0)
    (onion-score-common score)))

(defun onions-score ()
  (append (onion-score-1) (onion-score-2)))

(defparameter *chord-A* '((4 1 A) (5 2 Cs) (6 2 E)))
(defparameter *chord-B* '((4 1 B) (5 1 Ds) (6 1 Fs)))
(defparameter *chord-C* '((4 1 C) (5 1 E) (6 1 G)))
(defparameter *chord-D* '((4 1 D) (5 1 Fs) (6 1 A)))
(defparameter *chord-E* '((4 1 E) (5 1 Gs) (6 1 B)))
(defparameter *chord-F* '((4 1 F) (5 1 A) (6 2 C)))
(defparameter *chord-G* '((4 1 G) (5 1 B) (6 2 D)))

(defparameter *chord-Am* '((4 1 A) (5 2 C) (6 2 E)))
(defparameter *chord-Bm* '((4 1 B) (5 1 D) (6 1 Fs)))
(defparameter *chord-Cm* '((4 1 C) (5 1 Ds) (6 1 G)))
(defparameter *chord-Dm* '((4 1 D) (5 1 F) (6 1 A)))
(defparameter *chord-Em* '((4 1 E) (5 1 G) (6 1 B)))
(defparameter *chord-Fm* '((4 1 F) (5 1 Gs) (6 2 C)))
(defparameter *chord-Gm* '((4 1 G) (5 1 As) (6 2 D)))

(defparameter *katyusha-1a*
  `((3 (0 1 A) ,@*chord-Am*)
    (1 (0 1 B))
    (3 (0 2 C))
    (1 (0 1 A))

    (1 (0 2 C))
    (1 (0 2 C))
    (1 (0 1 B))
    (1 (0 1 A))
    (2 (0 1 B) ,@*chord-E*)
    (2 (0 1 E) ,@*chord-Em*)

    (3 (0 1 B))
    (1 (0 2 C))
    (3 (0 2 D))
    (1 (0 1 B))

    (1 (0 2 D))
    (1 (0 2 D))
    (1 (0 2 C))
    (1 (0 1 B))
    (4 (0 1 A) ,@*chord-Am*)))

(defparameter *katyusha-1b*
  `((2 (0 2 E) ,@*chord-Am*)
    (2 (0 2 A) ,@*chord-F*)
    (2 (0 2 G) ,@*chord-C*)
    (1 (0 2 A) ,@*chord-A*)
    (1 (0 2 G))

    (1 (0 2 F) ,@*chord-Dm*)
    (1 (0 2 F))
    (1 (0 2 E))
    (1 (0 2 D))
    (2 (0 2 E) ,@*chord-Am*)
    (2 (0 1 A))

    (1 (0 0 X) ,@*chord-Dm*)
    (2 (0 2 F))
    (1 (0 2 D))
    (3 (0 2 E) ,@*chord-Am*)
    (1 (0 2 C))

    (1 (0 1 B) ,@*chord-E*)
    (1 (0 1 E))
    (1 (0 2 C) ,@*chord-Em*)
    (1 (0 1 B))
    (4 (0 1 A) ,@*chord-Am*)))

(defparameter *katyusha-2a*
  `((3 (0 1 E) ,@*chord-Em*)
    (1 (0 1 Fs))
    (3 (0 1 G))
    (1 (0 1 E))

    (1 (0 1 G))
    (1 (0 1 G))
    (1 (0 1 Fs))
    (1 (0 1 E))
    (2 (0 1 Fs) ,@*chord-B*)
    (2 (0 0 B) ,@*chord-Bm*)

    (3 (0 1 Fs))
    (1 (0 1 G))
    (3 (0 1 A))
    (1 (0 1 Fs))

    (1 (0 1 A))
    (1 (0 1 A))
    (1 (0 1 G))
    (1 (0 1 Fs))
    (4 (0 1 E) ,@*chord-Em*)))

(defparameter *katyusha-2b*
  `((2 (0 1 B) ,@*chord-Em*)
    (2 (0 2 E) ,@*chord-C*)
    (2 (0 2 D) ,@*chord-G*)
    (1 (0 2 E) ,@*chord-E*)
    (1 (0 2 D))

    (1 (0 2 C) ,@*chord-Am*)
    (1 (0 2 C))
    (1 (0 1 B))
    (1 (0 1 A))
    (2 (0 1 B) ,@*chord-Em*)
    (2 (0 1 E))

    (1 (0 0 X) ,@*chord-Am*)
    (2 (0 2 C))
    (1 (0 1 A))
    (3 (0 1 B) ,@*chord-Em*)
    (1 (0 1 G))

    (1 (0 1 Fs) ,@*chord-B*)
    (1 (0 0 B))
    (1 (0 1 G) ,@*chord-Bm*)
    (1 (0 1 Fs))
    (4 (0 1 E) ,@*chord-Em*)))

(defparameter *katyusha-3a*
  `((3 (0 1 G) ,@*chord-Gm*)
    (1 (0 1 A))
    (3 (0 1 As))
    (1 (0 1 G))

    (1 (0 1 As))
    (1 (0 1 As))
    (1 (0 1 A))
    (1 (0 1 G))
    (2 (0 1 A) ,@*chord-D*)
    (2 (0 1 D) ,@*chord-Dm*)

    (3 (0 1 A))
    (1 (0 1 As))
    (3 (0 2 C))
    (1 (0 1 A))

    (1 (0 2 C))
    (1 (0 2 C))
    (1 (0 1 As))
    (1 (0 1 A))
    (4 (0 1 G) ,@*chord-Gm*)))

(defparameter *katyusha-3b*
  `((2 (0 2 D) ,@*chord-Gm*)
    (2 (0 2 G) ,@*chord-E*)
    (2 (0 2 F) ,@*chord-B*)
    (1 (0 2 G) ,@*chord-G*)
    (1 (0 2 F))

    (1 (0 2 Ds) ,@*chord-Cm*)
    (1 (0 2 Ds))
    (1 (0 2 D))
    (1 (0 2 C))
    (2 (0 2 D) ,@*chord-Gm*)
    (2 (0 1 G))

    (1 (0 0 X) ,@*chord-Cm*)
    (2 (0 2 Ds))
    (1 (0 2 C))
    (3 (0 2 D) ,@*chord-Gm*)
    (1 (0 1 As))

    (1 (0 1 A) ,@*chord-D*)
    (1 (0 1 D))
    (1 (0 1 As) ,@*chord-Dm*)
    (1 (0 1 A))
    (4 (0 1 G) ,@*chord-Gm*)))

(defparameter *katyusha-4a*
  `((3 (0 1 D) ,@*chord-Dm*)
    (1 (0 1 E))
    (3 (0 1 F))
    (1 (0 1 D))

    (1 (0 1 F))
    (1 (0 1 F))
    (1 (0 1 E))
    (1 (0 1 D))
    (2 (0 1 E) ,@*chord-A*)
    (2 (0 0 A) ,@*chord-Am*)

    (3 (0 1 E))
    (1 (0 1 F))
    (3 (0 1 G))
    (1 (0 1 E))

    (1 (0 1 G))
    (1 (0 1 G))
    (1 (0 1 F))
    (1 (0 1 E))
    (4 (0 1 D) ,@*chord-Dm*)))

(defparameter *katyusha-4b*
  `((2 (0 1 A) ,@*chord-Dm*)
    (2 (0 2 D) ,@*chord-B*)
    (2 (0 2 C) ,@*chord-F*)
    (1 (0 2 D) ,@*chord-D*)
    (1 (0 2 C))

    (1 (0 1 As) ,@*chord-Gm*)
    (1 (0 1 As))
    (1 (0 1 A))
    (1 (0 1 G))
    (2 (0 1 A) ,@*chord-Dm*)
    (2 (0 1 D))

    (1 (0 0 X) ,@*chord-Gm*)
    (2 (0 1 As))
    (1 (0 1 G))
    (3 (0 1 A) ,@*chord-Dm*)
    (1 (0 1 F))

    (1 (0 1 E) ,@*chord-A*)
    (1 (0 0 A))
    (1 (0 1 F) ,@*chord-Am*)
    (1 (0 1 E))
    (4 (0 1 D) ,@*chord-Dm*)))

(defun katyusha-drums-prefix ()
  (copy-score
   '((4  (2 0 C))
     (4  (2 0 C))
     (28 (2 0 C))
     (12 (2 0 C))

     (4  (2 0 C))
     (4  (2 0 C))
     (28 (2 0 C))
     (12 (2 0 C))

     (12 (2 0 C))
     (12 (2 0 C))
     (12 (2 0 C))
     (12 (2 0 C)))))

(defun katyusha-drums-a ()
  (append (katyusha-drums-prefix) '((24 (2 0 C)) (24 (2 0 C)))))

(defun katyusha-drums-c ()
  (append (katyusha-drums-prefix) '((48 (2 0 C)))))

(defun katyusha-drums-b ()
  (copy-score
   '((4  (2 0 C))
     (4  (2 0 C))
     (16 (2 0 C))
     (4  (2 0 C))
     (4  (2 0 C))
     (16 (2 0 C))
     (4  (2 0 C))
     (4  (2 0 C))
     (16 (2 0 C))
     (12 (2 0 C))
     (12 (2 0 C))

     (12 (2 0 C))
     (12 (2 0 C))
     (12 (2 0 C))
     (12 (2 0 C))

     (24  (2 0 C))
     (24  (2 0 C)))))

(defun katyusha-drums ()
  (append (katyusha-drums-a) (katyusha-drums-c)
	  (katyusha-drums-b) (katyusha-drums-c)
	  (katyusha-drums-b) (katyusha-drums-c)))

(defun katyusha-notes ()
  (copy-score
   (append
    *katyusha-1a* *katyusha-1b* *katyusha-1b*
    *katyusha-2a* *katyusha-2b* *katyusha-2b*
    *katyusha-3a* *katyusha-3b* *katyusha-3b*
    *katyusha-4a* *katyusha-4b* *katyusha-4b*)))

(defun katyusha-score ()
  (let ((score (katyusha-notes)))
    (scale-tempo score 12)
    (copy-channel score 0 1)
    (merge-into score (multiply (katyusha-drums) 4))
    (adjust-octaves score '(4 1 5 x 1 1 1))
    (channel-key-off score 0 5/6)
    (channel-key-off score 1 1/3)
    (clean-up-score score)
    score))

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

(defun generate-circle-table (&optional (steps 128) (radius 36) (ratio 1.0))
  (let ((result nil))
    (dotimes (i steps (reverse (scale-and-diff result radius)))
      (let ((angle (* 2 pi (/ i steps))))
	(push (* ratio (sin angle)) result)
	(push (cos angle) result)))))

(defun get-ray-step (alpha &optional (len 1.1))
  (list (* len (cos alpha)) (* len (sin alpha))))

(defun pos-to-diff (pos)
  (unless (null (rest pos))
    (cons (mapcar #'- (second pos) (first pos)) (pos-to-diff (rest pos)))))

(defun generate-single-ray (angle)
  (let* ((alpha (* (/ pi 180) angle))
	 (step (get-ray-step alpha))
	 (done (list (list 0 0)))
	 (line (list 0.0 0.0)))
    (loop for counter from 1 to 32 do
      (setf line (mapcar #'+ line step))
      (push (mapcar #'round line) done))
    (pos-to-diff (reverse done))))

(defun ray-name (angle axis)
  (format nil "ray_~A_~A" axis angle))

(defun save-steps (out angle steps)
  (let ((x-axis (mapcar #'first steps))
	(y-axis (mapcar #'second steps)))
    (save-char-array out (ray-name angle "pX") x-axis)
    (save-char-array out (ray-name angle "pY") y-axis)
    (save-char-array out (ray-name angle "mX") (mapcar #'- x-axis))
    (save-char-array out (ray-name angle "mY") (mapcar #'- y-axis))))

(defun sub-angle (angle)
  (let ((beta (mod angle 90)))
    (if (<= beta 45) beta (- 90 beta))))

(defun get-x-prefix (angle)
  (elt '("pX" "pY" "mY" "mX" "mX" "mY" "pY" "pX") (floor angle 45)))

(defun get-y-prefix (angle)
  (elt '("mY" "mX" "mX" "mY" "pY" "pX" "pX" "pY") (floor angle 45)))

(defun generate-ray-table (out &optional (step 5))
  (loop for angle from 0 to 45 by step do
    (save-steps out angle (generate-single-ray angle)))
  (format out "const Ray rays[] = {~%")
  (loop for angle from 0 to 359 by step do
    (let ((beta (sub-angle angle)))
      (format out "{ dx:~A, dy:~A },~%"
	      (ray-name beta (get-x-prefix angle))
	      (ray-name beta (get-y-prefix angle)))))
  (format out "};~%"))

(defun save-music ()
  (with-open-file (out "music.inc" :if-exists :supersede :direction :output)
    (save-array out "johnny_score" (save-score (johnny-score)))
    (save-array out "erika_score" (save-score (erika-score)))
    (save-array out "doves_score" (save-score (doves-score)))
    (save-array out "battotai_score" (save-score (battotai-score)))
    (save-array out "onions_score" (save-score (onions-score)))
    (save-array out "katyusha_score" (save-score (katyusha-score)))
    (save-array out "decople_table" (generate-decople-table))
    (save-char-array out "small_circle" (generate-circle-table))
    (save-char-array out "tiny_circle" (generate-circle-table 64 16))
    (save-char-array out "larger_circle" (generate-circle-table 256 52 1.8))
    (save-sfx out "perish" (convert-sfx 16 #'perish #'quadratic-fade))
    (save-sfx out "wiggle" (convert-sfx 20 #'wiggle #'wiggle-volume))
    (save-sfx out "slash" (convert-sfx 32 #'slash #'linear-fade))
    (generate-ray-table out)))

(defun save-and-quit ()
  (handler-case (save-music)
    (condition (var) (format t "ERROR: ~A~%" var)))
  (quit))
