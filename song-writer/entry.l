;; entry.l

(defclass entry ()
  ((word :accessor entry-word :initarg :word)
   (syllables :accessor entry-syllables :initarg :syllables)
   (sounds :accessor entry-sounds :initarg :sounds)
   (rhyme :accessor entry-rhyme :initarg :rhyme)
   (rhythm :accessor entry-rhythm :initarg :rhythm)
   (stress :accessor entry-stress :initarg :stress)
   (index :accessor entry-index :initarg :index)
   (syllasounds :accessor entry-syllasounds :initarg :syllasounds)
   (nn-input :accessor entry-nn-input :initarg :nn-input)))

(defmethod seek-entries ((words list) (entries list) &aux result)
  (cond ((null words) nil)
        ((setf result (seek-entry (car words) entries))
         (cons result (seek-entries (cdr words) entries)))
        (t (seek-entries (cdr words) entries))))

(defmethod seek-entry ((word string)(entries list))
  (remove-if #'(lambda (arg) (not (equal (string-downcase word) (string-downcase (entry-word arg))))) entries))

;; return a list of only the "word" from each entry in a list
(defmethod entries-to-word-list ((entries list))
  (cond ((null entries)
         nil)
        (t (cons (entry-word (car entries)) 
                 (entries-to-word-list (cdr entries)))))) 

(defmethod total-syllables ((entries list))
  (cond ((null entries)
         0)
        (t 
         (+ (length (entry-syllables (car entries)))
            (total-syllables (cdr entries))))))

(defmethod display ((e entry))
  (write-string "Word: ") (prin1 (entry-word e)) (terpri)
  (write-string "Syllables: ") (prin1 (entry-syllables e))(terpri)
  (write-string "Sounds: ") (prin1 (entry-sounds e)) (terpri)
  (write-string "Syllable / Sounds: ") (prin1 (entry-syllasounds e)) (terpri)
  (write-string "Rhyme Ending: ") (prin1 (entry-rhyme e)) (terpri)
  (write-string "Rhythm: ") (prin1 (entry-rhythm e)) (terpri)
  (write-string "Stress: ") (prin1 (entry-stress e)) (terpri)
  (write-string "Binary Version: ") (prin1 (entry-nn-input e)) (terpri)
  (write-string "Entry Index #: ") (prin1 (entry-index e)) (terpri)
  (write-string "------------------------------------------------")
  (terpri)
  e)

(defmethod display-in-line ((objects list))
  (cond ((null objects) 
         nil)
        (t
         (cons (display (car objects))
               (display-in-line (cdr objects))))))

(defun entryp (s)
  (typep s 'entry))