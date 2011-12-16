; lexicon.l
;; a lexicon is a collection of entries

(load "entry.l")

(defclass lexicon ()
  ((length :accessor lexicon-length :initarg :length)
   (entries :accessor lexicon-entries :initarg :entries)))

;; constructor for lexicon using file
(defmethod make-lexicon ((file-name string) &aux entries)
  (make-instance 'lexicon
		 :entries (setf entries (load-entries file-name))
		 :length (length entries)))

(defmethod load-entries ((file-name string))
  (evaluate-all (cdr (read (open file-name)))))

;; constructor for lexicon using entries
(defmethod make-lexicon ((entries list))
  (make-instance 'lexicon 
		 :entries entries
		 :length (length entries)))

;; save the lexicon
(setf *lexfile* "lex.l")
(setf *lexstream* nil)
(defmethod save-lexicon ((l lexicon))
  (setf *lexstream* (open *lexfile* :direction :output))
  (princ '\( *lexstream*) (princ 'list *lexstream*)
  (terpri *lexstream*)
  (mapcar 'write-entry-to-disk (lexicon-entries l))
  (princ '\) *lexstream*)
  (terpri *lexstream*)  (terpri *lexstream*)
  (close *lexstream*))

(defmethod write-entry-to-disk ((e entry))
  (prin1
   (append '(make-instance 'entry :word)
           (list (entry-word e))
           '(:syllables)  
           (list (list 'quote (entry-syllables e)))
           '(:sounds)
           (list (list 'quote (entry-sounds e)))
           '(:syllasounds)
           (list (list 'quote (entry-syllasounds e)))
           '(:stress)
           (list (entry-stress e))
           '(:rhyme)
           (list (list 'quote (entry-rhyme e)))
           '(:rhythm)
           (list (list 'quote (entry-rhythm e)))
           '(:nn-input)
           (list (list 'quote (entry-nn-input e)))
           '(:index)
           (list (entry-index e)))
   *lexstream*)
  (terpri *lexstream*))
  
;; constructor for lexicon using existing lexicon
(defmethod make-lexicon ((l lexicon))
  (make-lexicon (lexicon-entries l)))

(defmethod clone-lexicon ((l lexicon))
  (make-lexicon l))

;; add lexion B's entries to lexicon A
(defmethod add-entries ((l lexicon) (e lexicon))
  (setf (lexicon-entries l) 
        (cons (lexicon-entries e) (lexicon-entries e))))

(defmethod display ((objects list))
  (mapcar 'display objects))

;; display entries in a readable format
(defmethod display ((l lexicon))
  (write-string "Size: ") (princ (lexicon-length l)) (terpri)
  (write-string "Entries: ") (terpri)
  (mapcar 'display (lexicon-entries l))
  (write-string "that's all folks!") t)

