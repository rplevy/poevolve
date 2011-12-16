;genlex.l

;(in-package "pcl")
;(load "utilities.l")
;(load "lexicon.l")
;(load "phonemicon.l")

(setf *nr-entries* 0)

(defmethod genlex ((file string) &aux lex lines)
  (write-string-terpri "Generating lexicon, please hold...")
  (setf lex (make-instance 'lexicon))
  (setf lines (make-list-of-lines file))
  (setf (lexicon-entries lex) (mapcar #'make-entry-from-data lines))
  (setf (lexicon-length lex) (length (lexicon-entries lex)))
  lex) 

(defmethod make-entry-from-data ((entry-data string))
  (apply #'make-entry (segment (coerce entry-data 'list) '#\-)))

(defmethod make-entry ((word list) (syllables list) (sounds list) (stress list)&aux syllasounds)
  (display 
   (make-instance 'entry 
                  :word (clean-up-string-list-i word)
                  :syllables (clean-up-string-list-ii syllables)
                  :sounds (format-sounds sounds)
                  :stress (setf stress (clean-up-num-list stress))
                  :index (setf *nr-entries* (+ 1 *nr-entries*))
                  :syllasounds (setf syllasounds (format-syl-sounds sounds))
                  :rhyme (format-rhyme syllasounds stress)
                  :rhythm (format-rhythm stress syllasounds)
                  :nn-input (format-nn-input *nr-entries* syllasounds stress))))

(defmethod format-rhythm ((stress number) (syllables list))
  (cond ((zerop stress)
         (list 0))
        (t 
         (append     
          (make-list (- stress 1) :initial-element 0)
          (cons 1 (make-list (- (length syllables) stress)
                             :initial-element 0))))))
  
(defmethod format-rhyme ((sounds list) (stress number))
  (cond 
   ((equal stress 0)
    nil)
   ((equal stress 
           (length sounds))
    (get-masculine-rhyme (final sounds)))
   (t
    (get-feminine-rhyme stress sounds))))

(defmethod get-masculine-rhyme ((sounds list))
  (cond ((null sounds) nil)
        ((vowelp (get-phoneme (car sounds))) sounds)
        (t (get-masculine-rhyme (cdr sounds)))))

(defmethod get-feminine-rhyme ((stress number) (syllasounds list))
  (cond ((onep stress)
         (append (get-masculine-rhyme (car syllasounds))
                 (apply 'append (cdr syllasounds))))
        (t
         (get-feminine-rhyme (- stress 1) (cdr syllasounds)))))

(defmethod clean-up-string-list-i ((l list))
  (setf l (remove-if #'(lambda (n) (or (equal n '#\") (equal n '#\Space))) l))
  (setf l (coerce l 'string)))

(defmethod clean-up-string-list-ii ((l list))
  (mapcar #'(lambda (arg) (coerce arg 'string))
          (cdr (mapcar 'car (segment (segment l '#\") '(#\Space))))))

(defmethod format-nn-input ((index number) (syllasounds list) (stress number))
  (cond ((null syllasounds) 
         nil)
        ((onep stress) 
         (cons
          (represent-syllable-binary t index (format-ccvcc (car syllasounds)))
          (format-nn-input index (cdr syllasounds) (- stress 1))))
        (t
         (cons
          (represent-syllable-binary nil index (format-ccvcc (car syllasounds)))
          (format-nn-input index (cdr syllasounds) (- stress 1))))))

(setf *empty-c* "") 

(defmethod format-ccvcc ((syllable list))
  (let ((sy-info (syllable-structure syllable)))
    (cond ((equal '(c c v c c) sy-info) syllable)
          ((equal '(c c v c c c) sy-info)
           (list (first syllable) (second syllable) (third syllable)
                 (fifth syllable) (sixth syllable)))
          ((equal '(c v c c c) sy-info)
           (list *empty-c* (first syllable) (second syllable) (fourth syllable)
                 (fifth syllable)))
          ((equal '(v c c c) sy-info)
           (list *empty-c* *empty-c* (first syllable) 
                 (third syllable) (fourth syllable)))
          ((equal '(c c c v c c) sy-info)
           (list (first syllable) (third syllable) (fourth syllable)
                 (fifth syllable) (sixth syllable)))
          ((equal '(c c c v c) sy-info)
           (list (first syllable) (third syllable) (fourth syllable)
                 (fifth syllable) *empty-c*))
          ((equal '(c c c v) sy-info)
           (list (first syllable) (third syllable) (fourth syllable)
                 *empty-c* *empty-c*))
          ((equal '(c c v c) sy-info) 
           (append syllable (list *empty-c*)))
          ((equal '(c v c c) sy-info) 
           (append (list *empty-c*) syllable))
          ((equal '(c v c) sy-info) 
           (append (list *empty-c*) syllable (list *empty-c*)))
          ((equal '(c c v) sy-info) 
           (append syllable (list *empty-c* *empty-c*)))
          ((equal '(v c c) sy-info) 
           (append (list *empty-c* *empty-c*) syllable))
          ((equal '(c v) sy-info) 
           (append (list *empty-c*) syllable (list *empty-c* *empty-c*)))
          ((equal '(v c) sy-info) 
           (append (list *empty-c* *empty-c*) syllable (list *empty-c*)))
          ((equal '(v) sy-info) 
           (append (list *empty-c* *empty-c*) syllable (list *empty-c* *empty-c*))))))
 
(defmethod syllable-structure ((syllable list))
  (cond ((null syllable)
         nil)
        ((vowelp (get-phoneme (car syllable)))
         (cons 'v (syllable-structure (cdr syllable))))
        ((consonantp (get-phoneme (car syllable)))
         (cons 'c (syllable-structure (cdr syllable))))))
        
;; the binary representation of a syllable has a length of 67.
;; this means 13 bits per sound * 5 sounds = a 65 bit syllable 
;; 65 + 1 for stress/unstress = 66 ;; + 11 for index = 77
;; when fully expanded the length of a syllable should be 77

(defmethod represent-syllable-binary ((stress symbol) (index number) (phonemes list))
  (setf phonemes (mapcar 'get-phoneme phonemes))
  (append (base-ten-number-to-binary-list index 11)
          (phoneme-binary (first phonemes))
          (phoneme-binary (second phonemes))
          (phoneme-binary (third phonemes))
          (cond ((null stress) (list 0)) (t (list 1)))
          (phoneme-binary (fourth phonemes))
          (phoneme-binary (fifth phonemes))))

(defmethod format-syl-sounds ((l list))
  (mapcar 'clean-up-string-list-ii (segment l '#\,)))

(defmethod format-sounds ((l list))
  (apply 'append (format-syl-sounds l)))

(defmethod clean-up-num-list ((l list))
  (setf l (remove-if #'(lambda (n) (equal n '#\Space)) l))
  (setf l (mapcar #'(lambda (n) (coerce (list n) 'string)) l))
  (setf l (car (mapcar #'(lambda (n) (cond ((equal n "1") 1) ((equal n "2") 2) ((equal n "3") 3) ((equal n "4") 4) ((equal n "5") 5) ((equal n "6") 6) ((equal n "7") 7) ((equal n "8") 8) ((equal n "9") 9) ((equal n "0") 0))) l))))

(defmethod generate-lexicon () 
  (setf *nr-entries* 0) ; reset to zero for indexing purposes
  (make-phonemicon) 
  (setf *lex* (genlex "word-list.txt")))

