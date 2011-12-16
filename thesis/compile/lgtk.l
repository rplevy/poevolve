;; THE LIMERICK GENERATION TOOLKIT ;;
;; (LGTK)                          ;;
;; COPYRIGHT ROBERT LEVY 2000      ;;

(load "utilities.l")

(in-package "PCL")

;; ENTRY OBJECT CLASS              ;;

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

;; LEXICON OBJECT CLASS            ;;

(defclass lexicon ()
  ((length :accessor lexicon-length :initarg :length)
   (entries :accessor lexicon-entries :initarg :entries)))

;; constructor for lexicon object
(defmethod make-lexicon ((file-name string) &aux entries)
  (make-instance 'lexicon
		 :entries (setf entries (load-entries file-name))
		 :length (length entries)))

(defmethod load-entries ((file-name string))
  (let ((s (open file-name)))
    (values ; return stuff before closing file
     (evaluate-all (cdr (read s))) 
     (close s))))

;; PHONEME OBJECT CLASSES          ;;

(defclass phoneme ()
  ((character :accessor phoneme-character :initarg :character)
   (example :accessor phoneme-example :initarg :example)
   (binary :accessor phoneme-binary :initarg :binary)))

(defclass vowel (phoneme)
  ((height :accessor vowel-height :initarg :height)
   (depth :accessor vowel-depth :initarg :depth)))

(defclass consonant (phoneme)
  ((point :accessor consonant-point :initarg :point)
   (manner :accessor consonant-manner :initarg :manner)
   (voicing :accessor consonant-voicing :initarg :voicing)))

;; PHONEMICON OBJECT CLASS         ;;

(defclass phonemicon ()
  ((consonants :accessor phonemicon-consonants :initarg :consonants)
   (vowels :accessor phonemicon-vowels :initarg :vowels)))

;; LIMERICK POEM OBJECT CLASS      ;;

(defclass limerick-poem ()
  ((lines :accessor limerick-poem-lines :initarg :lines)
   (minilex :accessor limerick-poem-minilex :initarg :minilex)
   (rating :accessor limerick-poem-rating :initform 0)))   

;; ENTRY METHODS                   ;;

;;---------------------------------;;
;; find entry matching word

(defmethod seek-entry ((word string) (entries list))
  (remove-if #'(lambda (arg) 
                 (not (equal (string-downcase word) 
                             (string-downcase (entry-word arg))))) 
             entries))

;;---------------------------------;;
;; get only words from entries

(defmethod entries-to-word-list ((entries list))
  (cond ((null entries)
         nil)
        (t (cons (entry-word (car entries)) 
                 (entries-to-word-list (cdr entries)))))) 

;;---------------------------------;;
;; how many syllables in a list of entries

(defmethod total-syllables ((entries list))
  (cond ((null entries)
         0)
        (t 
         (+ (length (entry-syllables (car entries)))
            (total-syllables (cdr entries))))))

;;---------------------------------;;
;; is the given object an entry?

(defun entryp (s)
  (typep s 'entry))

;; LEXICON METHODS                 ;;

;;---------------------------------;;
;; generate lexicon from word-list

(setf *nr-entries* 0)

(defmethod generate-lexicon () 
  (setf *nr-entries* 0) ; reset to zero for indexing purposes
  (make-phonemicon) 
  (setf *lex* (genlex "word-list.txt")))

(defmethod genlex ((file string) &aux lex lines)
  (write-string-terpri "Generating lexicon, please hold...")
  (setf lex (make-instance 'lexicon))
  (setf lines (make-list-of-lines file))
  (setf (lexicon-entries lex) (mapcar #'make-entry-from-data lines))
  (setf (lexicon-length lex) (length (lexicon-entries lex)))
  lex) 

(defmethod make-entry-from-data ((entry-data string))
  (apply #'make-entry (segment (coerce entry-data 'list) '#\-)))

(defmethod make-entry ((word list) (syllables list) 
                       (sounds list)  (stress list) 
                       (class list) &aux syllasounds)
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
           (append (list *empty-c* *empty-c*) syllable 
                   (list *empty-c* *empty-c*))))))

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

(defmethod represent-syllable-binary ((stress symbol) (index number) 
                                      (phonemes list))
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
  (setf l (car (mapcar #'(lambda (n) (cond ((equal n "1") 1) 
                                           ((equal n "2") 2) 
                                           ((equal n "3") 3) 
                                           ((equal n "4") 4) 
                                           ((equal n "5") 5) 
                                           ((equal n "6") 6) 
                                           ((equal n "7") 7) 
                                           ((equal n "8") 8) 
                                           ((equal n "9") 9) 
                                           ((equal n "0") 0))) l))))

;;---------------------------------;;
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

; a note on binary representations:
; the phonemes are given binary representations to make themxo
; accessible to the neural network.  
;
; the first bit represents the parameter of consonant/vowel 
; the second bit represents the parameter of voiced/voiceless
; the third through fifth bits represent the seven 
;  possible places of articulation
; the sixth through eighth bits represent the seven possible 
;  manners of articulation
; the ninth tenth and eleventh represent the five possible heights
; the twelfth and thirteentth bits represent the three possible depths

;; PHONEMICON METHODS              ;;

;;---------------------------------;;
;; construct a phonemicon object

(defmethod make-phonemicon ()
  (setf *ph* 
	(make-instance 'phonemicon
		       :consonants (list
				    (make-instance 'consonant 
						   :character "p"
						   :example 'pear
						   :point 'bilabial
						   :manner 'stop
						   :voicing 'voiceless
						   :binary '(1 0 0 0 1 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "b"
						   :example 'bend
						   :point 'bilabial
						   :manner 'stop
						   :voicing 'voiced
						   :binary '(1 1 0 0 1 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "k"
						   :example 'cat
						   :point 'velar
						   :manner 'stop
						   :voicing 'voiceless
						   :binary '(1 0 1 1 1 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "g" 
						   :example 'glib
						   :point 'velar
						   :manner 'stop
						   :voicing 'voiced
						   :binary '(1 1 1 1 1 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "t"
						   :example 'talk
						   :point 'alveolar
						   :manner 'stop
						   :voicing 'voiceless
						   :binary '(1 0 1 0 0 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "d"
						   :example 'dab
						   :point 'alveolar
						   :manner 'stop
						   :voicing 'voiced
						   :binary '(1 1 1 0 0 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "h"
						   :example 'hue
						   :point 'glottal
						   :manner 'fricative
						   :voicing 'voiceless
						   :binary '(1 0 0 0 0 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "s"
						   :example 'star
						   :point 'alveolar
						   :manner 'fricative
						   :voicing 'voiceless
						   :binary '(1 0 1 0 0 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "z"
						   :example 'zap
						   :point 'alveolar
						   :manner 'fricative
						   :voicing 'voiced
						   :binary '(1 1 1 0 0 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "sh"
						   :example 'shard
						   :point 'palatal
						   :manner 'fricative
						   :voicing 'voicless
						   :binary '(1 0 1 0 1 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "zh"
						   :example 'azure
						   :point 'palatal 
						   :manner 'fricative
						   :voicing 'voiced
						   :binary '(1 1 1 0 1 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "th"
						   :example 'thicket
						   :point 'interdental
						   :manner 'fricative
						   :voicing 'voicless
						   :binary '(1 0 0 1 1 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "thh"
						   :example 'though
						   :point 'interdental
						   :manner 'fricative
						   :voicing 'voiced
						   :binary '(1 1 0 1 1 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "f"
						   :example 'face
						   :point 'labiodental
						   :manner 'fricative
						   :voicing 'voiceless
						   :binary '(1 0 0 1 0 0 1 0 0 0 0 0 0))

				    (make-instance 'consonant 
						   :character "v"
						   :example 'vine
						   :point 'labiodental
						   :manner 'fricative
						   :voicing 'voiced
						   :binary '(1 1 0 1 0 0 1 0 0 0 0 0 0))

				    (make-instance 'consonant 
						   :character "ch"
						   :example 'chase
						   :point 'palatal
						   :manner 'affricate
						   :voicing 'voiceless
						   :binary '(1 0 1 0 1 0 1 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "j"
						   :example 'jazz
						   :point 'palatal
						   :manner 'affricate
						   :voicing 'voiced
						   :binary '(1 1 1 0 1 0 1 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "m"
						   :example 'moan
						   :point 'bilabial
						   :manner 'nasal
						   :binary '(1 1 0 0 1 1 0 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "n"
						   :example 'nimble
						   :point 'alveolar
						   :manner 'nasal
						   :binary '(1 1 1 0 0 1 0 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "ng"
						   :example 'ankle
						   :point 'velar 
						   :manner 'nasal
						   :binary '(1 1 1 1 1 1 0 0 0 0 0 0 0))

				    (make-instance 'consonant 
						   :character "l"
						   :example 'learn
						   :point 'alveolar
						   :manner 'liquid
						   :binary '(1 1 1 0 0 1 0 1 0 0 0 0 0))
				    				    
				    (make-instance 'consonant 
						   :character "r"
						   :example 'wrist
						   :point 'palatal
						   :manner 'liquid
						   :binary '(1 1 1 0 0 1 1 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "y"
						   :example 'yip
						   :point 'palatal
						   :manner 'glide
						   :binary '(1 1 1 0 1 0 0 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "w" 
						   :example 'with
						   :point 'velar
						   :manner 'glide
						   :binary '(1 1 0 0 1 0 0 0 0 0 0 0 0)))
		       
		       :vowels (list 
				(make-instance 'vowel
					       :character "ee"
					       :example 'eat
					       :height 'high
					       :depth 'front
					       :binary '(0 0 0 0 0 0 0 0 0 0 1 0 1))
				
				(make-instance 'vowel
					       :character "ih"
					       :example 'if
					       :height 'low-high
					       :depth 'front
					       :binary '(0 0 0 0 0 0 0 0 0 1 0 0 1))
				
				(make-instance 'vowel
					       :character "uw"
					       :example 'foo
					       :height 'high
					       :depth 'back
					       :binary '(0 0 0 0 0 0 0 0 0 0 1 1 1))
				
				(make-instance 'vowel
					       :character "oo"
					       :example 'wood
					       :height 'low-high
					       :depth 'back
					       :binary '(0 0 0 0 0 0 0 0 0 1 0 1 1))
				
				(make-instance 'vowel
					       :character "ay"
					       :example 'ape
					       :height 'mid
					       :depth 'front
					       :binary '(0 0 0 0 0 0 0 0 0 1 1 0 1))
				
				(make-instance 'vowel
					       :character "eh"
					       :example 'egg
					       :height 'low-mid
					       :depth 'front
					       :binary '(0 0 0 0 0 0 0 0 1 0 0 0 1))
				
				(make-instance 'vowel
					       :character "uh"
					       :example 'ugly
					       :height 'mid
					       :depth 'central
					       :binary '(0 0 0 0 0 0 0 0 1 0 0 1 0))
				
				(make-instance 'vowel
					       :character "oh"
					       :example 'only
					       :height 'mid
					       :depth 'back
					       :binary '(0 0 0 0 0 0 0 0 0 1 1 1 1))
				
				(make-instance 'vowel
					       :character "au"
					       :example 'awning
					       :height 'low-mid
					       :depth 'back
					       :binary '(0 0 0 0 0 0 0 0 1 0 0 1 1))
				
				(make-instance 'vowel
					       :character "ae"
					       :example nil
					       :height 'low
					       :depth 'front
					       :binary '(0 0 0 0 0 0 0 0 1 0 1 0 1))

				(make-instance 'vowel
					       :character '"ah"
					       :example 'oblong
					       :height 'low
					       :depth 'back
					       :binary '(0 0 0 0 0 0 0 0 1 0 1 1 1))

				(make-instance 'vowel
					       :character "ai"
					       :example nil
					       :height 'diphthong
					       :depth 'diphthong
					       :binary '(0 0 0 0 0 0 0 0 1 1 1 0 0))

				(make-instance 'vowel
					       :character "oy"
					       :example nil
					       :height 'diphthong
					       :depth 'diphthong
					       :binary '(0 0 0 0 0 0 0 0 1 1 1 0 1))

				(make-instance 'vowel
					       :character "ao"
					       :example nil
					       :height 'diphthong
					       :depth 'diphthong
					       :binary '(0 0 0 0 0 0 0 0 1 1 1 1 0))

				(make-instance 'consonant 
					       :character "" 
					       :example nil 
					       :point nil
					       :manner nil 
					       :voicing nil 
					       :binary '(1 0 0 0 0 0 0 0 0 0 0 0 0))))))
  
(defmethod phonemicon-phonemes ((p phonemicon))
  (append (phonemicon-consonants p) (phonemicon-vowels p)))

(defmethod get-phonemes ((phonemes list))
  (mapcar 'get-phoneme phonemes))

(defmethod get-phoneme ((syntax-erroneous list))
  (display (get-phoneme (car syntax-erroneous)))
  (get-phoneme (car syntax-erroneous)))

(defmethod get-phoneme ((character string))
  (do-get-phoneme character (phonemicon-phonemes *ph*)))
(defmethod do-get-phoneme ((character string) (phonemes list))
  (cond ((null phonemes) 
	 nil)
	((equal character (phoneme-character (car phonemes)))
	 (car phonemes))
	(t 
	 (do-get-phoneme character (cdr phonemes)))))
	    
(defmethod phonemes-to-characters ((phonemes list))
  (cond ((null phonemes)
         nil)
        (t
         (cons (phoneme-character (car phonemes))
               (phonemes-to-characters (cdr phonemes))))))

(defmethod consonantp ((p phoneme)) (typep p 'consonant))

(defmethod consonantp ((s symbol)) nil)

(defmethod vowelp ((p phoneme)) (typep p 'vowel))

(defmethod vowelp ((s symbol)) nil)

;; LIMERICK POEM METHODS           ;;

;;---------------------------------;;
;; make an exact copy of a limerick

(defmethod clone ((l limerick-poem))
  (make-instance 'limerick-poem
                 :lines (limerick-poem-lines l)
                 :minilex (limerick-poem-minilex l)))

;;---------------------------------;;
;; list entries of all words in poem

(defmethod limerick-poem-entries ((lim limerick-poem))
  (apply 'append (limerick-poem-lines lim)))

;;---------------------------------;;
;; generate a random limerick poem
;; optionally save to disk

(defmethod genlimerator (&optional minilex file-out)
  (let ((rhyme-a (get-three-rhyme))
        (rhyme-b (get-two-rhyme))
        (mini-lex (or minilex 
                      (pick *working-lexicon-size* 
                            (lexicon-entries *lex*))))
        (limerick nil))
    (setf l (make-instance 'limerick-poem 
                           :lines (list
                                   (make-line-a mini-lex (first rhyme-a))
                                   (make-line-a mini-lex (second rhyme-a))
                                   (make-line-b mini-lex (first rhyme-b))
                                   (make-line-b mini-lex (second rhyme-b))
                                   (make-line-a mini-lex (third rhyme-a)))
                           :minilex mini-lex))
    (and file-out (write-limerick-to-disk l file-out)) l))

(defmethod write-limerick-to-disk ((l limerick-poem) (f string))
  (let ((str (open f :direction :output))     
        (poem-text (limerick-poem-text l)))
    (write-string (first poem-text) str) (terpri str)
    (write-string (second poem-text) str) (terpri str)
    (write-string (third poem-text) str) (terpri str)
    (write-string (fourth poem-text) str) (terpri str)
    (write-string (fifth poem-text) str) (terpri str)
    (terpri str) 
    (close str)))

;;---------------------------------;;
;; find three words that rhyme together

(defmethod get-three-rhyme () 
  (let (rhyme) 
    (setf rhyme 
          (pick 3 (rhyme-filter (any-one *rhymes*) 
                                (lexicon-entries *lex*))))
    (cond ((< (length rhyme) 3)
           (append rhyme (list (car rhyme))))
          (t rhyme))))

;;---------------------------------;;
;; find two words that rhyme together

(defmethod get-two-rhyme () 
  (let (rhyme) 
    (setf rhyme 
          (pick 2 (rhyme-filter (any-one *rhymes*) 
                                (lexicon-entries *lex*))))))

;;---------------------------------;;
;; make a limerick line with A scan

(defmethod make-line-a ((mini-lex list) (e entry))
  (make-poem-line mini-lex (insert-new-entry e (list 0 1 0 0 1 0 0 1))))

;;---------------------------------;;
;; make a limerick line with B scan

(defmethod make-line-b ((mini-lex list) (e entry))
  (make-poem-line mini-lex (insert-new-entry e (list 0 1 0 0 1))))

;;---------------------------------;;
;; make a limerick line with
;; specified rhythm template

(defmethod make-poem-line ((mini-lex list) (line list))
  (let   ; get list of each type 
      ((dots (rhythm-filter '(1) mini-lex))
       (inverse-dots (rhyme-filter '(0) mini-lex))
       (iambs (rhythm-filter '(0 1) mini-lex))
       (reverse-iambs (rhythm-filter '(1 0) mini-lex))
       (anapests (rhythm-filter '(0 0 1) mini-lex))
       (reverse-anapests (rhythm-filter '(1 0 0) mini-lex))
       (hills (rhythm-filter '(0 1 0) mini-lex)))
    (do-make-poem-line line dots inverse-dots  
                           iambs reverse-iambs hills
                           anapests reverse-anapests)))

(defmethod do-make-poem-line ((line list) (dots list) (inverse-dots list) 
                                  (iambs list) (reverse-iambs list) (hills list)
                                  (anapests list) (reverse-anapests list))
  (do ((o 0 (+ o 1)))
      ((pure-line-p line))
    (setf line (insert-new-entry 
                (choose-entry (list dots inverse-dots iambs 
                                    reverse-iambs hills
                                    anapests reverse-anapests) (random 6))
                line)))
  line)
      
(defmethod pure-line-p ((line list))
  (null (remove-if 'entryp line)))

(defmethod choose-entry ((types list) (preferred-type number))
  (or (do-choose-entry types preferred-type)
      (any-one (rhythm-filter '(1) (lexicon-entries *lex*)))))

(defmethod do-choose-entry ((types list) (preferred-type number))
  (let ((new nil))
    (do ((o 0 (+ o 1)))
        ((or new (null types)))   
      (if (null preferred-type) (setf preferred-type (random 6)))
      (setf new (any-one (nth preferred-type types)))
      (cond ((null new)
             (setf types (remove-nth preferred-type types))
             (setf preferred-type nil))))
    new))

(defmethod insert-new-entry ((new entry) (line list))
  (setf line (reverse line))
  (let ((rrythm (reverse (entry-rhythm new))))
    (cond ((and (numberp (car line)) (onep (car line)))
           (reverse (cons new (cdr-n (entry-stress new) line))))
          (t
           (reverse (do-insert-new-entry rrythm new line))))))

(defmethod do-insert-new-entry ((rrythm list) (new entry) (line list) &aux match)
  (cond ((null line) 
         nil)
        ((> (length rrythm) (length line))
         line)
        ((or (equal rrythm (setf match (car-n (length rrythm) line)))
             (and (equal rrythm '(1)) (equal match '(0))))
         (cons new (cdr-n (length rrythm) line)))
        (t
         (cons (car line) (do-insert-new-entry rrythm new (cdr line))))))

;;---------------------------------;;
;; find all words in list of entries
;; that match a specific rhythm

(defmethod rhythm-filter ((template list) (entries list))
  (remove-if #'(lambda (entry) 
                 (not (equal (entry-rhythm entry) template))) entries))

;;---------------------------------;;
;; find all words in list of entries
;; that match a specific rhyme

(defmethod rhyme-filter ((sounds list) (entries list))
  (remove-if #'(lambda (entry) 
                 (not (equal (entry-rhyme entry) sounds))) entries))

;;---------------------------------;;
;; resources for converting limericks 
;; into binary representations

(defmethod limerick-nn-input ((l limerick-poem))
  (entries-nn-input (limerick-poem-entries l)))

(defmethod entries-nn-input ((entries list))
  (apply 'append (mapcar 'entry-nn-input entries)))

;;---------------------------------;;
;; a function for reconstruction of 
;; poem objects out of plain texts

(defmethod reconstruct-limericks ((questionaire string))
  (setf limericks
        (mapcar 
         #'(lambda (arg) 
             (mapcar 
              #'(lambda (aarg) 
                  (coerce aarg 'list)) 
              arg))
         (segment (make-list-of-lines questionaire) "")))
  (setf limericks
        (mapcar
         #'(lambda (arg) 
             (mapcar 
              #'(lambda (aarg) 
                  (mapcar 'process-word
                          (remove-if 'null (segment aarg '#\Space))))
              arg))
         limericks))
  (reverse 
   (mapcar 'reconstruct-limerick
           limericks)))

(defmethod reconstruct-limerick ((lm list))
  (make-instance 
   'limerick-poem
   :lines (reverse (mapcar #'(lambda (arg) (apply 'append arg)) lm))))

(defmethod process-word ((chars list))
  (seek-entry
   (cat (mapcar 'string chars))
   (lexicon-entries *lex*)))

;;---------------------------------;;
;; save a collection of structured 
;; data as plain text limericks 

(defmethod save-population-as-text ((limerick-list list) (fn string))
  (setf *stream* (open fn :direction :output))
  (mapcar 'do-save-limerick-as-text limerick-list)
  (terpri *stream*)
  (close *stream*))

(defmethod do-save-limerick-as-text ((limerick limerick-poem))
  (let ((poem-text (limerick-poem-text limerick)))
    (write-string (first poem-text) *stream*)     (terpri *stream*)
    (write-string (second poem-text) *stream*)    (terpri *stream*)
    (write-string (third poem-text) *stream*)    (terpri *stream*)
    (write-string (fourth poem-text) *stream*)    (terpri *stream*)
    (write-string (fifth poem-text) *stream*)    (terpri *stream*)
    (terpri *stream*)))

;;---------------------------------;;
;; same as above but for individual
;; syllables instead

(defmethod save-syllables-as-text ((syllables-list list) (fn string))
  (setf *stream* (open fn :direction :output))
  (mapcar #'(lambda (syllable) 
              (write-string (cat syllable) *stream*)
              (terpri *stream*)) 
          syllables-list)
  (terpri *stream*)
  (close *stream*))

(defmethod all-syllables-of-limericks ((limericks list))
  (apply 'append (mapcar 'limerick-poem-syllables limericks)))

;;---------------------------------;;
;; use an external application
;; ("festival") to speak limericks 

(defmethod speak ((l limerick-poem))
  (let ((lines (mapcar 'line-to-text (limerick-poem-lines l))))
    (dotimes (i 5)
      (system (concatenate 'string "echo " 
                           (nth (1+ i) lines) 
                           " | festival -b --tts")))))


;; THE (OVERLOADED) DISPLAY METHODS;;

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

(defmethod display ((l lexicon))
  (write-string "Size: ") (princ (lexicon-length l)) (terpri)
  (write-string "Entries: ") (terpri)
  (mapcar 'display (lexicon-entries l))
  (write-string "that's all folks!") t)

(defmethod display ((v phoneme))
  (write-string "character: ")
  (princ (phoneme-character v))
  (write-string " rhymes: ")
  (princ (phoneme-rhymes v))
  (write-string " example: ")
  (princ (phoneme-example v))
  (terpri))

(defmethod display ((ph phonemicon))
  (mapcar #'display (phonemicon-phonemes ph))
  t)

(defmethod display ((objects list))
  (mapcar 'display objects))

(defmethod display ((lm limerick-poem) &aux poem-text)
  (setf poem-text (limerick-poem-text lm))
  (write-string-terpri (first poem-text))
  (write-string-terpri (second poem-text))
  (write-string-terpri (third poem-text))
  (write-string-terpri (fourth poem-text))
  (write-string-terpri (fifth poem-text))
  (terpri)
  lm)

(defmethod limerick-poem-text ((lm limerick-poem))
  (mapcar 'line-to-text (limerick-poem-lines lm)))

(defmethod line-to-text ((line list))
  (string-list-to-spaced-string (mapcar 'entry-word line)))

(defmethod limerick-poem-syllables ((lm limerick-poem))
  (apply 'append 
         (mapcar 'entry-syllasounds (limerick-poem-entries lm))))

(defmethod limerick-poem-syllables-count ((lm limerick-poem))
  (length (limerick-poem-syllables lm)))


(defmethod display-in-line ((objects list))
  (cond ((null objects) 
         nil)
        (t
         (cons (display (car objects))
               (display-in-line (cdr objects))))))

(defmethod display-all-syllables-of-limericks ((limericks list))
  (setf limericks (all-syllables-of-limericks limericks))
  (dotimes (i (length limericks))
    (mapcar 'princ (car limericks))    (terpri)
    (pop limericks)))

;; TOOLS FOR MUTATION

(defmethod line-a-modifier-strong ((minilex list) (line list))
  (make-line-a minilex (final line)))

(defmethod line-b-modifier-strong ((minilex list) (line list))
  (make-line-b minilex (final line)))

(defmethod line-modifier-weak ((minilex list) (line list))
  (apply #'make-poem-line 
         (list minilex 
               (apply 'append (retemplate-randomly (mapcar 'list line))))))

; reduce up to three words back to their rhythm template
; so that they may be run through the line-maker again
(defmethod retemplate-randomly ((line list))
  (cond ((> (length line) 4)
         (do-retemp-rand (random-lo-hi 1 3) line))
        (t
         (do-retemp-rand (random-lo-hi 1 2) line))))

(defmethod do-retemp-rand ((n number) (line list))
  (cond ((zerop n) 
         line)
        (t 
         (do-retemp-rand 
          (- n 1)
          (retemplate-nth 
           (random-lo-hi 1 (- (length line) 1)) 
           line)))))

(defmethod retemplate-nth ((index number) (line list))
  (cond ((onep index) 
         (if (numberp (car (car line)))
             line
           (cons (retemplate (car (car line)))
                 (cdr line))))
        (t
         (cons 
          (car line)
          (retemplate-nth (- index 1) (cdr line))))))

(defmethod retemplate ((e entry))
  (entry-rhythm e))


;; merge two lines together

(defun merge-lines (l1st l2st)
  (do-merge-lines l1st l2st 
                  (map-except-last-car 'retemplate l1st)
                  (map-except-last-car 'retemplate l2st)))

(defun do-merge-lines (l1st l2st tl1st tl2st)
  (cond
   ((atom (car tl1st)) l1st) 
   ((atom (car tl2st)) l1st) 
   ((equal
     (car tl1st) (car tl2st))
    (cons (car l2st) 
          (do-merge-lines
           (cdr l1st) (cdr l2st)
           (cdr tl1st) (cdr tl2st))))
   (t 
    (cons (car l1st)
          (do-merge-lines
           (cdr l1st) (cdr l2st)
           (cdr tl1st) (cdr tl2st))))))

(proclaim '(inline retemplate))

;; THE THINGS LGTK DOES AUTOMATICALLY AFTER LOADING: 

;; make a phonemicon, 
;; make a lexicon,
;; set the working mini-lex size to 100,
;; randomize seed using the time,
;; make list of all the rhyme endings 
;; that more than one word have,
;; 

(make-phonemicon)

;(load "genlex.l")         ;; uncomment if changes to word-list
;(generate-lexicon)        ;; have been made in the recent past

(setf *lex* (make-lexicon "lex.l"))

(setf *working-lexicon-size* 100)

;; set random number seed uniquely using the universal time
(setf *random-state* (make-random-state t))

; get-list of rhyme sound endings
(setf *rhymes* 
      (set-difference 
       (mapcar 'entry-rhyme (lexicon-entries *lex*))
       (elimenate-redundancy 
        (mapcar 'entry-rhyme (lexicon-entries *lex*)))))

