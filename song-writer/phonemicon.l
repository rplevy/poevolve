;; phonemicon
;; class definitions

(defclass phoneme ()
  ((character :accessor phoneme-character :initarg :character)
   (rhymes :accessor phoneme-rhymes :initarg :rhymes)
   (example :accessor phoneme-example :initarg :example)
   (binary :accessor phoneme-binary :initarg :binary)))

(defclass vowel (phoneme)
  ((height :accessor vowel-height :initarg :height)
   (depth :accessor vowel-depth :initarg :depth)))

(defclass consonant (phoneme)
  ((point :accessor consonant-point :initarg :point)
   (manner :accessor consonant-manner :initarg :manner)
   (voicing :accessor consonant-voicing :initarg :voicing)))

(defclass phonemicon ()
  ((consonants :accessor phonemicon-consonants :initarg :consonants)
   (vowels :accessor phonemicon-vowels :initarg :vowels)))

(defmethod phonemicon-phonemes ((p phonemicon))
  (append (phonemicon-consonants p) (phonemicon-vowels p)))

; a note on binary representations:
; the phonemes are given binary representations to make them
; accessible to the neural network.  
;
; the first bit represents the parameter of consonant/vowel 
; the second bit represents the parameter of voiced/voiceless
; the third through fifth bits represent the seven possible places of articulation
; the sixth through eighth bits represent the seven possible manners of articulation
; the ninth tenth and elevent represent the five possible heights
; the eleventh and twelfth bits represent the three possible depths

(defmethod make-phonemicon ()
  (setf *ph* 
	(make-instance 'phonemicon
		       :consonants (list
				    (make-instance 'consonant 
						   :character "p"
						   :example 'pear
						   :rhymes '("p" "b")
						   :point 'bilabial
						   :manner 'stop
						   :voicing 'voiceless
                           :binary '(1 0 0 0 1 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "b"
						   :example 'bend
						   :rhymes '("b" "p")
						   :point 'bilabial
						   :manner 'stop
						   :voicing 'voiced
                           :binary '(1 1 0 0 1 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "k"
						   :example 'cat
						   :rhymes '("k" "g")
						   :point 'velar
						   :manner 'stop
						   :voicing 'voiceless
                           :binary '(1 0 1 1 1 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "g" 
						   :example 'glib
						   :rhymes '("g" "k")
						   :point 'velar
						   :manner 'stop
						   :voicing 'voiced
                           :binary '(1 1 1 1 1 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "t"
						   :example 'talk
						   :rhymes '("t" "d")
						   :point 'alveolar
						   :manner 'stop
						   :voicing 'voiceless
                           :binary '(1 0 1 0 0 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "d"
						   :example 'dab
						   :rhymes '("d" "t")
						   :point 'alveolar
						   :manner 'stop
						   :voicing 'voiced
                           :binary '(1 1 1 0 0 0 0 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "h"
						   :example 'hue
                           :rhymes '()
						   :point 'glottal
						   :manner 'fricative
						   :voicing 'voiceless
                           :binary '(1 0 0 0 0 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "s"
						   :example 'star
						   :rhymes '("s" "sh" "zh" "z")
						   :point 'alveolar
						   :manner 'fricative
						   :voicing 'voiceless
                           :binary '(1 0 1 0 0 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "z"
						   :example 'zap
						   :rhymes '("z" "s" "zh")
						   :point 'alveolar
						   :manner 'fricative
						   :voicing 'voiced
                           :binary '(1 1 1 0 0 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "sh"
						   :example 'shard
						   :rhymes '("sh" "zh" "s" "ch" "j")
						   :point 'palatal
						   :manner 'fricative
						   :voicing 'voicless
                           :binary '(1 0 1 0 1 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "zh"
						   :example 'azure
						   :rhymes '("zh" "sh" "ch" "j" "z")
						   :point 'palatal 
						   :manner 'fricative
						   :voicing 'voiced
                           :binary '(1 1 1 0 1 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "th"
						   :example 'thicket
						   :rhymes '("th" "thh" "t")
						   :point 'interdental
						   :manner 'fricative
						   :voicing 'voicless
                           :binary '(1 0 0 1 1 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "thh"
						   :example 'though
						   :rhymes '("thh" "d" "th" "t" "v")
						   :point 'interdental
						   :manner 'fricative
						   :voicing 'voiced
                           :binary '(1 1 0 1 1 0 1 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "f"
						   :example 'face
						   :rhymes '("f" "v" "th")
						   :point 'labiodental
						   :manner 'fricative
						   :voicing 'voiceless
                           :binary '(1 0 0 1 0 0 1 0 0 0 0 0 0))

				    (make-instance 'consonant 
						   :character "v"
						   :example 'vine
						   :rhymes '("v" "f" "thh")
						   :point 'labiodental
						   :manner 'fricative
						   :voicing 'voiced
                           :binary '(1 1 0 1 0 0 1 0 0 0 0 0 0))

				    (make-instance 'consonant 
						   :character "ch"
						   :example 'chase
						   :rhymes '("ch" "j" "zh" "sh")
						   :point 'palatal
						   :manner 'affricate
						   :voicing 'voiceless
                           :binary '(1 0 1 0 1 0 1 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "j"
						   :example 'jazz
						   :rhymes '("j" "ch" "zh" "sh")
						   :point 'palatal
						   :manner 'affricate
						   :voicing 'voiced
                           :binary '(1 1 1 0 1 0 1 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "m"
						   :example 'moan
						   :rhymes '("m" "n") 
						   :point 'bilabial
						   :manner 'nasal
                           :binary '(1 1 0 0 1 1 0 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "n"
						   :example 'nimble
						   :rhymes '("n" "m" "ng")
						   :point 'alveolar
						   :manner 'nasal
                           :binary '(1 1 1 0 0 1 0 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "ng"
						   :example 'ankle
						   :rhymes '("ng" "n") 
						   :point 'velar 
						   :manner 'nasal
                           :binary '(1 1 1 1 1 1 0 0 0 0 0 0 0))

				    (make-instance 'consonant 
						   :character "l"
						   :example 'learn
						   :rhymes '("l" "r")
						   :point 'alveolar
						   :manner 'liquid
                           :binary '(1 1 1 0 0 1 0 1 0 0 0 0 0))
				    				    
				    (make-instance 'consonant 
						   :character "r"
						   :example 'wrist
						   :rhymes '("r" "l")
						   :point 'palatal
						   :manner 'liquid
                           :binary '(1 1 1 0 0 1 1 1 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "y"
						   :example 'yip
                           :rhymes nil
						   :point 'palatal
						   :manner 'glide
                           :binary '(1 1 1 0 1 0 0 0 0 0 0 0 0))
				    
				    (make-instance 'consonant 
						   :character "w" 
						   :example 'with
                           :rhymes nil
						   :point 'velar
						   :manner 'glide
                           :binary '(1 1 0 0 1 0 0 0 0 0 0 0 0)))
		       
		       :vowels (list 
				(make-instance 'vowel
					       :character "ee"
					       :rhymes '("ee" "ih") 
					       :example 'eat
					       :height 'high
					       :depth 'front
                           :binary '(0 0 0 0 0 0 0 0 0 0 1 0 1))
				
				(make-instance 'vowel
					       :character "ih"
					       :rhymes '("ih" "ee")
					       :example 'if
					       :height 'low-high
					       :depth 'front
                           :binary '(0 0 0 0 0 0 0 0 0 1 0 0 1))
				
				(make-instance 'vowel
					       :character "uw"
					       :rhymes '("uw" "oo")
					       :example 'foo
					       :height 'high
					       :depth 'back
                           :binary '(0 0 0 0 0 0 0 0 0 0 1 1 1))
				
				(make-instance 'vowel
					       :character "oo"
					       :rhymes '("oo" "uw")
					       :example 'wood
					       :height 'low-high
					       :depth 'back
                           :binary '(0 0 0 0 0 0 0 0 0 1 0 1 1))
				
				(make-instance 'vowel
					       :character "ay"
					       :rhymes '("ay" "eh")
					       :example 'ape
					       :height 'mid
					       :depth 'front
                           :binary '(0 0 0 0 0 0 0 0 0 1 1 0 1))
				
				(make-instance 'vowel
					       :character "eh"
					       :rhymes '("eh" "ay")
					       :example 'egg
					       :height 'low-mid
					       :depth 'front
                           :binary '(0 0 0 0 0 0 0 0 1 0 0 0 1))
				
				(make-instance 'vowel
					       :character "uh"
					       :rhymes nil
					       :example 'ugly
					       :height 'mid
					       :depth 'central
                           :binary '(0 0 0 0 0 0 0 0 1 0 0 1 0))
				
				(make-instance 'vowel
					       :character "oh"
					       :rhymes '("oh" "au")
					       :example 'only
					       :height 'mid
					       :depth 'back
                           :binary '(0 0 0 0 0 0 0 0 0 1 1 1 1))
				
				(make-instance 'vowel
					       :character "au"
					       :rhymes '("au" "oh" "ah" "uh")
					       :example 'awning
					       :height 'low-mid
					       :depth 'back
                           :binary '(0 0 0 0 0 0 0 0 1 0 0 1 1))
				
				(make-instance 'vowel
					       :character "ae"
					       :rhymes '("ae")
					       :example nil
					       :height 'low
					       :depth 'front
                           :binary '(0 0 0 0 0 0 0 0 1 0 1 0 1))

				(make-instance 'vowel
					       :character '"ah"
					       :rhymes '("au")
					       :example 'oblong
					       :height 'low
					       :depth 'back
                           :binary '(0 0 0 0 0 0 0 0 1 0 1 1 1))

				(make-instance 'vowel
					       :character "ai"
					       :rhymes '("ay")
					       :example nil
					       :height 'diphthong
					       :depth 'diphthong
                           :binary '(0 0 0 0 0 0 0 0 1 1 1 0 0))

				(make-instance 'vowel
					       :character "oy"
					       :rhymes '("au" "oh")
					       :example nil
					       :height 'diphthong
					       :depth 'diphthong
                           :binary '(0 0 0 0 0 0 0 0 1 1 1 0 1))

				(make-instance 'vowel
					       :character "ao"
					       :rhymes '("au" "oh")
					       :example nil
					       :height 'diphthong
					       :depth 'diphthong
                           :binary '(0 0 0 0 0 0 0 0 1 1 1 1 0))

                (make-instance 'consonant 
                               :character "" 
                               :example nil 
                               :rhymes nil 
                               :point nil
                               :manner nil 
                               :voicing nil 
                               :binary '(1 0 0 0 0 0 0 0 0 0 0 0 0))))))
  
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

(defmethod vowelp ((p phoneme)) (typep p 'vowel))

(defmethod vowelp ((s symbol)) nil)
(defmethod consonantp ((s symbol)) nil)

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
