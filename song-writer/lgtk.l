;; The Limerick Generation Tool Kit 
;; filename: lgtk.l
;; also part of the tool kit are the files entry.l, lexicon.l, and phonemicon.l, as well as genlex.l

(load "utilities.l")

(load "entry.l")
(load "lexicon.l")  
(load "phonemicon.l")
(make-phonemicon)
;(load "genlex.l")   
;(generate-lexicon)
;(load-rhyme-knowledge)
(setf *lex* (make-lexicon "lex.l"))
(setf *working-lexicon-size* 75)

;; set random number seed uniquely using the universal time
(setf *random-state* (make-random-state t))

; get-list of rhyming sounds
(setf *rhymes* 
      (set-difference 
       (mapcar 'entry-rhyme (lexicon-entries *lex*))
       (elimenate-redundancy 
        (mapcar 'entry-rhyme (lexicon-entries *lex*)))))

(defclass limerick-poem ()
  ((lines :accessor limerick-poem-lines :initarg :lines)
   (rating :accessor limerick-poem-rating :initform 0)))
   

; poets represent a collection of  
; varied influences of the operation of
; limerick generation machinery.

(defclass limerick-poet () ())

; a kind of poet who only modifies the working lexicon size
(defclass simple-poet (limerick-poet)
  ((lexicon-size :accessor simple-poet-lexicon-size :initarg :lexicon-size)))

(defmethod display ((simpoet simple-poet))
  (princ (simple-poet-lexicon-size simpoet)) 
  (write-string " , ") t)

(defmethod random-simple-poet ((floor number) (ceiling number))
  (make-instance 'simple-poet :lexicon-size (random-lo-hi floor ceiling))) 

(defmethod limerick-poem-entries ((lim limerick-poem))
  (apply 'append (limerick-poem-lines lim)))

(defmethod genlimerator (&optional poet) 
  (let ((rhyme-a (get-three-rhyme))
        (rhyme-b (get-two-rhyme))
        (mini-lex (or (and poet (pick (simple-poet-lexicon-size poet) 
                                      (lexicon-entries *lex*)))
                      (pick *working-lexicon-size* 
                            (lexicon-entries *lex*)))))
    (make-instance 'limerick-poem 
                   :lines (list
                           (make-line-a mini-lex (first rhyme-a))
                           (make-line-a mini-lex (second rhyme-a))
                           (make-line-b mini-lex (first rhyme-b))
                           (make-line-b mini-lex (second rhyme-b))
                           (make-line-a mini-lex (third rhyme-a))))))

(defmethod get-three-rhyme () 
  (let (rhyme) 
    (setf rhyme 
          (pick 3 (rhyme-filter (any-one *rhymes*) 
                                (lexicon-entries *lex*))))
    (cond ((< (length rhyme) 3)
           (append rhyme (list (car rhyme))))
          (t rhyme))))

(defmethod get-two-rhyme () 
  (let (rhyme) 
    (setf rhyme 
          (pick 2 (rhyme-filter (any-one *rhymes*) 
                                (lexicon-entries *lex*))))))

(defmethod make-line-a ((mini-lex list) (e entry))
  (make-poem-line mini-lex (insert-new-entry e (list 0 1 0 0 1 0 0 1))))

(defmethod make-line-b ((mini-lex list) (e entry))
  (make-poem-line mini-lex (insert-new-entry e (list 0 1 0 0 1))))

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
      

;(defmethod pure-line ((line list))
;  (if (pure-line-p line) line nil))

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
         
(defmethod rhythm-filter ((template list) (entries list))
  (remove-if #'(lambda (entry) (not (equal (entry-rhythm entry) template))) entries))

(defmethod rhyme-filter ((sounds list) (entries list))
  (remove-if #'(lambda (entry) (not (equal (entry-rhyme entry) sounds))) entries))
                            
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

;; resources for converting limericks into binary representations

(defmethod limerick-nn-input ((l limerick-poem))
  (entries-nn-input (limerick-poem-entries l)))

(defmethod entries-nn-input ((entries list))
  (apply 'append (mapcar 'entry-nn-input entries)))


